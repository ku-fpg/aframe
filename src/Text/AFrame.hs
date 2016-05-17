{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, GADTs, InstanceSigs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Text.AFrame where

import Data.Generic.Diff
import Data.Map(Map)
import Data.String
import Data.Text(Text,pack,unpack)
import Data.Maybe (listToMaybe)
import Data.List as L

import Text.XML.Light as X

-- | 'AFrame' describes the contents of an a-frame scene,
--   and is stored as a classical rose tree.

data AFrame       = AFrame Primitive [Attribute] [AFrame]
  deriving (Show, Eq)

newtype Primitive = Primitive Text
  deriving (Show, Eq, Ord, IsString)

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString)
  
newtype Property  = Property Text
  deriving (Show, Eq, Ord, IsString)

type Attribute = (Label,Property)


setAttribute :: Label -> Property -> AFrame -> AFrame
setAttribute lbl prop (AFrame p as af) = AFrame p ((lbl,prop) : [ (l,p) | (l,p) <- as, l /= lbl ]) af

getAttribute :: Label -> AFrame -> Maybe Property
getAttribute lbl (AFrame p as af) = lookup lbl as

resetAttribute :: Label -> AFrame -> AFrame
resetAttribute lbl (AFrame p as af) = AFrame p [ (l,p) | (l,p) <- as, l /= lbl ] af


-- | 'aFrameToElement' converts an 'AFrame' to an (XML) 'Element'. Total.
aFrameToElement :: AFrame -> Element
aFrameToElement (AFrame prim attrs rest) = node (unqual $ unpack prim') (attrs',rest')
  where
    Primitive prim' = prim
    attrs'          = [ Attr (unqual $ unpack a) (unpack p) 
                      | (Label a,Property p) <- attrs 
                      ]
    rest'           = map aFrameToElement rest


-- | 'aFrameToElement' converts a (XML) 'Element' to an 'AFrame'. Total.
-- Strips out any text (which is not used by 'AFrame' anyway.)
elementToAFrame :: Element -> AFrame
elementToAFrame ele = AFrame prim' attrs' content'
  where
    prim'    = Primitive $ pack $ qName $ elName $ ele
    attrs'   = [ (Label $ pack $ qName $ a,Property $ pack $ p)| Attr a p <- elAttribs ele ]
    content' = [ elementToAFrame ele' | Elem ele' <- elContent ele ]


-- | reads an aframe document. This can be enbedded in an XML-style document (such as HTML)
readAFrame :: String -> Maybe AFrame
readAFrame str = do
    element <- parseXMLDoc str
    let aframe  = elementToAFrame element
    findAFrame aframe
  where 
    findAFrame :: AFrame -> Maybe AFrame
    findAFrame a@(AFrame (Primitive "a-scene") _ _) = return a
    findAFrame (AFrame _ _ xs) = listToMaybe
      [ x
      | Just x <- map findAFrame xs
      ]

showAFrame :: AFrame -> String
showAFrame = ppcElement (useShortEmptyTags (\ _ -> False) prettyConfigPP) .  aFrameToElement
    
-- | inject 'AFrame' into an existing (HTML) file. Replaces complete "<a-scene>" element.
injectAFrame :: AFrame -> String -> String
injectAFrame aframe str = findScene str 0 
  where
    openTag  = "<a-scene"
    closeTag = "</a-scene>"

    findScene :: String -> Int -> String
    findScene xs     n | openTag `L.isPrefixOf` xs = insertScene (drop (length openTag) xs) n
    findScene (x:xs) n =
       case x of
         ' '  -> x : findScene xs (n+1)
         _    -> x : findScene xs 0
    findScene [] n = []

    insertScene :: String -> Int -> String
    insertScene xs n = unlines (s : map (spaces ++) (ss ++ [remainingScene xs]))
     where
       (s:ss) = lines $ showAFrame $ aframe
       spaces = take n $ repeat ' '

    -- This will mess up if the closeTag strict appears in the scene.
    remainingScene :: String -> String
    remainingScene xs | closeTag `L.isPrefixOf` xs = drop (length closeTag) xs
    remainingScene (x:xs) = remainingScene xs
    remainingScene []     = []

test = do
  xs <- readFile "samples/helloworld.html" 
  putStr $ xs
  a <- readFile "samples/background.aframe" 
  let Just af = readAFrame a
  putStr $ injectAFrame af xs
  
------
-- Adding gdiff support
------

data AFrameFamily :: * -> * -> * where
 AFrame'     ::              AFrameFamily AFrame      (Cons Primitive 
                                                      (Cons [Attribute] 
                                                      (Cons [AFrame] Nil)))
 ConsAttr'   ::              AFrameFamily [Attribute] (Cons Attribute (Cons [Attribute] Nil))
 NilAttr'    ::              AFrameFamily [Attribute] Nil
 ConsAFrame' ::              AFrameFamily [AFrame] (Cons AFrame (Cons [AFrame] Nil))
 NilAFrame'  ::              AFrameFamily [AFrame] Nil
 Primitive'  :: Primitive -> AFrameFamily Primitive Nil
 Attribute'  :: Attribute -> AFrameFamily Attribute Nil

instance Family AFrameFamily where
  decEq  :: AFrameFamily tx txs -> AFrameFamily ty tys -> Maybe (tx :~: ty, txs :~: tys)
  decEq AFrame'     AFrame'     = Just (Refl, Refl)
  decEq ConsAttr'   ConsAttr'   = Just (Refl, Refl)
  decEq NilAttr'    NilAttr'    = Just (Refl, Refl)
  decEq ConsAFrame' ConsAFrame' = Just (Refl, Refl)
  decEq NilAFrame'  NilAFrame'  = Just (Refl, Refl)

  decEq (Primitive' p1) (Primitive' p2) | p1 == p2 = Just (Refl, Refl)
  decEq (Attribute' a1) (Attribute' a2) | a1 == a2 = Just (Refl, Refl)
  decEq _           _           = Nothing

  fields :: AFrameFamily t ts -> t -> Maybe ts
  fields AFrame'        (AFrame prim attrs fs) 
                           = Just $ CCons prim $ CCons attrs $ CCons fs $ CNil
  fields ConsAttr'      ((lbl,prop):xs) 
                           = Just $ CCons (lbl,prop) $ CCons xs $ CNil
  fields NilAttr'       [] = Just CNil
  fields ConsAFrame'    (x:xs) 
                           = Just $ CCons x $ CCons xs $ CNil
  fields NilAFrame'     [] = Just CNil
  fields (Primitive' _) _  = Just CNil
  fields (Attribute' _) _  = Just CNil
  fields _              _  = Nothing

  apply  :: AFrameFamily t ts -> ts -> t
  apply AFrame'         (CCons prim (CCons attrs (CCons fs CNil)))
                             = AFrame prim attrs fs
  apply ConsAttr'       (CCons (lbl,prop) (CCons xs CNil)) = (lbl,prop) : xs
  apply NilAttr'        CNil = []
  apply ConsAFrame'     (CCons x (CCons xs CNil)) = x : xs
  apply NilAFrame'      CNil = []
  apply (Primitive' p1) CNil = p1
  apply (Attribute' a1) CNil = a1

  string :: AFrameFamily t ts -> String
  string AFrame'         = "AFrame"
  string ConsAttr'       = "ConsAttr"
  string NilAttr'        = "NilAttr"
  string ConsAFrame'     = "ConsAFrame"
  string NilAFrame'      = "NilAFrame"
  string (Primitive' l1) = show l1
  string (Attribute' p1) = show p1


instance Type AFrameFamily AFrame where
    constructors = [Concr AFrame']

instance Type AFrameFamily Primitive where
    constructors = [Abstr Primitive']
    
instance Type AFrameFamily [Attribute] where
    constructors = [Concr ConsAttr',Concr NilAttr']    

instance Type AFrameFamily [AFrame] where
    constructors = [Concr ConsAFrame',Concr NilAFrame']
 
instance Type AFrameFamily Attribute where
    constructors = [Abstr Attribute']


  