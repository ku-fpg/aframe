{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, GADTs, InstanceSigs, TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}

module Text.AFrame where

import Data.Generic.Diff
import Data.Map(Map)
import Data.String
import Data.Text(Text,pack,unpack)

import Text.XML.Light as X

-- | 'AFrame' describes the contents of an a-frame scene,
--   and is stored as a classical rose tree.

data AFrame       = AFrame Primitive [Attribute] [AFrame]
  deriving Show

newtype Primitive = Primitive Text
  deriving (Show, Eq, Ord, IsString)

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString)

newtype Property  = Property Text
  deriving (Show, Eq, Ord, IsString)

type Attribute = (Label,Property)


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


  