{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.AFrame where

import Data.Map(Map)
import Data.String
import Data.Text(Text,pack,unpack)

import Text.XML.Light as X


-- | 'AFrame' describes the contents of an a-frame scene,
--   and is stored as a classical rose tree.

data AFrame       = AFrame Primitive [(Attribute,Property)] [AFrame]
  deriving Show

newtype Primitive = Primitive Text
  deriving (Show, IsString)

newtype Attribute = Attribute Text
  deriving (Show, IsString)

newtype Property  = Property Text
  deriving (Show, IsString)


-- | 'aFrameToElement' converts an 'AFrame' to an (XML) 'Element'. Total.
aFrameToElement :: AFrame -> Element
aFrameToElement (AFrame prim attrs rest) = node (unqual $ unpack prim') (attrs',rest')
  where
    Primitive prim' = prim
    attrs'          = [ Attr (unqual $ unpack a) (unpack p) 
                      | (Attribute a,Property p) <- attrs 
                      ]
    rest'           = map aFrameToElement rest


-- | 'aFrameToElement' converts a (XML) 'Element' to an 'AFrame'. Total.
-- Strips out any text (which is not used by 'AFrame' anyway.)
elementToAFrame :: Element -> AFrame
elementToAFrame ele = AFrame prim' attrs' content'
  where
    prim'    = Primitive $ pack $ qName $ elName $ ele
    attrs'   = [ (Attribute $ pack $ qName $ a,Property $ pack $ p)| Attr a p <- elAttribs ele ]
    content' = [ elementToAFrame ele' | Elem ele' <- elContent ele ]
    
    