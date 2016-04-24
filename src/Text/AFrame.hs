{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.AFrame where

import Data.Map(Map)
import Data.String
import Data.Text(Text,unpack)

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


aframeToElement :: AFrame -> Element
aframeToElement (AFrame prim attrs rest) = node (unqual $ unpack prim') (attrs',rest')
  where
    Primitive prim' = prim
    attrs'          = [ Attr (unqual $ unpack a) (unpack p) 
                      | (Attribute a,Property p) <- attrs 
                      ]
    rest'           = map aframeToElement rest


