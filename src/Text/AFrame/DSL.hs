{-# LANGUAGE GADTs #-}
-- | Small monadic/applicative DSL for AFrame
module Text.AFrame.DSL where
 
import Control.Monad
import Data.Text(Text,unpack)
import Text.AFrame

scene :: DSL () -> AFrame
scene m = case run prog 0 of
             ([],[f]) -> f
  where prog = prim "scene" m
        run :: DSL () -> Int -> ([(Attribute,Property)],[AFrame])
        run (Pure a) _ = ([],[])
        run (Bind (Node nm m0) m) i = (attrs,AFrame (Primitive nm) attrs0 frames0:frames)
          where  (attrs,frames) = run m i
                 (attrs0,frames0) = run m0 i
        run (Bind (Attr a p) m) i = ((a,p):attrs,frames)
          where (attrs,frames) = run m i
        
--  AFrame (Primitive "scene") [] []

data DSL a where
  Pure :: a               -> DSL a
  Bind :: Item -> DSL a   -> DSL a
  Alloc :: (Int -> DSL a) -> DSL a

data Item where
  Node :: Text -> DSL () -> Item
  Attr :: Attribute -> Property -> Item

instance Functor DSL where
  fmap f g = pure f <*> g

instance Applicative DSL where
  pure = Pure
  (<*>) = ap

instance Monad DSL where
  return = pure
  (Pure a)    >>= k  = k a
  (Bind m k1) >>= k2 = Bind m (k1 >>= k2)
  (Alloc k)   >>= k2 = Alloc (\ i -> k i >>= k2)

prim :: Text -> DSL () -> DSL ()
prim nm m = Bind (Node nm m) (Pure ())

attr :: Attribute -> Property -> DSL ()
attr a p = Bind (Attr a p) (Pure ())

example :: AFrame 
example = scene $ 
  attr "Hello" "World"





