{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Roman Gonzalez 2013,
--              (c) Edward Kmett 2013, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Roman Gonzalez <roamandreg@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Control.Lens.EDN
  (
  -- * Numbers
  --   AsNumber(..)
  -- , integralValue
  -- , nonNil
  -- * Primitive
    Primitive(..)
  , AsPrimitive(..)
  -- * Objects and Arrays
  , AsValue(..)
  , key, nth
  -- * Decoding
  , AsEDN(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.EDN
import Data.EDN.Types
import Data.Attoparsec.Number
import Data.Data
import Data.Map (Map)
import Data.Text
import Data.Vector (Vector)
import Numeric.Lens
import Prelude hiding(null)

import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as Lazy hiding (putStrLn)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 hiding (decode)


-- $setup
-- >>> :set -XOverloadedStrings


-- ------------------------------------------------------------------------------
-- -- Number prisms
-- ------------------------------------------------------------------------------

-- class AsNumber t where
--   _Number :: Prism' t Number
--   default _Number :: AsPrimitive t => Prism' t Number
--   _Number = _Primitive._Number

--   -- | Prism into an 'Double' over a 'Value', 'Primitive' or 'Number'
--   _Double :: Prism' t Double
--   _Double = _Number.prism D (\v -> case v of D d -> Right d; _ -> Left v)

--   -- | Prism into an 'Integer' over a 'Value', 'Primitive' or 'Number'
--   _Integer :: Prism' t Integer
--   _Integer = _Number.prism I (\v -> case v of I i -> Right i; _ -> Left v)

-- instance AsNumber Value where
--   _Number = prism Number $ \v -> case v of Number n -> Right n; _ -> Left v

-- instance AsNumber Number where
--   _Number = id

-- instance AsNumber ByteString
-- instance AsNumber String

-- ------------------------------------------------------------------------------
-- -- Conversion Prisms
-- ------------------------------------------------------------------------------

-- -- | Access Integer 'Value's as Integrals.
-- --
-- -- defined as `integer . 'Numeric.Lens.integral'`
-- integralValue :: (AsNumber t, Integral a) => Prism' t a
-- integralValue = _Integer . integral

-- ------------------------------------------------------------------------------
-- -- Null values and primitives
-- ------------------------------------------------------------------------------

-- | Primitives of 'Value'
data Primitive
  = StringPrim !Text
  | CharacterPrim !Char
  | KeywordPrim !B.ByteString
  | SymbolPrim !B.ByteString
  | IntegerPrim !Integer
  | FloatingPrim !Double
  | BooleanPrim !Bool
  | NilPrim
  deriving (Eq,Ord,Show,Data,Typeable)

class AsPrimitive t where
  _Primitive :: Prism' t Primitive
  default _Primitive :: AsValue t => Prism' t Primitive
  _Primitive = _Value._Primitive

  _String :: Prism' t Text
  _String = _Primitive.prism StringPrim (\v -> case v of StringPrim s -> Right s; _ -> Left v)

  _Char :: Prism' t Char
  _Char = _Primitive.prism CharacterPrim (\v -> case v of CharacterPrim c -> Right c; _ -> Left v)

  _Boolean :: Prism' t Bool
  _Boolean = _Primitive.prism BooleanPrim (\v -> case v of BooleanPrim b -> Right b; _ -> Left v)

  _Symbol :: Prism' t B.ByteString
  _Symbol = _Primitive.prism SymbolPrim (\v -> case v of SymbolPrim s -> Right s; _ -> Left v)

  _Keyword :: Prism' t B.ByteString
  _Keyword = _Primitive.prism KeywordPrim (\v -> case v of KeywordPrim k -> Right k; _ -> Left v)

  _Integer :: Prism' t Integer
  _Integer = _Primitive.prism IntegerPrim (\v -> case v of IntegerPrim i -> Right i; _ -> Left v)

  _Floating :: Prism' t Double
  _Floating = _Primitive.prism FloatingPrim (\v -> case v of FloatingPrim f -> Right f; _ -> Left v)

  _Nil :: Prism' t ()
  _Nil = _Primitive.prism (const NilPrim) (\v -> case v of NilPrim -> Right (); _ -> Left v)

instance AsPrimitive Value where
  _Primitive = prism fromPrim toPrim
    where
      toPrim (String s)   = Right $ StringPrim s
      toPrim (Integer n)  = Right $ IntegerPrim n
      toPrim (Floating n) = Right $ FloatingPrim n
      toPrim (Boolean b)  = Right $ BooleanPrim b
      toPrim (Keyword k)  = Right $ KeywordPrim k
      toPrim (Symbol pr s)
         | B.null pr = Right $ SymbolPrim s
         | otherwise =
           Right $ SymbolPrim $ B.concat [pr,B.pack "/",s]
      toPrim Nil          = Right $ NilPrim
      toPrim v            = Left v

      fromPrim (StringPrim s)   = String s
      fromPrim (IntegerPrim n)  = Integer n
      fromPrim (FloatingPrim n) = Floating n
      fromPrim (BooleanPrim b)  = Boolean b
      fromPrim (KeywordPrim n)  = Keyword n
      fromPrim NilPrim          = Nil
      fromPrim (SymbolPrim b)   =
        let [prefix, s] = B.split '/' b
        in Symbol prefix s

  _String   = prism String $ \v -> case v of String s -> Right s; _ -> Left v
  _Char     = prism Character $ \v -> case v of Character s -> Right s; _ -> Left v
  _Keyword  = prism Keyword (\v -> case v of Keyword b -> Right b; _ -> Left v)
  _Boolean  = prism Boolean (\v -> case v of Boolean b -> Right b; _ -> Left v)
  _Symbol   = prism (Symbol B.empty)
                    (\v -> case v of
                             (Symbol s1 s2) -> Right $ B.concat [s1, B.pack "/", s2];
                             _ -> Left v)
  _Nil      = prism (const Nil) (\v -> case v of Nil -> Right (); _ -> Left v)

instance AsPrimitive TaggedValue
instance AsPrimitive UTF8.ByteString
instance AsPrimitive String

instance AsPrimitive Primitive where
  _Primitive = id

-- -- | Prism into non-'Null' values
nonNil :: Prism' Value Value
nonNil = prism id (\v -> if isn't _Nil v then Right v else Left v)

-- ------------------------------------------------------------------------------
-- -- Non-primitive traversals
-- ------------------------------------------------------------------------------

class AsPrimitive t => AsValue t where
  -- |
  -- >>> putStrLn $ "{\"a\": 1, \"b\": 3}" & key "a"._Integer *~ 100
  -- {"a":100,"b":3}
  _Value :: Prism' t Value

  _Map :: Prism' t EDNMap
  _Map = _Value.prism Map (\v -> case v of Map o -> Right o; _ -> Left v)

  _Vector :: Prism' t EDNVec
  _Vector = _Value.prism Vec (\v -> case v of Vec a -> Right a; _ -> Left v)

  _List :: Prism' t EDNList
  _List = _Value.prism List (\v -> case v of List a -> Right a; _ -> Left v)

  _Set :: Prism' t EDNSet
  _Set = _Value.prism Set (\v -> case v of Set a -> Right a; _ -> Left v)


instance AsValue Value where
  _Value = id

instance AsValue (Tagged Value) where
  _Value = iso stripTag notag

instance AsValue Lazy.ByteString where
  _Value = _EDN

instance AsValue String where
  _Value = iso UTF8.fromString UTF8.toString._Value


-- -- | Like 'ix', but for 'Object' with Text indices. This often has better inference than 'ix' when used with OverloadedStrings
key :: AsValue t => B.ByteString -> Traversal' t TaggedValue
key i = _Map . ix (Keyword i)

-- newtype Seq = Seq Value

-- class AsSeq v where
--   _Seq :: Prism' v Seq
--   default _Seq :: AsPrimitive t => Prism' t Seq
--   _Seq = _Primitive._Seq

--   _Vector :: Prism' v EDNVec
--   _Vector = _Seq.prism Vec (\val -> case val of Vec v -> Right v; _ -> Left val)

--   _List   :: Prism' v EDNList
--   _List = _Seq.prism  List (\val -> case val of List l -> Right l; _ -> Left val)

-- instance AsSeq Value where
--   _Seq = prism Seq (\v -> )


-- | Like 'ix', but for Arrays with Int indexes
nth :: AsValue t => Int -> Traversal' t TaggedValue
nth i = _Vector . ix i

class AsEDN t where
  -- | A Prism into 'Value' on lazy 'ByteString's.
  _EDN :: (FromEDN a, ToEDN a) => Prism' t a

instance AsEDN Lazy.ByteString where
  _EDN = prism' encode decode

instance AsEDN String where
  _EDN = iso UTF8.fromString UTF8.toString._EDN

-- ------------------------------------------------------------------------------
-- -- Orphan instances
-- ------------------------------------------------------------------------------

type instance Index Value = Value
type instance IxValue Value = TaggedValue

-- type instance Index TaggedValue = Value
-- type instance IxValue TaggedValue = TaggedValue

instance Applicative f => Ixed f Value where
  ix i = _Map.ix i

instance (Applicative f, Gettable f) => Contains f Value where
  contains i f (Map o) = coerce (contains i f o)
  contains i f _ = coerce (indexed f i False)

instance Plated Value where
  plate f v =
      case v of
        Map m  -> Map  <$> traverse f' m
        Vec v  -> Vec  <$> traverse f' v
        List l -> List <$> traverse f' l
        -- Set s  -> Set <$> traverse f' s
        xs -> pure xs
    where
      f' (NoTag v') = NoTag <$> f v'
      f' (Tagged v' s1 s2) = Tagged <$> f v'
                                    <*> pure s1
                                    <*> pure s2
