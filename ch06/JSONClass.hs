{-# LANGUAGE FlexibleInstances #-}
-- file: ch06/JSONClass.hs

module JSONClass
  (
    JAry(..)
  ) where

import SimpleJSON (JValue(..))

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue) -- was [(String, JValue)]
            | JArray (JAry JValue) -- was [JValue]
              deriving(Eq, Ord, Show)

newtype JObj a = JObj {
  fromObj :: [(String, a)]
  } deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "not a JSON boolean"

instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = Left "not a JSON string"

doubleToValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToValue f (JNumber v) = Right (f v)
doubleToValue _ _ = Left "not a JSON number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToValue id

instance {-# OVERLAPPING #-} (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined

instance {-# OVERLAPPING #-} (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined

newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving(Eq, Ord, Show)

jarrayFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jarrayToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
  toJValue = jarrayToJValue
  fromJValue = jarrayFromJValue

listToJValue :: (JSON a) => [a] -> [JValue]
listToJValue = map toJValue

jvalueToAry :: [JValue] -> JAry JValue
jvalueToAry = JAry
