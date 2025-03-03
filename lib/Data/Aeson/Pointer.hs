{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Description: JSON Pointers as described in RFC 6901.
module Data.Aeson.Pointer (
  Pointer(..),
  Key(..),
  Path,
  -- * Representing pointers
  formatPointer,
  parsePointer,
  -- * Using pointers
  get,
  pointerFailure,
) where

import           Data.Aeson (encode)
import qualified Data.Aeson.Key (Key)
import           Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as HM
import           Data.Aeson.Types (FromJSON(parseJSON), Parser, Result(Error), ToJSON(toJSON), Value(Array, Object, Number, String), modifyFailure)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isNumber)
import           Data.Scientific            (toBoundedInteger)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)

-- * Patch components

-- | Path components to traverse a single layer of a JSON document.
data Key
    = OKey Data.Aeson.Key.Key -- ^ Traverse a 'Value' with an 'Object' constructor.
    | AKey Int                -- ^ Traverse a 'Value' with an 'Array' constructor.
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Key where
    toJSON (OKey t) = toJSON t
    toJSON (AKey a) = Number . fromInteger . toInteger $ a

instance FromJSON Key where
    parseJSON (String t) = return . OKey . fromText $ t
    parseJSON (Number n) =
        case toBoundedInteger n of
            Nothing -> fail "A numeric key must be a positive whole number."
            Just n' -> return $ AKey n'
    parseJSON _ = fail "A key element must be a number or a string."

formatKey :: Key -> Text
formatKey (AKey i) = T.pack (show i)
formatKey (OKey t) = T.concatMap esc $ toText t
  where
    esc :: Char -> Text
    esc '~' = "~0"
    esc '/' = "~1"
    esc c = T.singleton c

-- * Pointers

-- | A sequence of 'Key's forms a path through a JSON document.
type Path = [Key]

-- | Pointer to a location in a JSON document.
--
-- Defined in RFC 6901 <http://tools.ietf.org/html/rfc6901>
newtype Pointer = Pointer { pointerPath :: Path }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Generic)

-- | Format a 'Pointer' as described in RFC 6901.
--
-- >>> formatPointer (Pointer [])
-- ""
-- >>> formatPointer (Pointer [OKey ""])
-- "/"
-- >>> formatPointer (Pointer [OKey " "])
-- "/ "
-- >>> formatPointer (Pointer [OKey "foo"])
-- "/foo"
-- >>> formatPointer (Pointer [OKey "foo", AKey 0])
-- "/foo/0"
-- >>> formatPointer (Pointer [OKey "a/b"])
-- "/a~1b"
-- >>> formatPointer (Pointer [OKey "c%d"])
-- "/c%d"
-- >>> formatPointer (Pointer [OKey "e^f"])
-- "/e^f"
-- >>> formatPointer (Pointer [OKey "g|h"])
-- "/g|h"
-- >>> formatPointer (Pointer [OKey "i\\j"])
-- "/i\\j"
-- >>> formatPointer (Pointer [OKey "k\"l"])
-- "/k\"l"
-- >>> formatPointer (Pointer [OKey "m~n"])
-- "/m~0n"
formatPointer :: Pointer -> Text
formatPointer (Pointer []) = ""
formatPointer (Pointer path) = "/" <> T.intercalate "/" (formatKey <$> path)

-- | Parse a 'Pointer' as described in RFC 6901.
parsePointer :: Text -> Parser Pointer
parsePointer t
  | T.null t = return (Pointer [])
  | otherwise = Pointer <$> mapM key (drop 1 $ T.splitOn "/" t)
  where
    step t
      | "0" `T.isPrefixOf` t = T.cons '~' (T.tail t)
      | "1" `T.isPrefixOf` t = T.cons '/' (T.tail t)
      | otherwise = T.cons '~' t
    unesc :: Text -> Text
    unesc t =
      let l = T.split (== '~') t
      in T.concat $ take 1 l <> fmap step (tail l)
    key t
      | T.null t         = fail "JSON components must not be empty."
      | T.all isNumber t = return (AKey (read $ T.unpack t))
      | otherwise        = return . OKey . fromText $ unesc t

instance ToJSON Pointer where
    toJSON pointer =
        String (formatPointer pointer)

instance FromJSON Pointer where
    parseJSON = modifyFailure ("Could not parse JSON pointer: " <>) . parse
      where
        parse (String t) = parsePointer t
        parse _ = fail "A JSON pointer must be a string."

-- | Follow a 'Pointer' through a JSON document as described in RFC 6901.
get :: Pointer -> Value -> Result Value
get (Pointer []) v = return v
get (Pointer (AKey i : path)) (Array v) =
  maybe (fail "") return (v V.!? i) >>= get (Pointer path)
get (Pointer (OKey n : path)) (Object v) =
  maybe (fail "") return (HM.lookup n v) >>= get (Pointer path)
get pointer value = pointerFailure pointer value

-- | Report an error while following a pointer.
pointerFailure :: Pointer -> Value -> Result a
pointerFailure (Pointer []) _value = Error "Cannot follow empty pointer. This is impossible."
pointerFailure (Pointer path@(key:_)) value =
    Error . BS.unpack $ "Cannot follow pointer " <> pt <> ". Expected " <> ty <> " but got " <> doc
  where
    doc = encode value
    pt = encode path
    ty = case key of
           (AKey _) -> "array"
           (OKey _) -> "object"


-- $setup
-- >>> :set -XOverloadedStrings
