module Data.NonEmptyText
    ( NonEmptyText

      -- Creation
    , new
    , singleton
    , toText
    , fromText

      -- Basic interface
    , cons
    , snoc
    , uncons
    , unsnoc
    , append
    , Data.NonEmptyText.head
    , Data.NonEmptyText.last
    , Data.NonEmptyText.tail
    , Data.NonEmptyText.init
    , Data.NonEmptyText.length
    , isSingleton
    ) where

import Data.Bifunctor ( bimap )
import qualified Data.Text as Text


data NonEmptyText = NonEmptyText Char Text.Text
                      deriving (Eq, Ord)


instance Show NonEmptyText where
    show = show . toText


-- | /O(1)/ Create a new 'NonEmptyText'
--
-- >>> new 'h' "ello world"
-- "hello world"
--
new :: Char -> Text.Text -> NonEmptyText
new = NonEmptyText


-- | /O(1)/ Convert a character into a 'NonEmptyText'.
--
-- >>> singleton 'a'
-- "a"
--
singleton :: Char -> NonEmptyText
singleton = flip NonEmptyText Text.empty


-- | /O(1)/ Check if the string is composed of only one character
isSingleton :: NonEmptyText -> Bool
isSingleton = Text.null . snd . uncons


-- | /O(n)/ Prefixes the 'NonEmptyText' with one character
cons :: Char -> NonEmptyText -> NonEmptyText
cons h t = new h (toText t)


-- | /O(n)/ Suffixes the 'NonEmptyText' with one character
snoc :: NonEmptyText -> Char -> NonEmptyText
snoc (NonEmptyText h t) c = new h (Text.snoc t c)


-- | /O(n)/ Appends one 'NonEmptyText' to another
--
-- >>> append <$> fromText "hello," <*> fromText " world."
-- Just "hello, world."
append :: NonEmptyText -> NonEmptyText -> NonEmptyText
append (NonEmptyText h t) = new h . Text.append t . toText


-- | /O(1)/ Return the first character and the rest of the 'NonEmptyText'
uncons :: NonEmptyText -> (Char, Text.Text)
uncons (NonEmptyText h t) = (h, t)


-- | /O(n)/ Return the beginning of the 'NonEmptyText', and its last character
unsnoc :: NonEmptyText -> (Text.Text, Char)
unsnoc (NonEmptyText h t) =
    case unsnocT t of
        Nothing     -> (Text.empty, h)
        Just (m, e) -> (Text.cons h m, e)
  where
    unsnocT :: Text.Text -> Maybe (Text.Text, Char)
    unsnocT text = -- Some old version of Data.Text don't have unsnoc
        let n = Text.length text - 1 in
        if Text.null text
        then Nothing
        else Just (Text.take n text, Text.index text n)


-- | /O(1)/ Return the first of the 'NonEmptyText'
--
-- As opposed to 'Data.Text.head', this is guaranteed to succeed, as the
-- the text is never empty.
head :: NonEmptyText -> Char
head = fst . uncons


-- | /O(1)/ Return the last character of the 'NonEmptyText'
--
-- This never fails.
last :: NonEmptyText -> Char
last = snd . unsnoc


-- | /O(1)/ Return all characters of the 'NonEmptyText' but the first one
tail :: NonEmptyText -> Text.Text
tail = snd . uncons


-- | /O(n)/ Return all character of the 'NonEmptyText' but the last one
init :: NonEmptyText -> Text.Text
init = fst . unsnoc


-- | /O(n)/ Return the length of the total 'NonEmptyText'.
length :: NonEmptyText -> Int
length = (1 +) . Text.length . Data.NonEmptyText.tail


-- | /O(n)/ Convert to NonEmptyText to Text.
--
-- The 'Data.Text.Text' result is guaranteed to be non-empty. However, this is
-- not reflected in the type.
toText :: NonEmptyText -> Text.Text
toText = uncurry Text.cons . uncons


-- | /O(n)/ 'map' @f@ @t@ is the 'NonEmptyText' obtained by applying @f@ to
-- each element of @t@.
map :: (Char -> Char) -> NonEmptyText -> NonEmptyText
map f = uncurry new . bimap f (Text.map f) . uncons


-- | /O(n)/ Create a 'NonEmptyText' from 'Data.Text.Text'.
--
-- If the original text is empty, this will return 'Data.Maybe.Nothing'.
--
-- >>> fromText "hello"
-- Just "hello"
-- >>> fromText ""
-- Nothing
fromText :: Text.Text -> Maybe NonEmptyText
fromText t = Prelude.uncurry NonEmptyText <$> Text.uncons t
