{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# OPTIONS_GHC -fsimpl-tick-factor=40000 #-}
#endif

------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Html.Word
-- Copyright:   (c) 2016 Dylan Simon
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- 'W.Write's and 'B.Builder's for serializing HTML escaped 'Word8' characters
-- and 'BS.ByteString's that have already been appropriately encoded into HTML by
-- escaping basic ASCII character references but leaving other bytes untouched.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Html.Word
  ( 
    -- * Writing HTML escaped bytes to a buffer
    writeHtmlEscapedWord
    -- * Creating Builders from HTML escaped bytes
  , fromHtmlEscapedWord
  , fromHtmlEscapedWordList
  , fromHtmlEscapedByteString
  , fromHtmlEscapedLazyByteString
#if MIN_VERSION_text(1,1,2) && MIN_VERSION_bytestring(0,10,4)
    -- * Creating Builders from HTML escaped and UTF-8 encoded text
    -- | /Note/ that these functions are only available if built against @text >= 1.1.2.0@ and @bytestring >= 0.10.4.0@.
  , fromHtmlEscapedText
  , fromHtmlEscapedLazyText
#endif
  ) where

import qualified Blaze.ByteString.Builder.Compat.Write as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as P
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

{-# INLINE wordHtmlEscaped #-}
wordHtmlEscaped :: P.BoundedPrim Word8
wordHtmlEscaped =
  P.condB (>  c2w '>' ) (P.condB (== c2w '\DEL') P.emptyB $ P.liftFixedToBounded P.word8) $
  P.condB (== c2w '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
  P.condB (== c2w '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
  P.condB (== c2w '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
  P.condB (== c2w '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $  -- &quot;
  P.condB (== c2w '\'') (fixed5 ('&',('#',('3',('9',';'))))) $  -- &#39;
  P.condB (\c -> c >= c2w ' ' || c == c2w '\t' || c == c2w '\n' || c == c2w '\r')
        (P.liftFixedToBounded P.word8) P.emptyB
  where
  {-# INLINE fixed4 #-}
  fixed4 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8
  {-# INLINE fixed5 #-}
  fixed5 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8
  {-# INLINE fixed6 #-}
  fixed6 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8

-- | Write a HTML escaped byte to a bufffer.
writeHtmlEscapedWord :: Word8 -> W.Write
writeHtmlEscapedWord = W.writePrimBounded wordHtmlEscaped

-- | /O(1)./ Serialize a HTML escaped byte.
fromHtmlEscapedWord :: Word8 -> B.Builder
fromHtmlEscapedWord = P.primBounded wordHtmlEscaped

-- | /O(n)/. Serialize a HTML escaped list of bytes.
fromHtmlEscapedWordList :: [Word8] -> B.Builder
fromHtmlEscapedWordList = P.primMapListBounded wordHtmlEscaped

-- | /O(n)/. Serialize a HTML escaped 'BS.ByteString'.
fromHtmlEscapedByteString :: BS.ByteString -> B.Builder
fromHtmlEscapedByteString = P.primMapByteStringBounded wordHtmlEscaped

-- | /O(n)/. Serialize a HTML escaped lazy 'BSL.ByteString'.
fromHtmlEscapedLazyByteString :: BSL.ByteString -> B.Builder
fromHtmlEscapedLazyByteString = P.primMapLazyByteStringBounded wordHtmlEscaped

#if MIN_VERSION_text(1,1,2) && MIN_VERSION_bytestring(0,10,4)
-- | /O(n)/. Serialize a HTML escaped strict 'T.Text' using the UTF-8 encoding.
-- This is identical to 'Blaze.ByteString.Builder.Html.Utf8.fromHtmlEscapedText' but more than twice as fast.
fromHtmlEscapedText :: T.Text -> B.Builder
fromHtmlEscapedText = TE.encodeUtf8BuilderEscaped wordHtmlEscaped

-- | /O(n)/. Serialize a HTML escaped lazy 'TL.Text' using the UTF-8 encoding.
-- This is identical to 'Blaze.ByteString.Builder.Html.Utf8.fromHtmlEscapedLazyText' but more than three times as fast.
fromHtmlEscapedLazyText :: TL.Text -> B.Builder
fromHtmlEscapedLazyText = TLE.encodeUtf8BuilderEscaped wordHtmlEscaped
#endif
