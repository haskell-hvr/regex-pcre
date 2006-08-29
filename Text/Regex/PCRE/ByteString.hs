{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-|
This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.PCRE.ByteString(
  -- ** Types
  Regex,
  MatchOffset,
  MatchLength,
  CompOption(CompOption),
  ExecOption(ExecOption),
  ReturnCode,
  WrapError,
  -- ** Miscellaneous
  unusedOffset,
  getVersion,
  -- ** Medium level API functions
  compile,
  execute,
  regexec,
  -- ** CompOption flags
  compBlank,
  compAnchored,
  compAutoCallout,
  compCaseless,
  compDollarEndOnly,
  compDotAll,
  compExtended,
  compExtra,
  compFirstLine,
  compMultiline,
  compNoAutoCapture,
  compUngreedy,
  compUTF8,
  compNoUTF8Check,
  -- ** ExecOption flags
  execBlank,
  execAnchored,
  execNotBOL,
  execNotEOL,
  execNotEmpty,
  execNoUTF8Check,
  execPartial
  ) where

import Text.Regex.PCRE.Wrap -- all
import Data.Array(Array,listArray)
import Data.ByteString(ByteString)
import qualified Data.ByteString as B(empty,useAsCString,last,take,drop)
import qualified Data.ByteString.Base as B(unsafeUseAsCString,unsafeUseAsCStringLen)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexContext(..),RegexMaker(..),RegexLike(..),MatchOffset,MatchLength)
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Foreign.C.String(CStringLen)

instance RegexContext Regex ByteString ByteString where
  match = polymatch
  matchM = polymatchM

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.PCRE.ByteString died: "++ show err)
                     Right v -> return v

asCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
asCStringLen = B.unsafeUseAsCStringLen

instance RegexMaker Regex CompOption ExecOption ByteString where
  makeRegexOpts c e pattern = unsafePerformIO $
    compile c e pattern >>= unwrap

instance RegexLike Regex ByteString where
  matchTest regex bs = unsafePerformIO $
    asCStringLen bs (wrapTest 0 regex) >>= unwrap
  matchOnce regex bs = unsafePerformIO $
    execute regex bs >>= unwrap
  matchAll regex bs = unsafePerformIO $ 
    asCStringLen bs (wrapMatchAll regex) >>= unwrap
  matchCount regex bs = unsafePerformIO $ 
    asCStringLen bs (wrapCount regex) >>= unwrap

-- ---------------------------------------------------------------------
-- | Compiles a regular expression
--
compile :: CompOption  -- ^ (summed together)
        -> ExecOption  -- ^ (summed together)
        -> ByteString  -- ^ The regular expression to compile
        -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: the compiled regular expression
compile c e pattern = do
  let asCString = if (0==B.last pattern)
                    then B.unsafeUseAsCString
                    else B.useAsCString
  asCString pattern (wrapCompile c e)

-- ---------------------------------------------------------------------
-- | Matches a regular expression against a buffer, returning the buffer
-- indicies of the match, and any submatches
--
-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.
execute regex bs = do
  maybeStartEnd <- asCStringLen bs (wrapMatch 0 regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
    Right (Just parts) -> 
      return . Right . Just . listArray (0,pred (length parts))
      . map (\(s,e)->(fromIntegral s, fromIntegral (e-s))) $ parts
    Left err -> return (Left err)

regexec :: Regex      -- ^ Compiled regular expression
        -> ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (ByteString, ByteString, ByteString, [ByteString])))
regexec regex bs = do
  let getSub (start,stop) | start == unusedOffset = B.empty
                          | otherwise = B.take (stop-start) . B.drop start $ bs
      matchedParts [] = (B.empty,B.empty,bs,[]) -- no information
      matchedParts (matchedStartStop@(start,stop):subStartStop) = 
        (B.take start bs
        ,getSub matchedStartStop
        ,B.drop stop bs
        ,map getSub subStartStop)
  maybeStartEnd <- asCStringLen bs (wrapMatch 0 regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
    Right (Just parts) -> return . Right . Just . matchedParts $ parts
    Left err -> return (Left err)
