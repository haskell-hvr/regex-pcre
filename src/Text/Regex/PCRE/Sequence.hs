{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE.Sequence(
  -- ** Types
  Regex,
  MatchOffset,
  MatchLength,
  CompOption(CompOption),
  MatchOption(MatchOption),
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
  compEndAnchored, -- new in v1.0.0.0 (pcre2)
  compAllowEmptyClass, -- new in v1.0.0.0 (pcre2)
  compAltBSUX, -- new in v1.0.0.0 (pcre2)
  compAltExtendedClass, -- new in v1.0.0.0 (pcre2)
  compAltVerbnames, -- new in v1.0.0.0 (pcre2)
  compAutoCallout,
  compCaseless,
  compDollarEndOnly,
  compDotAll,
  compDupNames, -- new in v1.0.0.0 (pcre2)
  compExtended,
  compExtendedMore, -- new in v1.0.0.0 (pcre2)
--   compExtra, -- obsoleted in v1.0.0.0, pcre2 is always strict in this way
  compFirstLine,
  compLiteral, -- new in v1.0.0.0 (pcre2)
  compMatchUnsetBackref, -- new in v1.0.0.0 (pcre2)
  compMultiline,
  compNeverBackslashC, -- new in v1.0.0.0 (pcre2)
  compNoAutoCapture,
  compNoAutoPossess, -- new in v1.0.0.0 (pcre2)
  compNoDotstarAnchor, -- new in v1.0.0.0 (pcre2)
--   compNoUTF8Check, -- obsoleted in v1.0.0.0 (pcre2), use compNoUTFCheck
  compNoUTFCheck,
  compUngreedy,
--   compUTF8, -- obsoleted in v1.0.0.0 (pcre2), use compUTF
  compUTF,
  -- ** MatchOption flags, new to v1.0.0.0 (pcre2), replacing the obsolete ExecOptions
  matchBlank,
  matchAnchored,
  matchCopyMatchedSubject, -- new in v1.0.0.0 (pcre2)
  matchDisableRecurseLoopCheck, -- new in v1.0.0.0 (pcre2)
  matchEndAnchored, -- new in v1.0.0.0 (pcre2)
  matchNotBOL,
  matchNotEOL,
  matchNotEmpty,
  matchNotEmptyAtStart, -- new in v1.0.0.0 (pcre2)
  matchNoUTFCheck,
  matchPartialHard,
  matchPartialSoft
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(fail))

import Text.Regex.PCRE.Wrap -- all
--import Foreign.C.String(withCStringLen,withCString)
import Data.Array(Array,listArray)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexLike(..),RegexContext(..),MatchLength,MatchOffset,Extract(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Data.Sequence as S hiding (length)
import qualified Data.Sequence as S (length)
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

instance RegexContext Regex (Seq Char) (Seq Char) where
  match = polymatch
  matchM = polymatchM

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.PCRE.Sequence died: "++ show err)
                     Right v -> return v

instance RegexMaker Regex CompOption MatchOption (Seq Char) where
  makeRegexOpts c e pattern = unsafePerformIO $
    compile c e pattern >>= unwrap
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $
    compile c e pattern

instance RegexLike Regex (Seq Char) where
  matchTest regex str = unsafePerformIO $
    withSeq str (wrapTest 0 regex) >>= unwrap
  matchOnce regex str = unsafePerformIO $
    execute regex str >>= unwrap
  matchAll regex str = unsafePerformIO $
    withSeq str (wrapMatchAll regex) >>= unwrap
  matchCount regex str = unsafePerformIO $
    withSeq str (wrapCount regex) >>= unwrap

-- | Compiles a regular expression
compile :: CompOption  -- ^ Flags (summed together)
        -> MatchOption -- ^ Flags (summed together)
        -> (Seq Char)  -- ^ The regular expression to compile
        -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: an error string and offset or the compiled regular expression
compile c e pattern = withSeq pattern (wrapCompile c e)

-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> (Seq Char) -- ^ (Seq Char) to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.
execute regex str = do
  maybeStartEnd <- withSeq str (wrapMatch 0 regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) -> fail "got [] back!" -- should never happen
    Right (Just parts) ->
      return . Right . Just . listArray (0,pred (length parts))
      . map (\(s,e)->(fromIntegral s, fromIntegral (e-s))) $ parts
    Left err -> return (Left err)

-- | execute match and extract substrings rather than just offsets
regexec  :: Regex      -- ^ compiled regular expression
         -> (Seq Char)     -- ^ string to match
         -> IO (Either WrapError (Maybe ((Seq Char), (Seq Char),(Seq Char), [(Seq Char)])))
                      -- ^ Returns: Nothing if no match, else
                      --   (text before match, text after match, array of matches with 0 being the whole match)
regexec regex str = do
  let getSub (start,stop) | start == unusedOffset = S.empty
                          | otherwise = extract (start,stop-start) str
      matchedParts [] = (S.empty,S.empty,str,[]) -- no information
      matchedParts (matchedStartStop@(start,stop):subStartStop) =
        (before start str
        ,getSub matchedStartStop
        ,after stop str
        ,map getSub subStartStop)
  maybeStartEnd <- withSeq str (wrapMatch 0 regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) -> fail "got [] back!" -- should never happen
    Right (Just parts) -> return . Right . Just . matchedParts $ parts
    Left err -> return (Left err)

withSeq :: Seq Char -> (CStringLen -> IO a) -> IO a
withSeq s f =
  let
      len = S.length s
      pokes p a | seq p (seq a False) = undefined
                | otherwise =
        case viewl a of
          EmptyL -> return ()
          c :< a' -> poke p (castCharToCChar c) >> pokes (advancePtr p 1) a'
  in allocaBytes (S.length s) (\ptr -> pokes ptr s >> f (ptr,len))
