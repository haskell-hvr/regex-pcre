-- The exported symbols are the same whether HAVE_PCRE_H is defined,
-- but when if it is not defined then 'getVersion == Nothing' and all
-- other exported values will call error or fail.

-- | This will fail or error only if allocation fails or a nullPtr is passed in.

-- TODO :: Consider wrapMatchAll using list of start/end offsets and not MatchArray
--

{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE.Wrap(
  -- ** High-level interface
  Regex,
  CompOption(CompOption),
--   ExecOption(ExecOption), -- obsoleted in v1.0.0.0 (pcre2), use MatchOption instead
  MatchOption(MatchOption),
  (=~),
  (=~~),

  -- ** Low-level interface
  StartOffset,
  EndOffset,
  ReturnCode(ReturnCode),
  WrapError,
  wrapCompile,
  wrapTest,
  wrapMatch,
  wrapMatchAll,
  wrapCount,

  -- ** Miscellaneous
  getVersion,
  configUTF8, -- deprecated in v1.0.0.0 (pcre2), UFT8 is always supported
  getNumSubs,
  unusedOffset,

  -- ** CompOption values
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
--  compExtra, -- obsoleted in v1.0.0.0, pcre2 is always strict in this way
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

  -- ** MatchOption values, new to v1.0.0.0 (pcre2), replacing the obsolete ExecOptions
  matchBlank,
  matchAnchored,
  matchCopyMatchedSubject, -- new in v1.0.0.0 (pcre2)
  matchDisableRecurseLoopCheck, -- new in v1.0.0.0 (pcre2)
  matchEndAnchored, -- new in v1.0.0.0 (pcre2)
  matchNotBOL,
  matchNotEOL,
  matchNotEmpty,
  matchNotEmptyAtStart, -- new in v1.0.0.0 (pcre2)
  matchNoUTFCheck, -- equivalent to the obsolete execNoUTF8Check
  matchPartialHard,
  matchPartialSoft, -- equivalent to the obsolete execPartial

  -- ** ReturnCode values
  retOk,
  retNoMatch,
  retPartial, -- new in v1.0.0.0 (pcre2)
  retNull,
  retBadOption,
  retBadMagic,
  retUnknownNode, -- deprecated in v1.0.0.0 (pcre2), no longer ever returned
  retNoMemory,
  retNoSubstring
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(fail))

import Control.Exception(bracket)
import Control.Monad(when)
import Data.Array(Array,accumArray)
import Data.Bits(Bits((.|.))) -- ((.&.),(.|.),complement))
import Data.Word(Word32)
import System.IO.Unsafe(unsafePerformIO)
import Foreign(Ptr,ForeignPtr,FinalizerPtr -- ,FunPtr
              ,alloca,allocaBytes,nullPtr
              ,mallocBytes,free
              ,peek,peekElemOff
              ,newForeignPtr,withForeignPtr)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C(CInt(CInt),CSize(CSize))
#else
import Foreign.C(CInt,CSize)
#endif
import Foreign.C.String(CString,CStringLen,peekCString)
import Text.Regex.Base.RegexLike(RegexOptions(..),RegexMaker(..),RegexContext(..),MatchArray,MatchOffset)

-- | Version string of PCRE library
--
-- __NOTE__: The 'Maybe' type is used for historic reasons; practically, 'getVersion' is never 'Nothing'.
{-# NOINLINE getVersion #-}
getVersion :: Maybe String

type PCRE = ()
type CompContext = ()
type MatchContext = ()
type MatchData = ()
type StartOffset = MatchOffset
type EndOffset = MatchOffset
type WrapError = (ReturnCode,String)

newtype CompOption  = CompOption  Word32 deriving (Eq,Show,Num,Bits)
newtype MatchOption = MatchOption Word32 deriving (Eq,Show,Num,Bits)
newtype ReturnCode  = ReturnCode  CInt deriving (Eq,Show)

-- | A compiled regular expression
data Regex = Regex (ForeignPtr PCRE) CompOption MatchOption Int

compBlank :: CompOption
matchBlank :: MatchOption
unusedOffset :: MatchOffset
retOk :: ReturnCode

wrapCompile :: CompOption  -- ^ Flags (summed together)
            -> MatchOption -- ^ Flags (summed together)
            -> CStringLen  -- ^ The regular expression to compile
            -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: an error offset and string or the compiled regular expression
wrapTest :: StartOffset -- ^ Starting index in CStringLen
         -> Regex       -- ^ Compiled regular expression
         -> CStringLen  -- ^ String to match against and length in bytes
         -> IO (Either WrapError Bool)
wrapMatch :: StartOffset -- ^ Starting index in CStringLen
          -> Regex       -- ^ Compiled regular expression
          -> CStringLen  -- ^ String to match against and length in bytes
          -> IO (Either WrapError (Maybe [(StartOffset,EndOffset)]))
                -- ^ Returns: 'Right Nothing' if the regex did not match the
                -- string, or:
                --   'Right Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions, or:
                --   'Left ReturnCode' if there is some strange error
wrapMatchAll :: Regex -> CStringLen -> IO (Either WrapError [ MatchArray ])
wrapCount :: Regex -> CStringLen -> IO (Either WrapError Int)

getNumSubs :: Regex -> Int

{-# NOINLINE configUTF8 #-}
configUTF8 :: Bool

(=~)  :: (RegexMaker Regex CompOption MatchOption source,RegexContext Regex source1 target)
      => source1 -> source -> target
(=~~) :: (RegexMaker Regex CompOption MatchOption source,RegexContext Regex source1 target,MonadFail m)
      => source1 -> source -> m target

#include <sys/types.h>
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

instance RegexOptions Regex CompOption MatchOption where
  blankCompOpt = compBlank
  blankExecOpt = matchBlank
  defaultCompOpt = compMultiline
  defaultExecOpt = matchBlank
  setExecOpts e' (Regex r c _ n) = Regex r c e' n
  getExecOpts (Regex _ _ e _) = e

-- (=~) :: (RegexMaker Regex CompOption MatchOption source,RegexContext Regex source1 target) => source1 -> source -> target
(=~) x r = let q :: Regex
               q = makeRegex r
           in match q x

-- (=~~) ::(RegexMaker Regex CompOption MatchOption source,RegexContext Regex source1 target,MonadFail m) => source1 -> source -> m target
(=~~) x r = do (q :: Regex) <-  makeRegexM r
               matchM q x

fi :: (Integral i,Num n) => i -> n
fi x = fromIntegral x

compBlank = CompOption 0
matchBlank = MatchOption 0
unusedOffset = (-1)
retOk = ReturnCode 0

-- retNeededMoreSpace :: ReturnCode
-- retNeededMoreSpace = ReturnCode 0

newtype InfoWhat   = InfoWhat   Word32 deriving (Eq,Show)
newtype ConfigWhat = ConfigWhat Word32 deriving (Eq,Show)

nullTest' :: Ptr a -> String -> IO (Either (MatchOffset,String) b) -> IO (Either (MatchOffset,String) b)
{-# INLINE nullTest' #-}
nullTest' ptr msg io = do
  if nullPtr == ptr
    then return (Left (0,"Ptr parameter was nullPtr in Text.Regex.PCRE.Wrap."++msg))
    else io

nullTest :: Ptr a -> String -> IO (Either WrapError b) -> IO (Either WrapError b)
{-# INLINE nullTest #-}
nullTest ptr msg io = do
  if nullPtr == ptr
    then return (Left (retOk,"Ptr parameter was nullPtr in Text.Regex.PCRE.Wrap."++msg))
    else io

getErrMsg :: CInt -> IO String
{-# INLINE getErrMsg #-}
getErrMsg errnum = do
  errstr <- mallocBytes 1024
  if nullPtr == errstr
    then return "Ptr parameter was nullPtr in Text.Regex.PCRE.Wrap.getErrMsg errstr"
    else do
      _ <- c_pcre2_get_error_message errnum errstr 1024
      errstr' <- peekCString errstr
      free errstr
      return errstr'

wrapRC :: ReturnCode -> IO (Either WrapError b)
{-# INLINE wrapRC #-}
wrapRC errnum@(ReturnCode errnum') = do
  errstr <- getErrMsg errnum'
  return (Left (errnum,"Error in Text.Regex.PCRE.Wrap: "++errstr))

-- | Compiles a regular expression
wrapCompile flags e (pattern,len) = do
 nullTest' pattern "wrapCompile pattern" $ do
  alloca $ \errOffset -> alloca $ \errPtr -> do
   nullTest' errPtr "wrapCompile errPtr" $ do
    pcre_ptr <- c_pcre2_compile pattern (fi len) flags errPtr errOffset nullPtr
    if pcre_ptr == nullPtr
      then do
        -- No need to use c_pcre2_code_free in the error case (e.g. pcredemo.c)
        offset <- peek errOffset
        errstr <- getErrMsg =<< peek errPtr
        return (Left (fi offset, errstr))
      else do
        alloca $ \st -> do -- (st :: Ptr CInt)
          when (st == nullPtr) (fail "Text.Regex.PCRE.Wrap.wrapCompile could not allocate a CInt for the capture count.")
          ok0 <- c_pcre2_pattern_info pcre_ptr pcre2InfoCapturecount st
          when (ok0 /= 0) (fail $ "Impossible/fatal: Haskell package regex-pcre error in Text.Posix.PCRE.Wrap.getNumSubs' of ok0 /= 0.  ok0 is from pcre2_pattern_info c-function which returned  "++show ok0)
          n <- peek st
          regex <- newForeignPtr c_pcre2_code_free pcre_ptr
          return . Right $ Regex regex flags e n

getNumSubs (Regex _ _ _ n) = n

withDataPtr :: IO (Ptr MatchData) -> String -> (Ptr MatchData -> IO (Either WrapError a)) -> IO (Either WrapError a)
withDataPtr data_create jobname job = bracket data_create c_pcre2_match_data_free job'
  where
    job' dataPtr = nullTest dataPtr (jobname++" dataPtr") (job dataPtr)

wrapTest startOffset (Regex pcre_fptr _ flags _) (cstr,len) = do
 nullTest cstr "wrapTest cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    withDataPtr (c_pcre2_match_data_create 1 nullPtr) "wrapTest" $ \dataPtr -> do
      r@(ReturnCode r') <- c_pcre2_match pcre_ptr cstr (fi len) (fi startOffset) flags dataPtr nullPtr
      if r == retNoMatch
        then return (Right False)
        else if r' < 0
               then wrapRC r
               else return (Right True)

-- | Matches a regular expression against a string
--
-- Should never return (Right (Just []))
wrapMatch startOffset (Regex pcre_fptr _ flags nsub) (cstr,len) = do
 nullTest cstr "wrapMatch cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    withDataPtr (c_pcre2_match_data_create_from_pattern pcre_ptr nullPtr) "wrapMatch" $ \dataPtr -> do
      r@(ReturnCode r') <- c_pcre2_match pcre_ptr cstr (fi len) (fi startOffset) flags dataPtr nullPtr
      if r == retNoMatch
        then do
          return (Right Nothing)
        else if r' < 0
          then wrapRC r
          else do
            ovecsize <- fi <$> c_pcre2_get_ovector_count dataPtr
            ovec     <- c_pcre2_get_ovector_pointer dataPtr
            let extraPairs :: [(Int,Int)]
                extraPairs = replicate (nsub + 1 - ovecsize) (unusedOffset,unusedOffset)
            pairs <- return . toPairs =<< mapM (peekElemOff ovec) [0 .. ((ovecsize*2)-1)]
            return . Right . Just $ (pairs ++ extraPairs)

-- | wrapMatchAll is an improvement over wrapMatch since it only
-- allocates memory with allocaBytes once at the start.
wrapMatchAll (Regex pcre_fptr _ flags nsub) (cstr,len) = do
 nullTest cstr "wrapMatchAll cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    let flags' = (matchNotEmpty .|. matchAnchored .|. flags)
    withDataPtr (c_pcre2_match_data_create_from_pattern pcre_ptr nullPtr) "wrapMatchAll" $ \dataPtr ->
      let loop acc flags_in_use pos = do
            r@(ReturnCode r') <- c_pcre2_match pcre_ptr cstr (fi len) (fi pos) flags_in_use dataPtr nullPtr
            if r == retNoMatch
              then return (Right (acc []))
              else if r' < 0
                     then wrapRC r
                     else do
                       ovecsize <- fi <$> c_pcre2_get_ovector_count dataPtr
                       ovec     <- c_pcre2_get_ovector_pointer dataPtr
                       pairs    <- return . toPairs =<< mapM (peekElemOff ovec) [0 .. ((ovecsize*2)-1)]
                       let acc' = acc . (toMatchArray nsub pairs:)
                       case pairs of
                         [] -> return (Right (acc' []))
                         ((s,e):_) | s==e -> if s == len
                                               then return (Right (acc' []))
                                               else loop acc' flags' e
                                   | otherwise -> loop acc' flags e
      in loop id flags 0
toMatchArray :: Int -> [(Int,Int)] -> Array Int (Int,Int)
toMatchArray n pairs = accumArray (\_ (s,e) -> (s,(e-s))) (-1,0) (0,n) (zip [0..] pairs)

toPairs :: [CSize] -> [(Int,Int)]
toPairs [] = []
toPairs (a:b:rest) = (fi a,fi b):toPairs rest
toPairs [_] = error "Should not have just one element in WrapPCRE.toPairs"

wrapCount (Regex pcre_fptr _ flags _) (cstr,len) = do
 nullTest cstr "wrapCount cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    withDataPtr (c_pcre2_match_data_create_from_pattern pcre_ptr nullPtr) "wrapCount" $ \dataPtr ->
      let act pos = c_pcre2_match pcre_ptr cstr (fi len) (fi pos) flags dataPtr nullPtr
          loop acc pos | acc `seq` pos `seq` False = undefined
                       | otherwise  = do
            r@(ReturnCode r') <- act pos
            if r == retNoMatch
              then return (Right acc)
              else if r' < 0
                then wrapRC r
                else do
                  ovec  <- c_pcre2_get_ovector_pointer dataPtr
                  pairs <- return . toPairs =<< mapM (peekElemOff ovec) [0,1]
                  case pairs of
                    [] -> return (Right (succ acc))
                    ((s,e):_) | s==e -> return (Right (succ acc))
                              | otherwise -> loop (succ acc) e
      in loop 0 0

getVersion = unsafePerformIO $ do
  vsize <- c_pcre2_config pcre2ConfigVersion nullPtr
  allocaBytes vsize $ \v -> do
    if v == nullPtr
      then return Nothing
      else do
        _ <- c_pcre2_config pcre2ConfigVersion v
        Just <$> peekCString v

configUTF8 = True -- deprecated in v1.0.0.0, UTF8 is always supported by pcre2

foreign import ccall unsafe "pcre2.h pcre2_config_8"
  c_pcre2_config :: ConfigWhat -> Ptr a -> IO Int
foreign import ccall unsafe "pcre2.h pcre2_compile_8"
  c_pcre2_compile :: CString -> CSize -> CompOption -> Ptr CInt -> Ptr CSize -> Ptr CompContext -> IO (Ptr PCRE)
foreign import ccall unsafe "pcre2.h pcre2_get_error_message_8"
  c_pcre2_get_error_message :: CInt -> CString -> CSize -> IO CInt
foreign import ccall unsafe "pcre2.h pcre2_pattern_info_8"
  c_pcre2_pattern_info :: Ptr PCRE -> InfoWhat -> Ptr a -> IO CInt
foreign import ccall unsafe "pcre2.h &pcre2_code_free_8"
  c_pcre2_code_free :: FinalizerPtr PCRE
foreign import ccall unsafe "pcre2.h pcre2_match_data_create_8"
  c_pcre2_match_data_create :: Word32 -> Ptr MatchContext -> IO (Ptr MatchData)
foreign import ccall unsafe "pcre2.h pcre2_match_data_create_from_pattern_8"
  c_pcre2_match_data_create_from_pattern :: Ptr PCRE -> Ptr MatchContext -> IO (Ptr MatchData)
foreign import ccall unsafe "pcre2.h pcre2_match_8"
  c_pcre2_match :: Ptr PCRE -> CString -> CSize -> CSize -> MatchOption -> Ptr MatchData -> Ptr MatchContext -> IO ReturnCode
foreign import ccall unsafe "pcre2.h pcre2_get_ovector_count_8"
  c_pcre2_get_ovector_count :: Ptr MatchData -> IO Word32
foreign import ccall unsafe "pcre2.h pcre2_get_ovector_pointer_8"
  c_pcre2_get_ovector_pointer :: Ptr MatchData -> IO (Ptr CSize)
foreign import ccall unsafe "pcre2.h pcre2_match_data_free_8"
  c_pcre2_match_data_free :: Ptr MatchData -> IO ()

-- compExtra :: CompOption
-- compExtra = CompOption 0 -- no longer needed, pcre2 is always strict in this way

#enum CompOption,CompOption, \
  compAllowEmptyClass = PCRE2_ALLOW_EMPTY_CLASS, \
  compAltBSUX = PCRE2_ALT_BSUX, \
  compAltExtendedClass = PCRE2_ALT_EXTENDED_CLASS, \
  compAltVerbnames = PCRE2_ALT_VERBNAMES, \
  compAnchored = PCRE2_ANCHORED, \
  compAutoCallout = PCRE2_AUTO_CALLOUT, \
  compCaseless = PCRE2_CASELESS, \
  compDollarEndOnly = PCRE2_DOLLAR_ENDONLY, \
  compDotAll = PCRE2_DOTALL, \
  compDupNames = PCRE2_DUPNAMES, \
  compEndAnchored = PCRE2_ENDANCHORED, \
  compExtended = PCRE2_EXTENDED, \
  compExtendedMore = PCRE2_EXTENDED_MORE, \
  compFirstLine = PCRE2_FIRSTLINE, \
  compLiteral = PCRE2_LITERAL, \
  compMatchUnsetBackref = PCRE2_MATCH_UNSET_BACKREF, \
  compMultiline = PCRE2_MULTILINE, \
  compNeverBackslashC = PCRE2_NEVER_BACKSLASH_C, \
  compNoAutoCapture = PCRE2_NO_AUTO_CAPTURE, \
  compNoAutoPossess = PCRE2_NO_AUTO_POSSESS, \
  compNoDotstarAnchor = PCRE2_NO_DOTSTAR_ANCHOR, \
  compNoUTFCheck = PCRE2_NO_UTF_CHECK, \
  compUngreedy = PCRE2_UNGREEDY, \
  compUTF = PCRE2_UTF

#enum MatchOption,MatchOption, \
  matchAnchored = PCRE2_ANCHORED, \
  matchCopyMatchedSubject = PCRE2_COPY_MATCHED_SUBJECT, \
  matchDisableRecurseLoopCheck = PCRE2_DISABLE_RECURSELOOP_CHECK, \
  matchEndAnchored = PCRE2_ENDANCHORED, \
  matchNotBOL = PCRE2_NOTBOL, \
  matchNotEOL = PCRE2_NOTEOL, \
  matchNotEmpty = PCRE2_NOTEMPTY, \
  matchNotEmptyAtStart = PCRE2_NOTEMPTY_ATSTART, \
  matchNoUTFCheck = PCRE2_NO_UTF_CHECK, \
  matchPartialHard = PCRE2_PARTIAL_HARD, \
  matchPartialSoft = PCRE2_PARTIAL_SOFT

retUnknownNode :: ReturnCode
retUnknownNode = ReturnCode minBound -- never returned by pcre2

#enum ReturnCode,ReturnCode, \
  retNoMatch = PCRE2_ERROR_NOMATCH, \
  retPartial = PCRE2_ERROR_PARTIAL, \
  retNull = PCRE2_ERROR_NULL, \
  retBadOption = PCRE2_ERROR_BADOPTION, \
  retBadMagic = PCRE2_ERROR_BADMAGIC, \
  retNoMemory = PCRE2_ERROR_NOMEMORY, \
  retNoSubstring = PCRE2_ERROR_NOSUBSTRING

-- Comment out most of these to avoid unused binding warnings

-- PCRE2_INFO_FIRSTCHAR is deprecated, use PCRE2_INFO_FIRSTBYTE instead.
#enum InfoWhat,InfoWhat, \
  PCRE2_INFO_CAPTURECOUNT
{-
  PCRE2_INFO_BACKREFMAX, \
  PCRE2_INFO_DEFAULT_TABLES, \
  PCRE2_INFO_FIRSTBYTE, \
  PCRE2_INFO_FIRSTCHAR, \
  PCRE2_INFO_FIRSTTABLE, \
  PCRE2_INFO_LASTLITERAL, \
  PCRE2_INFO_NAMECOUNT, \
  PCRE2_INFO_NAMEENTRYSIZE, \
  PCRE2_INFO_NAMETABLE, \
  PCRE2_INFO_OPTIONS, \
  PCRE2_INFO_SIZE, \
  PCRE2_INFO_STUDYSIZE
-}
#enum ConfigWhat,ConfigWhat, \
  PCRE2_CONFIG_VERSION
{-
  PCRE2_CONFIG_UTF8
  PCRE2_CONFIG_UNICODE_PROPERTIES, \
  PCRE2_CONFIG_NEWLINE, \
  PCRE2_CONFIG_LINK_SIZE, \
  PCRE2_CONFIG_POSIX_MALLOC_THRESHOLD, \
  PCRE2_CONFIG_MATCH_LIMIT, \
  PCRE2_CONFIG_MATCH_LIMIT_RECURSION, \
  PCRE2_CONFIG_STACKRECURSE
-}
