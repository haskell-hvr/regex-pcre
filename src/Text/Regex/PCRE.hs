{-|
The "Text.Regex.PCRE" module provides a backend for regular
expressions.  If you import this along with other backends, then
you should do so with qualified imports, perhaps renamed for
convenience.

From version 1.0.0.0, this library uses the newer libpcre2, which
supports UTF8-encoded strings by default.  As such, 'configUTF8'
now always returns True.

The regular expression can be provided as a 'ByteString'.  The
regular expression and search string are passed as 'CStringLen's
and may contain NUL bytes and do not need to end in a NUL byte.
'ByteString's are searched in place (via unsafeUseAsCStringLen).

A 'String' will be converted into a 'CStringLen' for processing.
Doing this repeatedly will be very inefficient.

The "Text.Regex.PCRE.String", "Text.Regex.PCRE.ByteString", and
"Text.Regex.PCRE.Wrap" modules provide both the high-level interface
exported by this module and medium- and low-level interfaces that
return errors using 'Either' structures.
-}
{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE(getVersion_Text_Regex_PCRE
  ,module Text.Regex.Base
  -- ** Wrap, for '=~' and '=~~', types and constants
  ,module Text.Regex.PCRE.Wrap) where

import Prelude hiding (fail)

import Text.Regex.PCRE.Wrap(
  Regex, CompOption(CompOption), MatchOption(MatchOption),
  (=~), (=~~),
  unusedOffset, getNumSubs, configUTF8, getVersion,
  compBlank, compAnchored, compEndAnchored, compAllowEmptyClass,
  compAltBSUX, compAltExtendedClass, compAltVerbnames,
  compAutoCallout, compCaseless, compDollarEndOnly, compDotAll,
  compDupNames, compExtended, compExtendedMore, compFirstLine,
  compLiteral, compMatchUnsetBackref, compMultiline,
  compNeverBackslashC, compNoAutoCapture, compNoAutoPossess,
  compNoDotstarAnchor, compNoUTFCheck, compUngreedy, compUTF,
  matchBlank, matchAnchored, matchCopyMatchedSubject,
  matchDisableRecurseLoopCheck, matchEndAnchored, matchNotBOL,
  matchNotEOL, matchNotEmpty, matchNotEmptyAtStart,
  matchNoUTFCheck, matchPartialHard, matchPartialSoft)
import Text.Regex.PCRE.String()
import Text.Regex.PCRE.Sequence()
import Text.Regex.PCRE.ByteString()
import Text.Regex.PCRE.ByteString.Lazy()
import Data.Version(Version(..))
import Text.Regex.Base
import qualified Paths_regex_pcre

getVersion_Text_Regex_PCRE :: Version
getVersion_Text_Regex_PCRE = Paths_regex_pcre.version
