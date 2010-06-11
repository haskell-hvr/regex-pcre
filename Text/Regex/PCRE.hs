{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
The "Text.Regex.PCRE" module provides a backend for regular
expressions.  If you import this along with other backends, then
you should do so with qualified imports, perhaps renamed for
convenience.

You will need to have libpcre, from www.pcre.org, to use
this module.  The haskell must be compiled with -DHAVE_PCRE_H and
linked with pcre.  This is the default in the cabal file.

If you do not compile with HAVE_PCRE_H then the functions will still
exist, but using them will create a run time error.  You can test for
the existance of PCRE by checking 'getVersion' which is 'Nothing' if
not compiled with PCRE or 'Just' 'String' if PCRE is present.

Using the provided 'CompOption' and 'ExecOption' values and if
'configUTF8' is True, then you might be able to send UTF8 encoded
ByteStrings to PCRE and get sensible results.  This is currently
untested.

The regular expression can be provided as a 'ByteString', but it will
be copied and a NUL byte appended to make a 'CString' unless such a
byte is already present.  Thus the regular expression cannot contain
an explicit NUL byte. The search string is passed as a 'CStringLen'
and may contain NUL bytes and does not need to end in a NUL
byte. 'ByteString's are searched in place (via unsafeUseAsCStringLen).

A 'String' will be converted into a 'CString' or 'CStringLen' for
processing.  Doing this repeatedly will be very inefficient.

The "Text.Regex.PCRE.String", "Text.Regex.PCRE.ByteString", and
"Text.Regex.PCRE.Wrap" modules provides both the high level interface
exported by this module and medium- and low-level interfaces that
returns error using Either structures.
-}
{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE(getVersion_Text_Regex_PCRE
  ,module Text.Regex.Base
  -- ** Wrap, for '=~' and '=~~', types and constants
  ,module Text.Regex.PCRE.Wrap) where

import Text.Regex.PCRE.Wrap(Regex, CompOption(CompOption), ExecOption(ExecOption), (=~), (=~~),
  unusedOffset, getNumSubs, configUTF8, getVersion,
  compBlank, compAnchored, compAutoCallout, compCaseless,
  compDollarEndOnly, compDotAll, compExtended, compExtra,
  compFirstLine, compMultiline, compNoAutoCapture, compUngreedy,
  compUTF8, compNoUTF8Check,
  execBlank, execAnchored, execNotBOL, execNotEOL, execNotEmpty,
  execNoUTF8Check, execPartial)
import Text.Regex.PCRE.String()
import Text.Regex.PCRE.Sequence()
import Text.Regex.PCRE.ByteString()
import Text.Regex.PCRE.ByteString.Lazy()
import Data.Version(Version(..))
import Text.Regex.Base

getVersion_Text_Regex_PCRE :: Version
getVersion_Text_Regex_PCRE =
  Version { versionBranch = [0,94,2]  -- Keep in sync with regex-pcre.cabal
          , versionTags = ["unstable"]
          }
