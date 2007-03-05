#!/usr/bin/env runhaskell

-- I usually compile this with "ghc --make -o setup Setup.hs"

import Distribution.Simple(defaultMain)
main = do putStrLn msg
          defaultMain

msg = "regex-pcre needs to compile against the libpcre libary from http://www.pcre.org/\n\
      \You might also need to edit the end of the regex-pcre.cabal file to point at\n\
      \the directories where libpcre 'include' and 'lib' have been installed.\n"
