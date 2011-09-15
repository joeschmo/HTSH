module ShellSig ( installIntHandler
                , installTStpHandler
                , installQuitHandler
                ) where

import System.Posix.Signals

installIntHandler h = installHandler sigINT h Nothing
installTStpHandler h = installHandler sigTSTP h Nothing
installQuitHandler h = installHandler sigQUIT h Nothing
