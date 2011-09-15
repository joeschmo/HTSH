module ShellSig ( installIntHandler
                , installTStpHandler
                , installQuitHandler
                ) where

import System.Posix.Signals

-- | Installs sigINT handler
installIntHandler :: Handler -> IO Handler
installIntHandler h = installHandler sigINT h Nothing

-- | Installs sigTSTP handler
installTStpHandler :: Handler -> IO Handler
installTStpHandler h = installHandler sigTSTP h Nothing

-- | Installs sigQUIT handler
installQuitHandler :: Handler -> IO Handler
installQuitHandler h = installHandler sigQUIT h Nothing
