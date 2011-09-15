module ShellEval (CmdType
                 , runIO) where
import System
import System.Directory
import System.IO
import System.Process

import System.Posix.Process

import Control.Monad
import Control.Concurrent

import ShellTypes

evalRedir cmd args file = do
    fhandle <- openFile file WriteMode
    let fstream = UseHandle fhandle
    (_,_,_,pid) <-
        createProcess (proc cmd args) { std_out=fstream }
    return (pid, Just fhandle)

evalCommand cmd args = do
    (_,_,_,pid) <-
        createProcess (proc cmd args)
    return (pid, Nothing)

evalPiped cmd1 args1 cmd2 args2 = do
    (_,Just hout,_,_) <-
        createProcess (proc cmd1 args1) { std_out = CreatePipe }
    (_,_,_,pid) <-
        createProcess (proc cmd2 args2) { std_in = UseHandle hout }
    return (pid, Nothing)

closeHandle Nothing = return ()
closeHandle (Just h) = hClose h >> return ()

waitBackground cmd pid handle = do
    putStrLn $ "Backgrounded: "++cmd
    forkIO $ waitForProcess pid >> closeHandle handle >> putStrLn ("Done: "++cmd) >> return ()
    return ()

wait pid handle = do
    waitForProcess pid
    closeHandle handle
    return ()

runIO cmd args cmdtype bg = do
    (pid,h) <- case cmdtype of
                    None -> evalCommand cmd args
                    Redir file -> evalRedir cmd args file
                    Pipe cmd' args' -> evalPiped cmd args cmd' args'
    case bg of
        True -> waitBackground cmd pid h
        False -> wait pid h
    return ()


