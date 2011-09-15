-- | This module is used to evaluated the
-- expressions processed by ShellParser.
module ShellEval (runIO) where

import System
import System.Directory
import System.IO
import System.Process

import System.Posix.Process

import Control.Monad
import Control.Concurrent

import ShellTypes

-- | Takes a command and a file and redirects the output of the command to the file
-- before evalutation. Returns pid of evaluated command and file handle.
evalRedir :: FilePath -> [String] -> FilePath -> IO (ProcessHandle, Maybe Handle)
evalRedir cmd args file = do
    fhandle <- openFile file WriteMode
    let fstream = UseHandle fhandle
    (_,_,_,pid) <-
        createProcess (proc cmd args) { std_out=fstream }
    return (pid, Just fhandle)

-- | Evaluates a command. Returns pid of evaluated command and nothing.
evalCommand :: FilePath -> [String] -> IO (ProcessHandle, Maybe Handle)
evalCommand cmd args = do
    (_,_,_,pid) <-
        createProcess (proc cmd args)
    return (pid, Nothing)

-- | Takes two commands and creates a pipe between them before evaluation. Returns
-- pid of second evaluated command and nothing.
evalPiped :: FilePath -> [String] -> FilePath -> [String] -> IO (ProcessHandle, Maybe Handle)
evalPiped cmd1 args1 cmd2 args2 = do
    (_,Just hout,_,_) <-
        createProcess (proc cmd1 args1) { std_out = CreatePipe }
    (_,_,_,pid) <-
        createProcess (proc cmd2 args2) { std_in = UseHandle hout }
    return (pid, Nothing)

-- | Closes handles wrapped in Maybe.
closeHandle :: Maybe Handle -> IO ()
closeHandle Nothing = return ()
closeHandle (Just h) = hClose h >> return ()

-- | Wait for background process to exit.
waitBackground :: FilePath -> ProcessHandle -> Maybe Handle -> IO ()
waitBackground cmd pid handle = do
    putStrLn $ "Backgrounded: "++cmd
    forkIO $ waitForProcess pid >> closeHandle handle >> putStrLn ("Done: "++cmd) >> return ()
    return ()

-- | Wait for process to exit.
wait :: ProcessHandle -> Maybe Handle -> IO ()
wait pid handle = do
    waitForProcess pid
    closeHandle handle
    return ()

-- | Given command, arguments, and file/pipe, evaluates command.
runIO :: FilePath -> [String] -> CmdType -> Bool -> IO ()
runIO cmd args cmdtype bg = do
    (pid,h) <- case cmdtype of
                    None -> evalCommand cmd args
                    Redir file -> evalRedir cmd args file
                    Pipe cmd' args' -> evalPiped cmd args cmd' args'
    case bg of
        True -> waitBackground cmd pid h
        False -> wait pid h
    return ()


