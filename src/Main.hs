-- | Main module where the interface for 
-- the shell is housed
module Main where
import System
import System.Console.Readline
import System.Exit
import System.IO
import System.Posix.Signals

import ShellParser
import ShellEval
import ShellSig

-- | Prints out a string
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | Prints out a string with a newline
flushStrLn :: String -> IO ()
flushStrLn str = putStrLn str >> hFlush stdout

until_ prompt action = do
    result <- prompt
    case result of
        Nothing   -> exitSuccess
        Just line -> case line of
                        "" -> return ()
                        _  -> do addHistory line
                                 action line
    until_ prompt action

runShell = until_ (readPrompt "htsh$ ") parseAndEval

readPrompt prompt = hFlush stderr >> hFlush stdout >> readline prompt

-- | Takes an expression, parses and evaluates it.
parseAndEval :: String -> IO ()
parseAndEval expr =
    let 
        (rawcmd, rawtype, bg) = parseExpr expr
    in
        case rawcmd of
            Nothing -> hPutStrLn stderr "htsh: syntax error"
            Just st -> let
                            parsed = words st
                            cmd = head parsed
                            args = tail parsed
                       in
                            case rawtype of
                                Nothing -> hPutStrLn stderr "htsh: parse error"
                                           >> return ()
                                Just ct -> runIO cmd args ct bg

main :: IO ()
main = do
    installIntHandler Ignore
    installTStpHandler Ignore
    installQuitHandler Ignore
    flushStrLn "Welcome to the Haskell Tiny Shell!"
    runShell
