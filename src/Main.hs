module Main where
import System
import System.Console.Readline
import System.Exit
import System.IO
import System.Posix.Signals

import ShellParser
import ShellEval
import ShellSig

flushStr str = putStr str >> hFlush stdout

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

main = do
    installIntHandler Ignore
    installTStpHandler Ignore
    installQuitHandler Ignore
    flushStrLn "Welcome to the Haskell Tiny Shell!"
    runShell
