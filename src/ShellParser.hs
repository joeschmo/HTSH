-- | This module is used to define all the 
-- parsing needed to process a user's given
-- input to the shell
module ShellParser (parseExpr) where

import System
import System.IO

import Control.Monad.Error

import Data.List

import ShellTypes

-- | Checks if the expression is backgrounded
-- and is well-formed (i.e. no '&' anywhere else
-- but the end. 
parsebg :: String -> (String, Bool, Maybe String)
parsebg [] = ([], False, Nothing)
parsebg ('&':xs) = case null xs of
                       True  -> (parsed, True, err)
                       False -> (parsed, bg, Just xs)
    where (parsed, bg, err) = parsebg xs
parsebg (x:xs) = (x:parsed, bg, err)
    where (parsed, bg, err) = parsebg xs

-- | Takes a char and sees if it delimits the expression once
parseinfix :: Char -> String -> (String, Maybe String)
parseinfix c [] = ([], Just [])
parseinfix c (x:xs) = case c == x of
                           False -> (x:cmd, cmd2)
                           True  -> case elem x xs || null xs of
                                         True  -> ([], Nothing)
                                         False -> ([], Just xs)
    where (cmd, cmd2) = parseinfix c xs

-- | Checks if the expression is well-formed for piping
parsepipe :: String -> (String, Maybe String)
parsepipe = parseinfix '|'

-- | Checks if the expression is well-formed for redirection
parseredirout :: String -> (String, Maybe String)
parseredirout = parseinfix '>'

-- | Parses the given expression and checks for pipe, redirect and backgrounding
parseExpr :: String -> (Maybe String, Maybe CmdType, Bool)
parseExpr expr =
    let (exp, bg, err) = parsebg expr
        (cmd1, cmd2) = parsepipe exp
        (cmd, file) = parseredirout exp
    in
        case err of
             Just _ -> (Nothing, Nothing, False)
             Nothing -> case (cmd2, file) of
                             (Nothing, _) -> (Nothing, Nothing, False)
                             (_, Nothing) -> (Nothing, Nothing, False)
                             (Just [], Just []) -> (Just exp, Just None, bg)
                             (Just xs, Just []) -> let 
                                                    pipe = words xs
                                                   in
                                                    (Just cmd1, Just (Pipe (head pipe) (tail pipe)), bg)
                             (Just [], Just ys) -> let
                                                    file = (concat . words) ys
                                                   in
                                                    (Just cmd, Just (Redir file), bg)
                             (_, _) -> (Nothing, Nothing, False)

                                                        
