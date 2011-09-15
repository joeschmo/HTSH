module ShellParser (parseExpr) where

import System
import System.IO

import Control.Monad.Error

import Data.List

import ShellTypes

parsebg [] = ([], False, Nothing)
parsebg ('&':xs) = case null xs of
                       True  -> (parsed, True, err)
                       False -> (parsed, bg, Just xs)
    where (parsed, bg, err) = parsebg xs
parsebg (x:xs) = (x:parsed, bg, err)
    where (parsed, bg, err) = parsebg xs

parseinfix c [] = ([], Just [])
parseinfix c (x:xs) = case c == x of
                           False -> (x:cmd, cmd2)
                           True  -> case elem x xs || null xs of
                                         True  -> ([], Nothing)
                                         False -> ([], Just xs)
    where (cmd, cmd2) = parseinfix c xs

parsepipe = parseinfix '|'
parseredirout = parseinfix '>'

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
                             (Just [], Just []) -> (Just expr, Just None, bg)
                             (Just xs, Just []) -> let 
                                                    pipe = words xs
                                                   in
                                                    (Just cmd1, Just (Pipe (head pipe) (tail pipe)), bg)
                             (Just [], Just ys) -> let
                                                    file = (concat . words) ys
                                                   in
                                                    (Just cmd, Just (Redir file), bg)
                             (_, _) -> (Nothing, Nothing, False)

                                                        
