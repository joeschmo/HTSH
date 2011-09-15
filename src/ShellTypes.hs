module ShellTypes (CmdType(None, Pipe, Redir)) where

data CmdType = None
             | Pipe String [String]
             | Redir String
             deriving (Show)
