-- | This module is used to define types that are
-- going to be used by the shell. For example,
-- CmdType is used to easily identify what needs
-- to be evaluated by runIO
module ShellTypes (CmdType(None, Pipe, Redir)) where

-- | CmdType allows us to know what type of
-- command is being evaluated
data CmdType = None
             | Pipe String [String]
             | Redir String
             deriving (Show)
