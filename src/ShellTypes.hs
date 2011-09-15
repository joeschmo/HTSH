module ShellTypes (CmdType(None, Pipe, Redir)) where

-- | CmdType allows us to know what type of
-- command is being evaluated
data CmdType = None
             | Pipe String [String]
             | Redir String
             deriving (Show)
