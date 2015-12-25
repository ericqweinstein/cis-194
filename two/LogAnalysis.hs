{-# OPTIONS_GHC -Wall #-}

-- See: http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
module LogAnalysis where

import Log

{- EXERCISE 1 -}

-- Parses an individual log message.
--
-- Examples:
--   parseMessage "E 2 562 help help"
--   => LogMessage (Error 2) 562 "help help"
--   parseMessage "I 29 la la la"
--   => LogMessage Info 29 "la la la"
--   parseMessage "This is not in the right format"
--   => Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage s = let logs = words s in
                 case logs of
                   ("I":timestamp:message)       -> LogMessage Info (read timestamp) (unwords message)
                   ("W":timestamp:message)       -> LogMessage Warning (read timestamp) (unwords message)
                   ("E":level:timestamp:message) -> LogMessage (Error (read level)) (read timestamp) (unwords message)
                   _                             -> Unknown s

-- Parses an entire log file at once and returns
-- its contents as a list of LogMessages. (The
-- file is supplied as a \n-delimited string.)
parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

{- EXERCISE 2 -}

-- Inserts a new LogMessage into an existing
-- MessageTree, producing a new MessageTree.
-- Assumes it is given a sorted MessageTree
-- and produces a new sorted MessageTree
-- containing the new LogMessage in addition
-- to the contents of the original MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left msg2 (insert msg1 right)
  | otherwise = Node (insert msg1 left) msg2 right
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert _ tree                      = tree

{- EXERCISE 3 -}

-- Builds up a MessageTree containing the
-- messages in the list by successively
-- inserting the messages into a MessageTree
-- (beginning with a Leaf).
build :: [LogMessage] -> MessageTree
build msgs = foldl (\tree msg -> insert msg tree) Leaf msgs

{- EXERCISE 4 -}

-- Takes a sorted MessageTree and produces
-- a list of all the LogMessages it contains,
-- sorted by timestamp from smallest to biggest
-- (an in-order traversal of the MessageTree).
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

{- EXERCISE 5 -}

-- Takes an unsorted list of LogMessages and
-- returns a list of the messages corresponding
-- to any errors with a severity of 50 or
-- greater, sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getLogMessages . filter (isSevere 50)

-- Transforms a list of LogMessages into
-- a list of Strings, where each string is
-- the message associated with the LogMessage.
getLogMessages :: [LogMessage] -> [String]
getLogMessages (LogMessage _ _ msg:msgs) = msg: getLogMessages msgs
getLogMessages _                         = []

-- Checks whether an error is severe or not.
isSevere :: Int -> LogMessage -> Bool
isSevere threshold (LogMessage (Error level) _ _)
  | level > threshold = True
  | otherwise         = False
isSevere _ _ = False
