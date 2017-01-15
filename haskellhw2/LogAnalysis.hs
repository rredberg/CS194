{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

isValidNum :: [Char] -> Bool
isValidNum [] = True
isValidNum (x:xs) = (isDigit x) && isValidNum xs

parseMessage :: String -> LogMessage
parseMessage line
             | x0 == "E" && (isValidNum x1) && (isValidNum x2) = LogMessage (Error (read x1 :: Int)) (read x2 :: Int) (unwords xs)
             | x0 == "I" && (isValidNum x1) = LogMessage Info (read x1 :: Int) (unwords (x2:xs))
             | x0 == "W" && (isValidNum x1) = LogMessage Warning (read x1 :: Int) (unwords (x2:xs))
             | otherwise = Unknown line
             where (x0:x1:x2:xs) = words line

parse :: String -> [LogMessage]
parse "" = []
parse file = (parseMessage (head messages)) : (parse (unlines (tail messages)))
             where messages = lines file

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mTree = mTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage messageTree
              | oldTimeStamp < newTimeStamp = (Node left message (insert logMessage right))
              | oldTimeStamp > newTimeStamp = (Node (insert logMessage left) message right)
              | otherwise = error "Timestamp already exists"
              where (LogMessage _ newTimeStamp _) = logMessage
                    (Node left message right) = messageTree
                    (LogMessage _ oldTimeStamp _) = message

build :: [LogMessage] -> MessageTree
build [] = Leaf
build messages = insert (head messages) (build (tail messages))

getLeftMost :: MessageTree -> LogMessage
getLeftMost messageTree 
            | left == Leaf = logMessage
            | otherwise = getLeftMost left
            where (Node left logMessage _) = messageTree

inOrder :: MessageTree -> [LogMessage]
inOrder messageTree
            | messageTree == Leaf = []
            | left == Leaf = logMessage:[]
            | otherwise = (inOrder left) ++ [logMessage] ++ (inOrder right)
            where (Node left logMessage right) = messageTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = let sortedMessages = inOrder (build messages)
                          in [string | (LogMessage (Error sev) _ string) <- sortedMessages, sev > 50]
--whatWentWrong messages = [string | (LogMessage (Error sev) _ string) <- messages, sev > 50]

