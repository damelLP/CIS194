{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage msg 
    | x == "E" = parseError xs
    | x == "I" = parseInfo xs
    | x == "W" = parseWarning xs
    | otherwise = Unknown msg
    where (x:xs) = words msg

parseError :: [String] -> LogMessage
parseError (a:b:c)= LogMessage (Error $ read a) (read b) $ unwords c
parseError x = Unknown $ unwords x

parseInfo :: [String] -> LogMessage
parseInfo (a:b) = LogMessage Info  (read a) $ unwords b
parseInfo x = Unknown $ unwords x

parseWarning :: [String] -> LogMessage
parseWarning (a:b) = LogMessage Warning  (read a) $ unwords b
parseWarning x = Unknown $ unwords x

parse :: String -> [LogMessage]
parse str = map parseMessage $ lines str

-- Exercise 2
timestampOf :: LogMessage -> TimeStamp
timestampOf (LogMessage _ tS _) = tS
timestampOf _ = 0

insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown _) tree = tree
insert x Leaf = Node Leaf x Leaf
insert x tree
  | timestampOf x <= timestampOf t = Node (insert x l) t r
  | timestampOf x > timestampOf t = Node l t (insert x r)
  where (Node l t r) = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree 
build = foldr insert Leaf 

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l t r) = inOrder l ++ [t] ++ inOrder r

-- Exercise 5
toString :: LogMessage -> String 
toString (LogMessage _ _ str) = str
toString _ = "Unknown"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lst = map toString $ (inOrder . build) lst
