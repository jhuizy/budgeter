{-# LANGUAGE OverloadedStrings #-}

module Goals.Model where

import Data.Text

newtype Money = Money { unMoney :: Int } deriving (Show, Eq)
newtype Deadline = Deadline { unDeadline :: Int } deriving (Show, Eq)

data Goal = Goal
  { goalName :: Text
  , goalDesc :: Maybe Text 
  , goalDeadline :: Deadline
  , goalSubGoals :: [Goal]
  , goalTarget :: Money
  } deriving (Eq, Show)

mkNewGoal :: Goal
mkNewGoal = Goal name desc deadline subgoals target
  where
    name = "My New Goal"
    desc = Nothing
    deadline = Dealine 1000
    subgoals = []
    target = Money 100


