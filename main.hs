
module Phone where

import Data.List
import Data.Char



main :: [[UserInput]]
main = phoneInputsForConversation conversation

phoneInputsForConversation :: [Message] -> [[UserInput]]
phoneInputsForConversation = map (inputsForMessage phone)

phone :: Phone
phone = Phone [
  ('1', ""),
  ('2', "abc"),
  ('3', "def"),
  ('4', "ghi"),
  ('5', "jkl"),
  ('6', "mno"),
  ('7', "pqrs"),
  ('8', "tuv"),
  ('9', "wxyz"),
  ('*', "^"),
  ('0', "+ _"),
  ('+', ".,")
  ]






-- Data

conversation :: [Message]
conversation = [
  "Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]






-- Types

data Phone = Phone Buttons
type Buttons = [(Digit, String)]
type Digit = Char
type Presses = Int
type UserInput = (Digit, Presses)
type Message = String






-- Functions



-- Calculate the total number of taps of an input chain.
tapCount :: [UserInput] -> Presses
tapCount = foldr ((+) . snd) 0



-- Find the most popular character in a message.
popestChar :: Message -> Char
popestChar =
  -- The value is the second tuple element.
  snd .
  -- Fold the list to the most popular.
  maximumBy (\ (n1,_) (n2,_) -> compare n1 n2) .
  -- Calculate the length of each sub list. We need to track
  -- the value itself too because that is what we will return
  -- at the end.
  map (\ xs@(x:_) -> (length xs, x)) .
  -- group splits the list at every non-equal boundary. The
  -- result is easy to then count how long each sub list is
  -- which is our metric for popularity.
  group .
  -- By sorting we make same characters neighbors which
  -- is great for the next step, group.
  sort



inputsForMessage :: Phone -> String -> [UserInput]
inputsForMessage = concatMap . inputForChar

inputForChar :: Phone -> Char -> [UserInput]
inputForChar (Phone buttons) = go buttons
  where

    go :: Buttons -> Char -> [UserInput]
    -- go [] = This should never happen.
    go ((digit, chars):buttonsRest) x

      -- To write an upper case letter User has to first press the
      -- star button once.
      | isUpper x = ('*', 1) : go buttons (toLower x)

      -- To write a digit user has to repeatedly press a button until its
      -- characters have been cycled through.
      | isDigit x = if x == digit
                    then [(digit, length chars + 1)]
                    else go buttonsRest x

      | otherwise = case elemIndex x chars of
                    Just n  -> [(digit, n + 1)] -- 1-indexed
                    Nothing -> go buttonsRest x

    isDigit :: Char -> Bool
    isDigit = (`elem` map fst buttons)
