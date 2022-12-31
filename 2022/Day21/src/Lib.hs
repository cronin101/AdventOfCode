{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    evaluate,
    setRootAndHumn,
    search,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlpha)
import Data.Either (fromRight)
import Data.Map qualified as M

data OP = Add | Sub | Mul | Div | Cmp deriving (Show)

data MonkeyAction a = Literal a | Operation String OP String deriving (Show)

data Monkey a = Monkey {name :: String, action :: MonkeyAction a} deriving (Show)

parseOperation :: A.Parser (MonkeyAction a)
parseOperation = do
  leftName <- BSC.unpack <$> A.takeWhile isAlpha <* " "
  op <-
    A.choice
      [ Add <$ "+",
        Sub <$ "-",
        Mul <$ "*",
        Div <$ "/",
        Cmp <$ "="
      ]
  rightName <- BSC.unpack <$> (" " *> A.takeWhile isAlpha)
  return $ Operation leftName op rightName

-- >>> A.parseOnly parseMonkey "dbpl: 5"
-- Right (Monkey {name = "dbpl", action = Literal 5})

-- >>> A.parseOnly parseMonkey "root: pppw + sjmn"
-- Right (Monkey {name = "root", action = Operation "pppw" Add "sjmn"})
parseMonkey :: Integral a => A.Parser (Monkey a)
parseMonkey = Monkey <$> (BSC.unpack <$> A.takeWhile isAlpha <* ": ") <*> A.choice [Literal <$> A.signed A.decimal, parseOperation]

parseMonkeys :: Integral a => A.Parser [Monkey a]
parseMonkeys = A.sepBy1 parseMonkey A.endOfLine

loadInput :: Integral a => String -> IO (M.Map String (MonkeyAction a))
loadInput filename = toMap . fromRight [] . A.parseOnly parseMonkeys <$> BSC.readFile ("src/" ++ filename)
  where
    toMap = M.fromList . map (\m -> (name m, action m))

evaluate :: Integral a => String -> M.Map String (MonkeyAction a) -> a
evaluate name monkeys = case monkeys M.! name of
  Literal n -> n
  Operation leftName op rightName ->
    let left = evaluate leftName monkeys
        right = evaluate rightName monkeys
     in case op of
          Add -> left + right
          Cmp -> left - right
          Sub -> left - right
          Mul -> left * right
          Div -> left `div` right

setRootAndHumn :: a -> M.Map String (MonkeyAction a) -> M.Map String (MonkeyAction a)
setRootAndHumn n m = M.insert "root" (Operation left Cmp right) $ M.insert "humn" (Literal n) m
  where
    Operation left _ right = m M.! "root"

search :: (Integral a, Show a) => M.Map String (MonkeyAction a) -> (a, a) -> a
search m (lo, hi) = case abs (evaluate "root" (setRootAndHumn lo m)) `compare` abs (evaluate "root" (setRootAndHumn hi m)) of
  LT -> search m (lo, mid)
  EQ -> case signum (evaluate "root" $ setRootAndHumn mid m) of 0 -> mid; -1 -> search m (mid, hi); _ -> search m (lo, mid)
  GT -> search m (mid, hi)
  where
    mid = fromInteger $ (toInteger lo + toInteger hi) `div` 2
