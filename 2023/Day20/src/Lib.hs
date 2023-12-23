{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (findIndex, intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

data Signal = High | Low deriving (Show, Eq, Ord)

class Module a where
  connections :: a -> [String]
  forward :: (String, Signal) -> a -> Maybe Signal
  nextState :: (String, Signal) -> a -> a
  name :: a -> String

newtype Broadcaster = BroadCaster {bConnections :: [String]} deriving (Show, Eq, Ord)

instance Module Broadcaster where
  connections :: Broadcaster -> [String]
  connections = bConnections

  forward :: (String, Signal) -> Broadcaster -> Maybe Signal
  forward (_, sig) _ = Just sig

  nextState :: (String, Signal) -> Broadcaster -> Broadcaster
  nextState _ = id

  name :: Broadcaster -> String
  name _ = "broadcaster"

data FlipFlop = FlipFlop {fState :: Bool, fConnections :: [String], fName :: String} deriving (Show, Eq, Ord)

instance Module FlipFlop where
  connections :: FlipFlop -> [String]
  connections = fConnections

  forward :: (String, Signal) -> FlipFlop -> Maybe Signal
  forward (_, High) _ = Nothing
  forward (_, Low) (FlipFlop prevState _ _) = Just $ if prevState then Low else High

  nextState :: (String, Signal) -> FlipFlop -> FlipFlop
  nextState (_, High) f = f
  nextState (_, Low) f@(FlipFlop prevState _ _) = f {fState = not prevState}

  name :: FlipFlop -> String
  name = fName

data Conjunction = Conjunction {cState :: M.Map String Signal, cConnections :: [String], cName :: String} deriving (Show, Eq, Ord)

instance Module Conjunction where
  connections :: Conjunction -> [String]
  connections = cConnections

  forward :: (String, Signal) -> Conjunction -> Maybe Signal
  forward (name, sig) (Conjunction prevState _ _) = Just $ if all (== High) (M.insert name sig prevState) then Low else High

  nextState :: (String, Signal) -> Conjunction -> Conjunction
  nextState (name, sig) c@(Conjunction prevState _ _) = c {cState = M.insert name sig prevState}

  name :: Conjunction -> String
  name = cName

data Mod = ModB Broadcaster | ModF FlipFlop | ModC Conjunction deriving (Show, Eq, Ord)

instance Module Mod where
  connections :: Mod -> [String]
  connections (ModB b) = connections b
  connections (ModF f) = connections f
  connections (ModC c) = connections c

  forward :: (String, Signal) -> Mod -> Maybe Signal
  forward (name, sig) (ModB b) = forward (name, sig) b
  forward (name, sig) (ModF f) = forward (name, sig) f
  forward (name, sig) (ModC c) = forward (name, sig) c

  nextState :: (String, Signal) -> Mod -> Mod
  nextState (name, sig) (ModB b) = ModB $ nextState (name, sig) b
  nextState (name, sig) (ModF f) = ModF $ nextState (name, sig) f
  nextState (name, sig) (ModC c) = ModC $ nextState (name, sig) c

  name :: Mod -> String
  name (ModB b) = name b
  name (ModF f) = name f
  name (ModC c) = name c

type Modules = M.Map String Mod

type Bus = M.Map String [(String, Signal)]

type State = (Modules, Bus)

-- >>> A.parseOnly parseBroadcaster "broadcaster -> gz, xg, cd, sg"
parseBroadcaster :: A.Parser Mod
parseBroadcaster = ModB . BroadCaster . map BSC.unpack <$> ("broadcaster -> " *> (A.takeWhile1 A.isAlpha_ascii `A.sepBy` ", "))

-- >>> A.parseOnly parseFlipFlop "%gj -> zd, pm"
-- Right (ModF (FlipFlop {fState = False, fConnections = ["zd","pm"], fName = "gj"}))
parseFlipFlop :: A.Parser Mod
parseFlipFlop = do
  n <- "%" *> A.takeWhile1 A.isAlpha_ascii <* " -> "
  cs <- A.takeWhile1 A.isAlpha_ascii `A.sepBy` ", "
  return $ ModF $ FlipFlop False (map BSC.unpack cs) (BSC.unpack n)

-- >>> A.parseOnly parseConjunction "&bd -> gm, gz, fh, sv"
-- Right (ModC (Conjunction {cState = fromList [("fh",Low),("gm",Low),("gz",Low),("sv",Low)], cConnections = ["gm","gz","fh","sv"], cName = "bd"}))
parseConjunction :: A.Parser Mod
parseConjunction = do
  n <- "&" *> A.takeWhile1 A.isAlpha_ascii <* " -> "
  cs <- map BSC.unpack <$> A.takeWhile1 A.isAlpha_ascii `A.sepBy` ", "
  return $ ModC $ Conjunction M.empty cs (BSC.unpack n)

parseModule :: A.Parser Mod
parseModule = parseBroadcaster <|> parseFlipFlop <|> parseConjunction

parseModules :: A.Parser Modules
parseModules = M.fromList . map (\m -> (name m, m)) <$> parseModule `A.sepBy1` A.endOfLine

-- >>> loadInput "example1.txt"
-- fromList [("a",ModF (FlipFlop {fState = False, fConnections = ["b"], fName = "a"})),("b",ModF (FlipFlop {fState = False, fConnections = ["c"], fName = "b"})),("broadcaster",ModB (BroadCaster {bConnections = ["a","b","c"]})),("c",ModF (FlipFlop {fState = False, fConnections = ["inv"], fName = "c"})),("inv",ModC (Conjunction {cState = fromList [("a",Low)], cConnections = ["a"], cName = "inv"}))]

-- >>> loadInput "example2.txt"
-- fromList [("a",ModF (FlipFlop {fState = False, fConnections = ["inv","con"], fName = "a"})),("b",ModF (FlipFlop {fState = False, fConnections = ["con"], fName = "b"})),("broadcaster",ModB (BroadCaster {bConnections = ["a"]})),("con",ModC (Conjunction {cState = fromList [], cConnections = ["output"], cName = "con"})),("inv",ModC (Conjunction {cState = fromList [], cConnections = ["b"], cName = "inv"}))]
loadInput :: [Char] -> IO Modules
loadInput = (fromRight M.empty . A.parseOnly parseModules <$>) . BSC.readFile . ("src/" ++)

step :: State -> State
step (mods, bus) = (mods', bus')
  where
    mods' = M.map nextMod mods
    bus' = M.fromListWith (++) $ map (second pure) $ concatMap forwardSignals $ M.toList bus
    forwardSignals (recipient, signals) = concatMap (forwardSignal . (recipient,)) signals
    forwardSignal (recipient, (sender, sig))
      | M.member recipient mods =
          let m = mods M.! recipient
           in case forward (sender, sig) m of
                Nothing -> []
                Just sig' -> (,(recipient, sig')) <$> connections m
      | otherwise = []
    nextMod m = case M.lookup (name m) bus of
      Nothing -> m
      Just signals -> foldl (flip nextState) m signals

initialize :: Modules -> State
initialize m = (m', M.empty)
  where
    m' = M.map (\case c@(ModC _) -> updateConjunction c; o -> o) m
    updateConjunction (ModC c) =
      let inputs = S.fromList $ map name $ M.elems $ M.filter (elem (name c) . connections) m
       in ModC $ c {cState = M.fromSet (const Low) inputs}

pushButton :: State -> State
pushButton (mod, bus) = (mod, M.insert "broadcaster" [("button", Low)] bus)

showSignal :: (String, [(String, Signal)]) -> String
showSignal (recipient, [(sender, sig)]) = sender ++ " -" ++ show sig ++ "-> " ++ recipient

--- >>> map debugOutput . pushMany 4 <$> loadInput "example2.txt"
-- ["button -Low-> broadcaster\nbroadcaster -Low-> a\na -High-> con\na -High-> inv\ninv -Low-> b\ncon -High-> output\nb -High-> con\ncon -Low-> output","button -Low-> broadcaster\nbroadcaster -Low-> a\na -Low-> con\na -Low-> inv\ninv -High-> b\ncon -High-> output","button -Low-> broadcaster\nbroadcaster -Low-> a\na -High-> con\na -High-> inv\ninv -Low-> b\ncon -Low-> output\nb -Low-> con\ncon -High-> output","button -Low-> broadcaster\nbroadcaster -Low-> a\na -Low-> con\na -Low-> inv\ninv -High-> b\ncon -High-> output"]
debugOutput :: [State] -> [Char]
debugOutput = intercalate "\n" . map showSignal . concatMap (M.toList . snd)

steps :: State -> [State]
steps s = map snd $ takeWhile (\(c, _) -> not $ M.null $ snd c) $ zip (s : steps') steps'
  where
    steps' = iterate step s

-- >>> map countPulses . pushMany 1 <$> loadInput "example1.txt"
-- [(8,4)]
countPulses :: [State] -> (Int, Int)
countPulses ss = (lows, highs)
  where
    lows = length $ filter (== Low) signals
    highs = length $ filter (== High) signals
    signals = concatMap (map snd) $ concatMap (M.elems . snd) ss

pushForever :: Modules -> [[State]]
pushForever s =
  let initial = steps $ pushButton $ initialize s
   in iterate (steps . pushButton . last) initial

pushMany :: Int -> Modules -> [[State]]
pushMany count = take count . pushForever

-- >>> countMany 1000 <$> loadInput "example1.txt"
-- (8000,4000)

-- >>> countMany 1000 <$> loadInput "example2.txt"
-- (4250,2750)
countMany :: Int -> Modules -> (Int, Int)
countMany n m = foldl1 (\(l, h) (l', h') -> (l + l', h + h')) (map countPulses $ pushMany n m)

-- >>> part1 <$> loadInput "example1.txt"
-- 32000000

-- >>> part1 <$> loadInput "example2.txt"
-- 11687500
part1 :: Modules -> Int
part1 = uncurry (*) . countMany 1000

-- >>> pressesUntilSignalToModuleMatching "con" (\(sender, sig) -> sender == "a" && sig == Low) <$> loadInput "example2.txt"
-- 2
pressesUntilSignalToModuleMatching :: String -> ((String, Signal) -> Bool) -> Modules -> Int
pressesUntilSignalToModuleMatching mName predicate = maybe (-1) (+ 1) . findIndex signalSent . pushForever
  where
    signalSent :: [State] -> Bool
    signalSent = any (\(_, bus) -> any predicate (fromMaybe [] $ M.lookup mName bus))

part2 :: Modules -> Int
part2 ms =
  -- Inspection of the input data shows that "dn" module outputs to rx and is a Conjunction
  let (ModC lastHop) = snd $ M.findMin $ M.filter (elem "rx" . connections) ms
      -- Therefore the previous hops must all have sent a High signal to "dn" for it to send Low
      previousHops = map name $ M.elems $ M.filter (elem (name lastHop) . connections) ms

      -- We can find the indices when they send a High signal
      indices = map (\m -> pressesUntilSignalToModuleMatching (name lastHop) (\(sender, sig) -> sender == m && sig == High) ms) previousHops
   in -- And use LCM to find the first time they all send a High signal in the same cycle
      foldl1 lcm indices
