-- Fuel required to launch a given module is based on its mass.
-- Specifically, to find the fuel required for a module, take its mass,
--    divide by three, round down, and subtract 2.
calculateFuel :: Int -> Int
calculateFuel mass = (mass `quot` 3) - 2

--What is the sum of the fuel requirements for all of the modules on your spacecraft
--    when also taking into account the mass of the added fuel?
calculateFuelRec :: Int -> Int
calculateFuelRec = sum . takeWhile (> 0) . drop 1 . iterate calculateFuel

--The Fuel Counter-Upper needs to know the total fuel requirement.
--To find it, individually calculate the fuel needed for the mass of each module (your puzzle input),
--    then add together all the fuel values.
--What is the sum of the fuel requirements for all of the modules on your spacecraft?
main = do
  masses <- map read . lines <$> readFile "./input.txt"
  putStr "Part1: "
  print $ sum $ map calculateFuel masses
  putStr "Part2: "
  print $ sum $ map calculateFuelRec masses
