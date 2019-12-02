module Day1
    ( calculateFuel
    , calculateFuelOfFuel
    , Mass
    , Fuel
    ) where

type Mass = Integer
type Fuel = Integer

calculateFuel :: Mass -> Fuel
calculateFuel m =
    let m' = subtract 2 (div m 3)  in
        if m' < 0 then 0 else m'

calculateFuelOfFuel :: Fuel -> Fuel
calculateFuelOfFuel f = go f 0
  where go finput ftotal
          | finput <= 0 = ftotal
          | otherwise = go (calculateFuel finput) (ftotal + calculateFuel finput)