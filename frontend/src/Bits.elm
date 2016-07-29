module Bits exposing (split)

import Bitwise exposing (and)

test : Int -> Int -> Bool
test a b = a `and` b == a

split : Int -> List Bool
split word8 =
  List.map (\n -> test (2^n) word8) [0..7]
