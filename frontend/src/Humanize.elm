module Humanize exposing (humanize)

import List exposing (filter, head, reverse)

siPrefixes : List (Float, String)
siPrefixes =
  [ (2^10^1, "KiB")
  , (2^10^2, "MiB")
  , (2^10^3, "GiB")
  , (2^10^4, "TiB")
  ]

humanize : Float -> String
humanize num =
  let
      filtered = filter f siPrefixes
      f (n, _) = (num / n) >= 1
  in case reverse filtered |> head  of
    Just (n, name) -> toString (num / n) ++ name
    Nothing -> toString num ++ "B"
