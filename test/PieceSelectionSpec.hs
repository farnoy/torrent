module PieceSelectionSpec where

import Data.AEq
import qualified Data.Vector.Unboxed as VU
import Network.BitTorrent.PieceSelection
import qualified Network.BitTorrent.BitField as BF
import Numeric.IEEE
import Test.Hspec

spec :: SpecWith ()
spec = do
  it "gets the most demanded piece" $
    pending
  it "does not return anything after completing" $
    pending
