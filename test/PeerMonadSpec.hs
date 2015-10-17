module PeerMonadSpec where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Monad.Error.Class
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Network.BitTorrent.Bencoding
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.PeerMonad
import Network.BitTorrent.PWP
import Network.BitTorrent.Types
import System.Directory
import System.IO
import System.Timeout

import Test.Hspec
import SpecHelper

spec :: SpecWith ()
spec = do
  tmpdir <- runIO getTemporaryDirectory
  state <- runIO $ newClientState tmpdir testMeta 9999
  let addr = testAddr [1, 0, 0, 127] 9999
      peerId = B.replicate (fromEnum '1') 20
      bf = BF.newBitField 4
      pData = newPeer bf addr peerId

  describe "handlePWP" $ do
    describe "for Have message" $ do
      it "properly adjust the peer bitfield" $ do
        let exp = handlePWP (Have 2) *> getPeerData
        pData' <- runPeerMonad state pData stdout exp
        BF.get (peerBitField pData') 2 `shouldBe` True

  describe "PeerMonadIO" $ do
    describe "local state manipulation" $ do
      it "respects state updates" $ do
        let pData' = pData { amChoking = False }
            exp = updatePeerData pData' *> getPeerData
        returned <- runPeerMonad state pData stdout exp
        returned `shouldBe` pData'

    describe "exception handling" $ do
      describe "resource allocation" $ do
        it "emits a message when releasing abruptly" $ do
          let exp = do
                      registerCleanup 3 0
                      undefined
          runPeerMonad state pData stdout exp `shouldThrow` anyErrorCall
          res <- timeout 1000 $ readChan (sharedMessages state)
          res `shouldBe` Just WakeUp

