module Network.Gearman.ClientSpec (spec) where

import qualified Data.ByteString.Char8 as B
import qualified Network.Gearman.Client as C
import Network.Gearman.Internal (Port)
import Network.Socket (HostName)
import Test.Hspec

spec :: Spec
spec = do
  describe "connectGearman" $ do
      it("connects to gearmand on " ++ host ++ ":" ++ show port) $ do testConnectGearman

testConnectGearman :: IO ()
testConnectGearman = do
    c <- C.connectGearman (B.pack "i") host port
    return ()

host ::  HostName
host = "localhost"

port :: Port
port =  4730
