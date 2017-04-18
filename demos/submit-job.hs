{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception (Exception, IOException, catch, throwIO)
import qualified Data.ByteString.Char8 as B
import Data.Typeable     (Typeable)
import qualified Network.Gearman.Client as C
import Network.Gearman.Internal (Function, GearmanClient, GearmanError, Port, withGearman)
import Network.Socket (HostName)

data ConnectException = ConnectException HostName Port IOException
    deriving (Show, Typeable)
instance Exception ConnectException

main :: IO ()
main = do
  c <- C.connectGearman (B.pack "client-id") host port `catch` \e -> throwIO (ConnectException host port e)
  either (error . B.unpack) return c
    >>= submitFooJob
    >>= either(error . B.unpack) (putStrLn . B.unpack)
  return ()
    where
      host = "localhost"::HostName
      port =  4730::Port
      submitFooJob :: GearmanClient -> IO (Either GearmanError B.ByteString)
      submitFooJob gc = withGearman gc $ C.submitJob (B.pack "reverse"::Function) (B.pack "bar")
