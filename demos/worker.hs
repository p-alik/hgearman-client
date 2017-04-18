{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception (Exception, IOException, catch, throwIO)
import qualified Data.ByteString.Char8 as B
import Control.Monad.State
import Data.Typeable     (Typeable)
import qualified Network.Gearman.Client as C
import qualified Network.Gearman.Worker as W
import Network.Gearman.Internal (Function, GearmanClient, Port)
import Network.Socket (HostName)
import           Control.Concurrent
import qualified Control.Monad.State as S

data ConnectException = ConnectException HostName Port IOException
    deriving (Show, Typeable)
instance Exception ConnectException

main :: IO ()
main = do
  c <- connect
  gc <- either (error . B.unpack) (return) c
  work gc
  return ()
  where
    connect = C.connectGearman (B.pack "worker-id") host port `catch` \e -> throwIO (ConnectException host port e)
    host = "localhost"::HostName
    port =  4730::Port

work :: GearmanClient -> IO ()
work gc = do
      (res, _) <- flip S.runStateT gc $ do
          W.registerWorker ((B.pack "reverse")::Function)  (\v -> B.reverse v)
          S.get >>= (\env -> forever $ S.liftIO (W.runWorker env (return ()) >> (threadDelay $ 1000*1000)))
          return ()
      return res
