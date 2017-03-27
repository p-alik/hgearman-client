{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception (Exception, IOException, catch, throwIO)
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import Data.Typeable     (Typeable)
import qualified Network.Gearman.Client as C
import Network.Gearman.Internal (Function, GearmanClient, Port)
import Network.Socket (HostName)

data ConnectException = ConnectException HostName Port IOException
    deriving (Show, Typeable)
instance Exception ConnectException

main :: IO ()
main = do
    tryConn `catch` \e -> throwIO (ConnectException host port e)
    where tryConn = connect >>= either (\e -> putStrLn $ B.unpack e) (\c -> sendJob c >>= \v -> putStrLn v)
          connect = C.connectGearman (B.pack "i") host port
          host = "localhost"::HostName
          port =  4730::Port

-- sendJob :: GearmanClient -> IO Bool
sendJob c = do
  (res, _) <- flip S.runStateT c $ do
    res' <- C.submitJob func arg
    case res' of
      Left _ -> undefined
      Right res'' -> do
        return (B.unpack res'')
        -- return True
  return res
  where func = (B.pack "foo")::Function
        arg = B.pack "bar"

