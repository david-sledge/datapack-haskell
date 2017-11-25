import Control.Exception.Safe (Exception, MonadThrow, SomeException, throwM)
import Control.Monad          (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable          (TypeRep, Typeable, typeRep)
import Text.Read              (readMaybe)

data ReadException = ReadException String TypeRep
  deriving (Typeable)

instance Show ReadException where
  show (ReadException s typ) = concat
    [ "Unable to parse as "
    , show typ
    , ": "
    , show s
    ]

instance Exception ReadException

readM :: (MonadThrow m, Read a, Typeable a) => String -> m a
readM s = res
  where
    res =
      case readMaybe s of
        Just x -> return x
        Nothing -> throwM $ ReadException s (typeRep res)

readLine1 :: (MonadIO m, MonadThrow n, Read a, Typeable a) => m (n a)
readLine1 = fmap readM (liftIO getLine)

-- Without the usage of liftIO here, we'd need both MonadIO and
-- MonadThrow constraints.
readLine2 :: (MonadIO m, Read a, Typeable a) => m a
readLine2 = liftIO (join readLine1)

main :: IO ()
main = do
  putStrLn "Enter an Int (non-runtime exception)"
  res1 <- readLine1
  print (res1 :: Either SomeException Int)
  putStrLn "Enter an Int (runtime exception)"
  res2 <- readLine2
  print (res2 :: Int)
