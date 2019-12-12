-- file: ch09/BetterPredicate.hs
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath
                -> Permissions
                -> Maybe Integer
                -> UTCTime
                -> Bool

-- getFileSize :: FilePath -> IO (Maybe Integer)
betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <0 getModificationTime name
          return (p name perms size modified)

safeFileSize :: FilePath -> IO (Maybe Integer)
safeFileSize path = handle (\_ -> return Nothing) $ do
  bracket(openFile path ReadMode) hClose $ \h -> do
  size <- hFileSize h
  return (Just size)
