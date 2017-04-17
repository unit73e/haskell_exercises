import qualified Data.ByteString.Lazy as L
import System.Environment

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)

main = do
  args <- getArgs
  let path = args !! 0
  let res = isElfFile path
  putStrLn $ show res
