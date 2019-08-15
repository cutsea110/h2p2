import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import Data.Bits (xor)
import System.Environment (getArgs)

encrypt :: B.ByteString -> B.ByteString -> B.ByteString
encrypt key plain = L.toStrict $ Builder.toLazyByteString $ go key plain
  where
    keyLength = B.length key
    go k0 b
      | B.null b  = mempty
      | otherwise =
        let (b0, bn) = B.splitAt keyLength b
            r0       = mconcat $ map Builder.word8 $ B.zipWith xor k0 b0
            in r0 `mappend` go b0 bn

decrypt :: B.ByteString -> B.ByteString -> B.ByteString
decrypt key plain = go key plain
  where
    keyLength = B.length key
    go k0 b | B.null b  = B.empty
            | otherwise =
                let (b0, bn) = B.splitAt keyLength b
                    r0       = B.pack $ B.zipWith xor k0 b0
                    in r0 `B.append` go r0 bn

main :: IO ()
main = do
  [action, keyFile, inputFile] <- getArgs
  key <- B.readFile keyFile
  input <- B.readFile inputFile
  case action of
    "encrypt" -> B.writeFile (inputFile ++ ".out") $ encrypt key input
    "decrypt" -> B.writeFile (inputFile ++ ".out") $ decrypt key input
