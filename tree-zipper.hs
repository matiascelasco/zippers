import qualified TreeZipper as TZ
import qualified Command

main :: IO (TZ.Zipper Int)
main = do
  Command.readAllAndRun TZ.commandParser (TZ.leaf 0)
