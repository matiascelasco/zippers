import qualified ListZipper as LZ
import qualified Command

main :: IO (LZ.Zipper Int)
main = do
  Command.readAllAndRun LZ.commandParser LZ.empty
