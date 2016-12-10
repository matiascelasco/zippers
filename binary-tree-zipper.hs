import qualified BinaryTreeZipper as BTZ
import qualified Command

main :: IO (BTZ.Zipper Int)
main = do
  Command.readAllAndRun BTZ.commandParser BTZ.empty
