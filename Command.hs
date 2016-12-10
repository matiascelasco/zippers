module Command (readAllAndRun, CommandParser) where

-- Este módulo implementa la función readAllAndRun, la cual lee líneas de la 
-- entrada estándar y ejecuta comandos a partir de un valor inicial. 
-- Los comandos son funciones que transforman un valor.
-- Luego de aplicar una transformación, se imprime el nuevo valor.
-- En este caso los comandos transforman zippers pero serviría para cualquier 
-- miembro de la clase Show.
-- readAllAndRun recibe como parámetro el valor inicial (que por lo general 
-- será un zipper vacío) y un parser.
-- El parser es una función que toma un String y devuelve un Maybe.
-- Esto permite ignorar comandos mal ingresados sin necesidad de terminar la 
-- ejecución del programa. Sin embargo, no se manejan otros posibles errores 
-- como podría ser, en el caso de zippers, intentar moverse fuera de los 
-- límites de la estructura.

import Data.Maybe (catMaybes)
import Control.Monad (liftM, foldM)

type Command t = t -> t
type CommandParser t = String -> Maybe (Command t)

applyCommandAndPrint :: (Show t) => t -> Command t -> IO t
applyCommandAndPrint old command = do
  let new = command old
  putStrLn ""
  print new
  putStrLn "--------------------------------------"
  return new

readAllAndRun :: (Show t) => CommandParser t -> t -> IO t
readAllAndRun parser initialValue = do
  putStrLn ""
  print initialValue
  putStrLn "--------------------------------------"
  commands <- liftM (catMaybes . map parser . lines) getContents
  foldM applyCommandAndPrint initialValue commands
  where