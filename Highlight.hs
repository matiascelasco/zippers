module Highlight where

-- Este módulo permite "marcar" o "resaltar" elementos dentro de una estructura 
-- de datos.

-- La necesidad surgió a partir de querer imprimir los Zippers rodeando al valor
-- actual con un par de llaves ({}) para distinguirlo de los demás valores.
-- En un primer intento quise hacer algo como esto:

-- put ("{" ++ show (get zipper) ++ "}") (fmap show zipper)

-- Pero obtenía un dibujo del árbol en el que cada entero estaba rodeado de 
-- comillas. Es decir:

--              "1"                           1   
--               |                            |   
--     Esto     ----      en vez de esto     ---  
--             /    \                       /   \ 
--            "2"  "{3}"                    2  {3}

-- Esto se debe a que, para el tipo String, la implementación de Show no es la 
-- función identidad como cabría de esperase sino que rodea al String con un par
-- de comillas. Para evitar este problema creé un nuevo tipo de dato 
-- MaybeHighlighted que toma un valor y lo rodea con un constructor que nos dice
-- si ese valor está resaltado o no.

data MaybeHighlighted t = Highlighted t | NonHighlighted t

-- MaybeHighlighted es miembro de la clase Show y agrega llaves alrededor de los
-- valores resaltados mientras que los valores no resaltados se imprimen como
-- siempre.

instance (Show t) => Show (MaybeHighlighted t) where
  show (Highlighted x) = "{" ++ show x ++ "}"
  show (NonHighlighted x) = show x

-- La clase Highlightable implementa una función highlightCurrent que marca el 
-- elemento actual como resaltado y a todos los demás como no resaltado. Para 
-- que esto sea posible, el tipo debe ser un funtor, es decir, un tipo que 
-- implemente la función, fmap la cual permite mapear una función a todos sus 
-- elementos. El tipo también debe implementar dos funciones: get y put, las 
-- cuales permiten obtener el valor en la posición actual y escribir un valor en
-- la posición actual respectivamente.

class (Functor z) => Highlightable z where
  get :: (Highlightable z) => z t -> t
  put :: (Highlightable z) => t -> z t -> z t
  highlightCurrent :: (Highlightable z) => z t -> z (MaybeHighlighted t)
  highlightCurrent z = put (Highlighted (get z)) (fmap NonHighlighted z)
