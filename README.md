**ListZipper.hs**, **TreeZipper.hs** y **BinaryTreeZipper.hs** son módulos que implementan los correspondientes zippers. `BinaryTreeZipper` y `TreeZipper` tienen bastantes diferencias además de la cantidad de hijos. `TreeZipper` usa los árboles definidos en `Data.Tree` que es parte de la biblioteca estándar. Esta estructura, a diferencia de los árboles binarios que implementé en `BinaryTreeZipper`, no admite árboles vacíos. Otra particularidad es que utiliza el zipper de listas definido en `ListZipper` para como parte del  `Crumb`.

**list-zipper.hs**, **binary-tree-zipper.hs** y **tree-zipper.hs** son programas que aceptan comandos de la entrada estándar y muestran paso a paso como se modifican los zippers. Los tres programas trabajan con zippers de números enteros. Los comandos aceptados por cada programa son:

##### list-zipper.hs

* `new x1 x2 x3 .. xN`: recibe una lista de enteros como parámetro y crea una nueva lista con esos valores, reemplazando cualquier lista preexistente.
*  `put x`: recibe un entero como parámetro el cual reemplaza al valor en la posición actual. Se asume que la lista no está vacía.
* `insert before x`: recibe un entero como parámetro y lo inserta antes de la posición actual. Se asume que la lista no está vacía.
* `insert after x`: recibe un entero como parámetro y lo inserta después de la posición actual. Se asume que la lista no está vacía.
* `next`: mueve el foco de atención una posición hacia adelante. Se asume que la lista no está vacía y que el foco no está al final de la lista.
* `prev`: mueve el foco de atención una posición hacia atrás. Se asume que la lista no está vacía y que el foco no está al principio de la lista.
* `delete`: elimina el nodo en la posición actual y mueve el foco a la posición siguiente. Si el foco estaba en la última posición lo mueve hacia atrás. Si la lista tenía un solo nodo, pasa a estar vacía.


##### binary-tree-zipper.hs

* `new x1 x2 x3 .. xN`: recibe una lista de enteros como parámetro y crea un  nuevo árbol con esos valores, reemplazando cualquier árbol preexistente. Se construye de forma tal que el orden de los números en la lista ingresada sea el de un recorrido por niveles.
*  `put x`: recibe un entero como parámetro el cual reemplaza al valor en la raíz del subárbol actual. Se asume que el árbol no está vacío.
* `insert left x`: recibe un entero como parámetro y lo inserta como una hoja, la cual pasa a ser el hijo izquierdo del nodo actual. Se asume que el árbol no está vacío y que el nodo actual no tiene un hijo izquierdo.
* `insert rigth x`: recibe un entero como parámetro y lo inserta como una hoja, la cual pasa a ser el hijo derecho del nodo actual. Se asume que el árbol no está vacío y que el nodo actual no tiene un hijo derecho.
* `left`: mueve el foco de atención al hijo izquierdo del nodo actual. Se asume que el árbol no está vacío y el hijo izquierdo existe.
* `right`: mueve el foco de atención al hijo derecho del nodo actual. Se asume que el árbol no está vacío y el hijo derecho existe.
* `up`: mueve el foco de atención al padre del nodo actual. Se asume que el árbol no está vacío y el hijo derecho existe y que el nodo actual no es la raíz.
* `sibling`: mueve el foco de atención al hermano del nodo actual, asumiendo que existe.
* `delete`: elimina el subárbol actual y mueve el foco al nodo padre si este existe.

##### tree-zipper.hs

* `new x1 x2 x3 .. xN`: recibe una lista de enteros como parámetro y crea un nuevo árbol de a lo sumo dos niveles con esos valores, reemplazando cualquier árbol preexistente. El primer número ocupa la raíz mientras que los demás son  sus hijos.
*  `put x`: recibe un entero como parámetro el cual reemplaza al valor en la raíz del subárbol actual.
* `insert x at child i`: recibe dos enteros como parámetro. El primero es el valor que ocupará la raíz de una nueva hoja, la cual pasara a ocupar la posición indicada por el segundo en la lista de hijos.
* `child i`: recibe un entero como parámetro y mueve el foco de atención al `i`-ésimo hijo del nodo actual. Se asume que el nodo actual tiene al menos `i` hijos.
* `left sibling`: mueve el foco de atención al hermano izquierdo del nodo actual. Se asume que el hermano izquierdo existe.
* `right sibling`: mueve el foco de atención al hermano derecho del nodo actual. Se asume que el hermano derecho existe.
* `up`: mueve el foco de atención al padre del nodo actual. Se asume que el nodo actual no es la raíz.
* `delete`: elimina el subárbol actual y mueve el foco al nodo padre si este existe.

Los tres módulos correspondientes a las tres estructuras implementan además una función `get` que devuelve el valor del elemento actual. Sin embargo ésta no es incluida como comando ya que la ejecución de cada uno de ellos muestra el estado actual de la estructura, lo cual incluye el valor del elemento actual. Además, de esta forma se mantiene la coherencia de que todos los comandos aplican una transformación al zipper actual.

Los módulos **Command.hs**, y **Highlighted.hs** abstraen funciones, tipos, y clases utilizados por todos los otros módulos y programas. Los comentarios en el código explican para que se utiliza cada uno.

Para ejecutar el programa es necesario instalar la biblioteca [`Data.Tree.Pretty`](https://hackage.haskell.org/package/pretty-tree-0.1.0.0/docs/Data-Tree-Pretty.html)

    cabal install pretty-tree

Usé la versión 7.10.3 de GHC.