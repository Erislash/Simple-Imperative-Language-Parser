_Este trabajo práctico fue hecho como parte del curso de [Análisis de Lenguajes de Programación](https://dcc.fceia.unr.edu.ar/es/lcc/r313) de la carrera Licenciatura en Ciencias de la Computación de la Universidad Nacional de Rosario._

Nos pidieron programar un parser y evaluadores para un _lenguaje imperativo simple_ que consta de comandos de declaración y asignación de variables, [composición sequencial](https://homepages.inf.ed.ac.uk/stg/NOTES/node86.html#:~:text=In%20imperative%20programming%20it%20is,into%20a%20single%20compound%20expression.), ejecución condicional, un comando de bucle.

El trabajo práctico fue realizado en Haskell. Se puso a nuestra disposición un esqueleto inicial del parser que tuvimos que completar (el mismo se encuentra en el archivo`inicial.rar`) que incluía el pretty printer, las dependencias usadas por el proyecto y el ejecutable, definido en `app/Main.hs`. Además, hicimos uso de la librería [Parsec](https://hackage.haskell.org/package/parsec).

Los ejercicios 1 y 2 consistieron en extender las sintaxis abstracta y concreta de LIS, para incluír **asignaciones de variables como expresiones enteras**, de la forma `x = e` y el **operador** "`,`" para escribir una secuencia de expresiones enteras. La extensión se hizo tanto en la gramática dada (en forma BNF) como en la representación de las reglas en Haskell (en el archivo `AST.hs`).

Para el ejercicio 3 tuvimos que implementar el parser, haciendo uso de la biblioteca Parsec.