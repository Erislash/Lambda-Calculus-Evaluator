_Este trabajo práctico fue hecho como parte del curso de Análisis de Lenguajes de Programación de la carrera Licenciatura en Ciencias de la Computación de la Universidad Nacional de Rosario._

Nos pidieron implementar en **Haskell** un evaluador de términos de $\lambda$-cáclulo. 

Además, implementamos un algoritmo (en $\lambda$-cáclulo) para probar el evaluador. El mismo es la función _divisores_ que dado un $n \in \N$ calcule la lista con los divisores de $n$ distintos de $n$.


# Implementación del algoritmo

Implementamos un algoritmo que, dado un número $n \in \N$ nos devuelve una lista con todos sus divisores (distintos a $n$).

Comenzamos definiendo funciones auxiliares:

* $not$

Disponiendo de `false` ($\lambda x \ y. \ y$) y `true` ($\lambda x \ y. \ x$), la definición de `not` fue sencilla:


| $not = \lambda b. \ b \ false \ true$ |`def not = \b . b false true`|
|---|---|


* $resta$


