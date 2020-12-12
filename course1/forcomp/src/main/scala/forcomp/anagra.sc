val list = List(('a',1),('b',2),('c',3),('c',-1))
val numPairs = List((2, 5), (3, -7), (20, 56))
def func(list: List[(Int,Int)]): List[Int] = {
  for ((a,b) <- list)  yield a * b
}