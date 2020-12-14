val level = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
level.length
level.head.length
level.indexWhere(_ == 'o')
val row = level.indexWhere(_.indexWhere(_ == 'T') != -1)
val col = level(row).indexOf('T')
level(row)(col)
val set = Set("A","B")
set contains("A")