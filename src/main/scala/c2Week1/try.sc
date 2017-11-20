import c2Week1.Try

val success = Try(1 + 2 == 3)
val fail = Try(1 / 0 == 4)

def div(x: Int)(d: Int): Int = x / d

div(10)(3)

var result1 = for {
  f1 <- Try(div(10)(3))
  f2 <- Try(div(10)(5))
} yield (f1, f2)

var result2 = for {
  f1 <- Try(div(10)(3))
  f2 <- Try(div(10)(0))
} yield (f1, f2)
