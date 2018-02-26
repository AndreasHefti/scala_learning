import ppWeek2.{MapReduce, MergeSort, ParallelUtils}

val arr = Array[Int](5,3,7,8,3,24,6,9,1)
MergeSort.parMergeSort(arr, 5)
println(arr.deep.mkString(","))


// map reduce

val collection = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

// sum = reduce(collection, _ + _)
// length = reduce(map(collection, (x: Int) => 1), _ + _)
// avr = sum/length

val sum = collection.reduce(_ + _)
val length = collection.map(_ => 1).reduce(_ + _)
val avr1 = sum/length

def f(sl1: (Int, Int), sl2: (Int, Int)): (Int, Int) =
  (sl1._1 + sl2._1, sl1._2 + sl2._2)



// (sum, length) = reduce(map(collection, (x: Int) => (x, 1), f)
val sumLength = collection.map((x: Int) => (x, 1)).reduce(f(_, _))
val avr2 = sumLength._1/sumLength._2

// rounding error sown in difference of reduce left and
// reduce right for very small double numbers

def associativeFunction(u: Double, v:Double): Double =
  (u + v) / (1.0 + u * v)

def err(lst:List[Double]): Double =
  lst.reduceLeft(associativeFunction) - lst.reduceRight(associativeFunction)

def testAssoc: Double = {
  val r = new scala.util.Random
  val lst = List.fill(400)(r.nextDouble*0.002)
  err(lst)
}

testAssoc


