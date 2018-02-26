val list1 = List("1", "2", "3", "4", "5")
// map example: map a function to each string in the list
// and returns a new list with the new string objects resulted from the
// function
val mapped = list1.map(_ + "added")

val flatMapped = list1.flatMap(s => s)

val intList = List(1, 2, 3, 4, 5)
val flatMappedInt = intList.flatMap(i => List(i, 1))