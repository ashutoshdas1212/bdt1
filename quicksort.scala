object CombinedQuickSort {

  def functionalSort(a: List[Int]): List[Int] = {
    if (a.length < 2)
      a
    else {
      val pivot = a(a.length / 2)
      functionalSort(a.filter(_ < pivot)) :::
        a.filter(_ == pivot) :::
        functionalSort(a.filter(_ > pivot))
    }
  }

  def imperativeSort(a: Array[Int]) {

    def swap(i: Int, j: Int) {
      val t = a(i); a(i) = a(j); a(j) = t
    }

    def sort1(l: Int, r: Int) {
      val pivot = a((l + r) / 2)
      var i = l
      var j = r
      while (i <= j) {
        while (a(i) < pivot) i += 1
        while (a(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort1(l, j)
      if (i < r) sort1(i, r)
    }

    if (a.length > 0)
      sort1(0, a.length - 1)
  }

  def main(args: Array[String]) {
    val xs = List(6, 2, 8, 5, 1)
    val arr = Array(6, 5, 2, 1, 8)

    println("Functional QuickSort:")
    println(xs)
    val sortedList = functionalSort(xs)
    println(sortedList)

    println("\nImperative QuickSort:")
    println(arr.mkString(", "))
    imperativeSort(arr)
    println(arr.mkString(", "))
  }
}
