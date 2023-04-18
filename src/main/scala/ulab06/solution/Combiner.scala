package ulab06.solution

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

//1.1
object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = a.foldLeft(0.0)(_ + _)
  override def concat(a: Seq[String]): String = a.foldLeft("")(_ + _)
  override def max(a: List[Int]): Int =
    if (a.isEmpty)
      return Int.MinValue
    var maxVal = a.head
    for (x <- a.tail)
      if (x > maxVal)
        maxVal = x
    maxVal


/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

//1.2
object SumCombiner extends Combiner[Double]:
  override def unit: Double = 0.0
  override def combine(a: Double, b: Double): Double = a + b

object ConcatCombiner extends Combiner[String]:
  override def unit: String = ""
  override def combine(a: String, b: String): String = a + b

object MaxCombiner extends Combiner[Int]:
  override def unit: Int = Int.MinValue
  override def combine(a: Int, b: Int): Int = if (a > b) a else b

object FunctionsImpl2 extends Functions:
  override def sum(a: List[Double]): Double = combiner(a, SumCombiner)
  override def concat(a: Seq[String]): String = combiner(a, ConcatCombiner)
  override def max(a: List[Int]): Int = combiner(a, MaxCombiner)

  def combiner[A](a: Seq[A], c: Combiner[A]): A =
    if (a.isEmpty)
      return c.unit
    var res = a.head
    for (x <- a.tail)
      res = c.combine(res, x)
    res

//1.3
object FunctionsImpl3 extends Functions:
  override def sum(a: List[Double]): Double = combiner(a)(using SumCombiner)
  override def concat(a: Seq[String]): String = combiner(a)(using ConcatCombiner)
  override def max(a: List[Int]): Int = combiner(a)(using MaxCombiner)

  def combiner[A](a: Seq[A])(using c: Combiner[A]): A =
    if (a.isEmpty)
      return c.unit
    var res = a.head
    for (x <- a.tail)
      res = c.combine(res, x)
    res


@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648

  val f2: Functions = FunctionsImpl2
  println(f2.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f2.sum(List())) // 0.0
  println(f2.concat(Seq("a", "b", "c"))) // abc
  println(f2.concat(Seq())) // ""
  println(f2.max(List(-10, 3, -5, 0))) // 3
  println(f2.max(List())) // -2147483648

  val f3: Functions = FunctionsImpl3
  println(f3.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f3.sum(List())) // 0.0
  println(f3.concat(Seq("a", "b", "c"))) // abc
  println(f3.concat(Seq())) // ""
  println(f3.max(List(-10, 3, -5, 0))) // 3
  println(f3.max(List())) // -2147483648
