object Main extends App {

  import Rules.Implicits._

  val greed1000 = Greed.score(List(1, 1, 1))
  val greed100 = Greed.score(List(1))
  val greed50 = Greed.score(List(5))
  val greed800 = Greed.score(List(2, 2, 3, 3, 5, 5))
  val greed0 = Greed.score(List(1, 2, 3))
  val greedInvalid = Greed.score(List(0, 7))
  val greedInvalid2 = Greed.score(List(0, 7, 1, 1, 1, 1, 1, 1))

  println(greed1000)
  println(greed100)
  println(greed50)
  println(greed800)
  println(greed0)
  println(greedInvalid)
  println(greedInvalid2)

}
