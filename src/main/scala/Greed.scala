import Greed.DieResult

object Greed {

  type DieValue = Int
  type Score = Int
  type GeneralRule = DieResult => Score
  type SpecialRule = Map[DieValue, Int] => Score

  case class DieResult(die: Int, amount: Int)

  def score(
             dieValues: List[DieValue]
           )(
             implicit generalRules: List[GeneralRule],
             specialRules: List[SpecialRule]
           ): Score = {
    val valueMap = dieValues.groupBy(identity).map { case (dieValue, amount) => (dieValue, amount.size) }

    val specialScores = specialRules.map(rule => rule(valueMap))
    val generalScore = valueMap
      .map { case (dieValue, amount) => generalRules.map(rule => rule(DieResult(dieValue, amount))).max }
      .iterator
      .sum

    (generalScore :: specialScores).max
  }
}

object Rules {

  import Greed.{GeneralRule, SpecialRule}

  private val singleOne: GeneralRule = dieResult => if (dieResult.die == 1 && dieResult.amount == 1) 100 else 0
  private val tripleOne: GeneralRule = dieResult => if (dieResult.die == 1 && dieResult.amount == 3) 1000 else 0
  private val singleFive: GeneralRule = dieResult => if (dieResult.die == 5 && dieResult.amount == 1) 50 else 0
  private val singleSpecialRules = List(singleOne, singleFive, tripleOne)

  private val triples: GeneralRule = dieResult => if (dieResult.amount == 3) dieResult.die * 100 else 0
  private val quadruples: GeneralRule = dieResult => if (dieResult.amount == 4) dieResult.die * 100 * 2 else 0
  private val quintuples: GeneralRule = dieResult => if (dieResult.amount == 5) dieResult.die * 100 * 4 else 0
  private val sextuples: GeneralRule = dieResult => if (dieResult.amount == 6) dieResult.die * 100 * 8 else 0

  private val singleRules = List(triples, quadruples, quintuples, sextuples)

  private val compoundSingleRules = singleRules ++ singleSpecialRules
  private val compoundRules: SpecialRule = valMap => valMap
    .map { case (dieValue, amount) => compoundSingleRules.map(rule => rule(DieResult(dieValue, amount))).max }
    .iterator
    .sum
  private val straight: SpecialRule = valMap => if (valMap.size == 6) 1200 else 0
  private val threePairs: SpecialRule = { valMap =>
    val amounts = valMap.map { case (_, amount) => amount }.toList
    if (amounts.forall(_ == 2) && amounts.size == 3) 800 else 0
  }

  object Implicits {
    implicit val generalRules: List[GeneralRule] = compoundSingleRules
    implicit val specialRules: List[SpecialRule] = straight :: threePairs :: compoundRules :: Nil
  }

}

//object Greed extends App {
//  type DieValue = Int
//  type Score = Int
//
//  case class PartialScore(score: Score, diceUsed: List[DieValue])
//
//  def highestScoreOf(remaining: List[DieValue]): PartialScore = ???
//
//  def score(values: List[DieValue]): Score = {
//
//    //    def traverse(remaining: List[DieValue], acc: Score): Score = remaining match {
//    //      case Nil =>
//    //        acc
//    //      case _ => {
//    //        val partialScore = highestScoreOf(remaining)
//    //        traverse(remaining partialScore.diceUsed, partialScore.score + acc)
//    //      }
//    //    }
//    //
//    //    traverse(values, 0)
//
//    //    val numberOnes = values.groupBy(identity).get(1).map(_.size).getOrElse(0)
//    //    if (numberOnes == 1) 100
//    //    else if (numberOnes == 2) 200
//    //    else if (numberOnes == 3) 1000
//    //    else if (numberOnes == 4) 2000
//    //    else if (numberOnes == 5) 4000
//    //    else if (numberOnes == 6) 8000
//    //    else 0
//  }
//}
//
//
////class Greed {
////
////  sealed trait Result
////
////  final case class Score(value: Int) extends Result
////
////  final case class InvalidNumberOfDice(providedNumberOfDice: Int) extends Result
////
////  def score(dieValues: List[Int]): Result = {
////    if (dieValues.length > 6) InvalidNumberOfDice(dieValues.length)
////    else {
////      //      val valuesByDice = dieValues.groupBy(identity).map { case (k, v) => (k, v.size) }
////      val valuesByDice: Map[Int, Int] = dieValues
////        .groupBy(identity)
////        .view
////        .mapValues(_.size)
////        .toMap
////
////      if (valuesByDice.keys.size == 6) Score(1200)
////      else ???
////
////
////      //      if (dieValues.contains(1)) Score(100)
////      //      else Score(0)
////    }
////    ???
////  }
////}
////
////object DiceApp extends App {
////  new Greed().score(List(1, 2, 3, 3, 3, 5))
////}