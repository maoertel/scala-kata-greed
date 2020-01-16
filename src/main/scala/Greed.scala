import cats.Applicative
import cats.data._
import cats.implicits._

object Greed {

  type DieValue = Int
  type Score = Int
  type SingleRule = DieResult => Score
  type CompoundRule = Map[DieValue, Int] => Score
  type ValidationResult[A] = ValidatedNel[DieInputValidationError, A]

  def score(dieValues: List[DieValue])(implicit rules: List[CompoundRule]): ValidationResult[Score] =
    Applicative[ValidationResult].map2(
      Validator.validateDieValues(dieValues),
      Validator.validateAmountOfDies(dieValues)
    ) { (_, _) =>
      val valueMap = dieValues.groupBy(identity).map { case (dieValue, amount) => (dieValue, amount.size) }
      val scores = rules.map(rule => rule(valueMap))

      scores.max
    }

}
