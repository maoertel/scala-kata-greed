import Rules.CompoundRule
import cats.data._
import cats.implicits._

object Greed {

  type DieValue = Int
  type Score = Int
  type ValidationResult[A] = ValidatedNel[DieInputValidationError, A]

  def score(dieValues: List[DieValue])(implicit rules: List[CompoundRule]): ValidationResult[Score] =
    (Validator.validateDieValues(dieValues), Validator.validateAmountOfDies(dieValues)).mapN { (_, _) =>
      val valueMap = dieValues.groupBy(identity).map { case (dieValue, amount) => (dieValue, amount.size) }
      val scores = rules.map(rule => rule(valueMap))

      scores.max
    }

}
