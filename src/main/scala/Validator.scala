import Greed.{DieValue, ValidationResult}
import cats.implicits._

object Validator {

  def validateAmountOfDies(dieValues: List[DieValue]): ValidationResult[List[DieValue]] =
    if (dieValues.size > 6) AmountOfDiesError.invalidNel else dieValues.validNel

  def validateDieValues(dieValues: List[DieValue]): ValidationResult[List[DieValue]] =
    if (dieValues.exists(die => die > 6 || die < 1)) DieNotValid.invalidNel else dieValues.validNel

}
