package c2Week4


class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _ // _ means uninitialised
  compute()

  private def compute() =
    total = observed.map(_.currentBalance).sum

  def handler(pup: Publisher) = compute()
  def totalBalance = total

}
