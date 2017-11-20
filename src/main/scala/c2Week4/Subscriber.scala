package c2Week4

trait Subscriber {
  def handler(pup: Publisher): Unit
}
