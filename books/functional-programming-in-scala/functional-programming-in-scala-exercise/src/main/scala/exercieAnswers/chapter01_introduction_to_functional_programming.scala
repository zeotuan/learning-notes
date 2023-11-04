package exercieAnswers


object chapter01_introduction_to_functional_programming {
  // Example of pure function with no side effect
  case class Charge(cc: CreditCard, amount: Double) {
    def combine(other: Charge): Charge = if (cc == other.cc) {
      Charge(cc, amount + other.amount)
    } else {
      throw new Exception("Can't combine charges to different credit card")
    }
  }
  class Cafe {
    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
      val cup = new Coffee()
      (cup, Charge(cc, cup.price))
    }

    def buyCoffees(cc: CreditCard, n: Int): (Seq[Coffee], Charge) = {
      val purchases: Seq[(Coffee, Charge)] = Seq.fill(n)(buyCoffee(cc))
      val (coffees, charges) = purchases.unzip
      (coffees, charges.reduce(_ combine _)) // This is short hand for charges.reduce((c1,c2) => c1.combine(c2))
    }
  }

  class CreditCard {}

  class Coffee {
    val price: Double = 2
  }

  // get combined charge groupby credit card
  def coalesce(charges: Seq[Charge]): Seq[Charge] = {
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toSeq
  }

  val cafeShop = new Cafe()
  val myCreditCard = new CreditCard()
  val (coffees, charge) = cafeShop.buyCoffees(myCreditCard, 5)

  // sends coffees and charge information to payment service for processing
  // consolidate side effect handling only to certain part of the program

  assert(coffees.length == 5)
  assert(charge.amount == 10)
}

