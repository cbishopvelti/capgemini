package capgemini

import capgemini._

object ShoppingCart {

	def main(args: Array[String]): Unit = {
		while (true) {
			println("Please enter the checkout items as a comma seperate list ie 'Apple, Orange'")
			val ln = readLine()
			val ln2 = ln.replaceAll(" ", "")
			val items = ln2.split(",").toList;
			val price = cacluratePriceInput(items);
			println(price);
		}
	}

	/**
	Takes a list of items as string which can include "Apple" or "Orange"
	Performes validation to ensure that the list only contains "Apple" or "Orange"
	Returns the sum of the price
	*/
	def cacluratePriceInput (cart: List[String]): Option[BigDecimal] = {

		val cartTyped: Option[List[ShoppingCartModel]] = ShoppingCartService.getAllByString(cart);

		cartTyped match {
			case None => None
			case Some(items) => Some(ShoppingCartModel.calculatePriceWithDiscount(items))
		}
	}
}
