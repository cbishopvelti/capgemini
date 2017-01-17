package capgemini

import java.lang.RuntimeException

class Item (val name: String, val price: BigDecimal) {}

object Item {
	def apply (name: String, price: BigDecimal): Item = new Item(name, price)
}

object ShoppingCart {

	val items: Map[String, Item] = Map(
		"Apple" -> Item("Apple", BigDecimal("0.60")),
		"Orange" -> Item("Orange", BigDecimal("0.25"))
	)

	def main(args: Array[String]): Unit = {
		while(true){
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

		val cartTyped: Option[List[Item]] = _lookup(cart);

		cartTyped match {
			case None => None
			case Some(items) => Some(_calculatePrice(items))
		}
	}

	def _lookup (cart: List[String]): Option[List[Item]] = {
		val cartTyped: List[Option[Item]] = cart.map(itemString => items.get(itemString))

		return _allValid(cartTyped)
	}

	/**
	Converts List[Option] to Option[List] if any items ane None
	*/
	def _allValid[T](items: List[Option[T]]): Option[List[T]] = {

		items.contains(None) match {
			case true => None
			case false => Some(items.map(itemOption => itemOption match {
				case Some(item) => item
				// probably a better way of doing this, could fold instead
				case None => throw new RuntimeException("returned None")
			}))
		}
	}

	/**
	Sums the list of items.
	Arguably the simplest possible solution for 'Step 1'
	*/
	def _calculatePrice (cart: List[Item]): BigDecimal = {

		val cartPrice: BigDecimal = cart.foldRight[BigDecimal](BigDecimal("0.0")) {
			(item, a) => item.price + a
		}

		return cartPrice
	}
}
