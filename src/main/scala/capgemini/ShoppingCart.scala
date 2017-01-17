package capgemini

import java.lang.RuntimeException

// getOneFree is the number of items in buy x get 1 free rule.
class Item (val name: String, val price: BigDecimal, val getOneFree: Option[Integer] ) {}

object Item {
	def apply (name: String, price: BigDecimal): Item = new Item(name, price, None)
	def apply (name: String, price: BigDecimal, getOneFree: Option[Integer]): Item = new Item(name, price, getOneFree)
}

object ShoppingCart {

	// Could use enums here to ensure type safty.
	val items: Map[String, Item] = Map(
		"Apple" -> Item("Apple", BigDecimal("0.60"), Some(2)),
		"Orange" -> Item("Orange", BigDecimal("0.25"), Some(3))
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
			case Some(items) => Some(_calculatePriceWithDiscount(items))
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
	Calculates the price, applying a buy x get 1 free rule.
	*/
	def _calculatePriceWithDiscount (cart: List[Item]): BigDecimal = {
		var groupedItems: Map[String, List[Item]] = cart.groupBy(item => item.name)

		var groudpItems2: Iterable[BigDecimal] = groupedItems.map({ case (key, theItems) => 

			// Set the x get one free value
			val tItem = items.get(key) match {
				case Some(value) => value
				// Shouldn't happen, as we have allready validated for this case.
				case None => throw new RuntimeException("No value")
			}

			// Calculate the number of items to charge for
			val noOfItems = tItem.getOneFree match {
				case None => theItems.length
				case Some(getOneFree) => theItems.length - (theItems.length / getOneFree)
			}

			val priceForItems = noOfItems * tItem.price
			priceForItems
		})

		groudpItems2.foldRight[BigDecimal](BigDecimal("0.0")) {
			(item, a) => item + a
		}
	}
}
