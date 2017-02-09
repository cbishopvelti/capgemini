package capgemini

import java.lang.RuntimeException

// getOneFree is the number of items in buy x get 1 free rule.
class ShoppingCartModel (val name: String, val price: BigDecimal, val getOneFree: Option[Integer] ) {
	override def equals (that: Any): Boolean = {
		that match {
			case that: ShoppingCartModel => this.name == that.name
			case _ => false
		}
	}
}

object ShoppingCartModel {
	def apply (name: String, price: BigDecimal, getOneFree: Option[Integer]): ShoppingCartModel = 
		new ShoppingCartModel(name, price, getOneFree)

	/**
	Calculates the price, applying a buy x get 1 free rule.
	*/
	def calculatePriceWithDiscount (cart: List[ShoppingCartModel]): BigDecimal = {
		val groupedItems: Map[String, List[ShoppingCartModel]] = cart.groupBy(item => item.name)

		val groudpItems2: Iterable[BigDecimal] = groupedItems.map({ case (key, theItems) => 

			// Set the x get one free value
			val tItem = ShoppingCartService.get(key) match {
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
