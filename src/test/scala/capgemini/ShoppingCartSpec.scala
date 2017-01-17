package capgemini

import org.specs2._
import capgemini.ShoppingCart._
import scala.collection.immutable

class ShoppingCartSpec extends Specification {
	def is = s2"""

	ShoppingCart

	Empty shopping cart should return 0 		$emptyShoppingCart

	[Apple] shopping cart should return 0.60 	$appleShoppingCart

	[Orange] shopping cart should return 0.25	$orangeShoppingCart

	[Apple, Orange] shopping cart should return 0.85 	$appleOrangeShoppingCart

	Invalid shopping cart should return None	$invalidShoppingCart
	"""

	def emptyShoppingCart = {
		ShoppingCart.cacluratePriceInput(List()) must beEqualTo(Some(BigDecimal("0.0")))
	}

	def appleShoppingCart = {
		ShoppingCart.cacluratePriceInput(List("Apple")) must beEqualTo(Some(BigDecimal("0.60")))
	}

	def orangeShoppingCart = {
		ShoppingCart.cacluratePriceInput(List("Orange")) must beEqualTo(Some(BigDecimal("0.25")))
	}

	def appleOrangeShoppingCart = {
		ShoppingCart.cacluratePriceInput(List("Apple", "Orange")) must beEqualTo(Some(BigDecimal("0.85")))
	}

	def invalidShoppingCart = {
		ShoppingCart.cacluratePriceInput(List("Banana")) must beEqualTo(None)
	}
}

