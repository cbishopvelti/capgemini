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

	[Apple, Apple] should return 0.60			$appleDiscount

	[Apple, Apple, Apple] should discount 1 returning 1.20	$appleDiscount3

	[Orange, Orange, Orange] should discount 1 returning 0.50	$orangeDiscount

	[Orange, Orange, Orange, Orange] should discount 1 returning 0.75	$orangeDiscount4
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

	def appleDiscount = {
		ShoppingCart.cacluratePriceInput(List("Apple", "Apple")) must beEqualTo(Some(BigDecimal("0.60")))
	}
	def appleDiscount3 = {
		ShoppingCart.cacluratePriceInput(List("Apple", "Apple", "Apple")) must beEqualTo(Some(BigDecimal("1.20")))
	}

	def orangeDiscount = {
		ShoppingCart.cacluratePriceInput(List("Orange", "Orange", "Orange")) must beEqualTo(Some(BigDecimal("0.50")))
	}
	def orangeDiscount4 = {
		ShoppingCart.cacluratePriceInput(List("Orange", "Orange", "Orange", "Orange")) must beEqualTo(Some(BigDecimal("0.75")))
	}
}

