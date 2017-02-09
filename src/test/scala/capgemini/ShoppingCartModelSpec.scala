package capgemini

import org.specs2._
import capgemini.ShoppingCartModel._
import scala.collection.immutable

class ShoppingCartModelSpec extends Specification {
	def is = s2"""

	ShoppingCartModel

	Empty shopping cart should return 0 		$emptyShoppingCart
	One item should return 0.6					$oneItem
	3 items with buy one get one free			$buyOneGetOneFree
	"""

	def emptyShoppingCart = {
		ShoppingCartModel.calculatePriceWithDiscount(List()) must beEqualTo(BigDecimal("0.0"))
	}

	def oneItem = {
		val cart = List(ShoppingCartModel("Apple", BigDecimal(0.6), None))
		ShoppingCartModel.calculatePriceWithDiscount(cart) must beEqualTo(BigDecimal("0.6"))	
	}

	def buyOneGetOneFree = {
		val cart = List.fill(3)(ShoppingCartModel("Apple", BigDecimal(0.6), Some(2)))
		ShoppingCartModel.calculatePriceWithDiscount(cart) must beEqualTo(BigDecimal("1.2"))
	}
}