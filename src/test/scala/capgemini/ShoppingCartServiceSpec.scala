package capgemini

import org.specs2._
import capgemini.ShoppingCartService._
import scala.collection.immutable

class ShoppingCartServiceSpec extends Specification {
	def is = s2"""

	ShoppingCartService

	getAll								 		$getAll

	getAllByString invalid 						$getAllByStringInvalid

	getAllByString valid 						$getAllByStringValid

	getOne										$getOne
	"""

	def getAll = {
		val expectedItem = ShoppingCartModel("Orange", BigDecimal("0.25"), Some(2))
		ShoppingCartService.getAll().get("Orange").get must beEqualTo(expectedItem)
	}

	def getAllByStringInvalid = {
		ShoppingCartService.getAllByString(List("Banana")) must beEqualTo(None)
	}

	def getAllByStringValid = {
		val expectedItem = ShoppingCartModel("Apple", BigDecimal("0.60"), Some(2))
		ShoppingCartService.getAllByString(List("Apple")) must beEqualTo(Some(List(expectedItem)))
	}

	def getOne = {
		val expectedItem = ShoppingCartModel("Apple", BigDecimal("0.60"), Some(2))
		ShoppingCartService.get("Apple") must beEqualTo(Some(expectedItem))
	}
}