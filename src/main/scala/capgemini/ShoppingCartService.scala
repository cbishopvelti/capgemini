package capgemini

object ShoppingCartService {

	val items: Map[String, ShoppingCartModel] = Map(
		"Apple" -> ShoppingCartModel("Apple", BigDecimal("0.60"), Some(2)),
		"Orange" -> ShoppingCartModel("Orange", BigDecimal("0.25"), Some(3))
	)

	def getAll (): Map[String, ShoppingCartModel] = {
		items
	}

	def getAllByString(stringItems: List[String]): Option[List[ShoppingCartModel]] = {
		val cartTyped: List[Option[ShoppingCartModel]] = 
			stringItems.map(itemString => items.get(itemString))

		return allValid(cartTyped)
	}

	def get(key: String): Option[ShoppingCartModel] = items.get(key)

	/**
	Converts List[Option] to Option[List] if any items ane None
	*/
	protected def allValid[T](items: List[Option[T]]): Option[List[T]] = {

		items.contains(None) match {
			case true => None
			case false => Some(items.map(itemOption => itemOption match {
				case Some(item) => item
				case None => throw new RuntimeException("returned None")
			}))
		}
	}
}