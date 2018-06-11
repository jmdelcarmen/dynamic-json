package json

abstract class Config extends Map[String, Any]

object Parser {
  def apply(config: Config)(data: String): Option[JSON] = {
    new JSON(data)
  }

  private def parse(data: String): JSON = {

  }
}
