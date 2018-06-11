package json

trait DynamicJSON
trait JString extends DynamicJSON
trait JInt extends DynamicJSON
trait JDouble extends DynamicJSON
trait JBool extends DynamicJSON
trait JSeq extends Seq[DynamicJSON]

class JSON(data: String) extends DynamicJSON {

}
