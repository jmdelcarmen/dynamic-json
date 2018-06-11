val res = parse("{\"marco \":\"del carmen\", \"Marco\": \"Hooters\", \"yolo\": \"swagger\"}")

trait Accumulator[T] {
  val value: Any
  val terminator: Option[Char]
  val assocSeq: Option[Accumulator[T]]

  def isKey: Boolean = this match {
    case AccKey(_, _, _) => true
    case AccValue(_, _, _) => false
  }
}

case class AccKey[T](value: String, terminator: Option[Char], assocSeq: Option[Accumulator[T]])
  extends Accumulator[T]

case class AccValue[T](value: T, terminator: Option[Char], assocSeq: Option[Accumulator[T]])
  extends Accumulator[T]

def parse[T](data: String): Map[String, Any] = {

  def _parse(data: List[Char], acc: List[Accumulator[T]]): Any = {
    data match {
      case Nil => acc.head // get value from case class
      case char :: charList => char match {

        case '{' => {
          acc match {
            case Nil => _parse(charList, AccValue(Map.asInstanceOf[T], Some('}'), None) :: acc)
            case AccKey(_, _, assocSeq) :: rest =>
              _parse(charList, AccValue(Map.asInstanceOf[T], Some('}'), assocSeq) :: rest)
            case _ => println("You fucked up symbol: { ")
          }
        }

        case '}' => acc match {
          // TODO: finish
          case AccValue(_, _, valAssocSeq) :: _ => {
            // get keys and values and insert into map
            def composeMap(currentKey: Option[Accumulator[T]],
                           currentValue: Option[Accumulator[T]],
                           accumulators: List[Accumulator[T]],
                           map: T): T = {
              if (accumulators.isEmpty) map
              else {
                if (currentValue.isDefined && currentKey.isDefined) {
                  val nextMap = map.asInstanceOf[Map[String, T]] +
                    (currentKey.get.value.asInstanceOf[String] -> currentValue.get.value)

                  composeMap(None, None, accumulators, nextMap.asInstanceOf[T])
                } else {
                  val nextAcc :: rest = accumulators
                  // Set next acc
                  if (nextAcc.isKey) composeMap(Some(nextAcc), currentValue, rest, map)
                  else composeMap(currentKey, Some(nextAcc), rest, map)
                }
              }
            }

            val (keyOrValAccList, rest) = acc.partition{
              case AccValue(_, _, currentAssocSeq) => currentAssocSeq == valAssocSeq
              case AccKey(_, _, currentAssocSeq) => currentAssocSeq == valAssocSeq
            }

            val mapResult = composeMap(None, None, keyOrValAccList, Map().asInstanceOf[T])

            _parse(charList, AccValue(mapResult, None, valAssocSeq) :: rest.drop(1))
          }
        }

        case '\"' => acc match {
          // first key
          case AccValue(_, _, valAssocSeq) :: Nil =>
            _parse(charList, AccKey("", Some('\"'), valAssocSeq) :: acc)

          case AccKey(key, keyTerm, keyAssocSeq) :: AccValue(_, _, _) :: _ => {
            // new value
            if (keyTerm.isEmpty) _parse(charList, AccValue("".asInstanceOf[T], Some('\"'), keyAssocSeq) :: acc)
            // closing current key
            else _parse(charList, AccKey(key, None, keyAssocSeq) :: acc.drop(1))
          }

          case AccValue(value, valTerm, valAssocSeq) :: AccKey(_, _, _) :: _ => {
            // new key
            if (valTerm.getOrElse("").toString.isEmpty) _parse(charList, AccKey("", Some('\"'), valAssocSeq) :: acc)
            // closing current value
            else _parse(charList, AccValue(value, None, valAssocSeq) :: acc.drop(1))
          }

          case _ => println(s"I don't know what to do $charList")
        }

        // TODO: handle general form
        case ':' => acc match {
          case _ => _parse(charList, acc)
        }

        // TODO: handle JSON tokens
        case _ => acc match {
          case AccValue(value, valTerm, valAssocSeq) :: rest =>
            _parse(charList, AccValue(s"$value$char".asInstanceOf[T], valTerm, valAssocSeq) :: rest)

          case AccKey(key, keyTerm, keyAssocSeq) :: rest =>
            _parse(charList, AccKey(s"$key$char", keyTerm, keyAssocSeq) :: rest)
        }
      }
    }
  }

  _parse(data.toList, List()).asInstanceOf[AccValue[Map[String, Any]]].value
}