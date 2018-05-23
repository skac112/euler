package skac.euler.inout.gexf

object AttrDef {
  val ATTR_TYPE_STRING = 'ATTR_TYPE_STRING
  val ATTR_TYPE_BOOLEAN = 'ATTR_TYPE_BOOLEAN
  val ATTR_TYPE_INTEGER = 'ATTR_TYPE_INTEGER
  val ATTR_TYPE_FLOAT = 'ATTR_TYPE_FLOAT
  val ATTR_TYPE_DOUBLE = 'ATTR_TYPE_DOUBLE
  val ATTR_TYPE_LIST_STRING = 'ATTR_TYPE_LIST_STRING

  val TypeNames = Map(ATTR_TYPE_STRING -> "string",
   ATTR_TYPE_BOOLEAN -> "boolean",
   ATTR_TYPE_INTEGER -> "integer",
   ATTR_TYPE_FLOAT -> "float",
   ATTR_TYPE_DOUBLE -> "double",
   ATTR_TYPE_LIST_STRING -> "list-string")

  def typeFromAttr(Attr: Any) = Attr match {
    case _: Boolean => ATTR_TYPE_BOOLEAN
    case _: String => ATTR_TYPE_STRING
    case _: Int => ATTR_TYPE_INTEGER
    case _: Float => ATTR_TYPE_FLOAT
    case _: Double => ATTR_TYPE_DOUBLE
  }

  def typeFromStr(TypeStr: String) = {
    // odwrocenie mapy
    val type_names_transp = TypeNames map {_.swap}
    type_names_transp(TypeStr)
  }
}

case class AttrDef(Id: String, Title: String, Type: Symbol, DefaultValue: Option[Any] = None) {
  import AttrDef._

  def typeStr = TypeNames(Type)

}
