package skac.euler.inout

package object gexf {
	type AttrDefs = Set[AttrDef]
	type Attributes = Map[String, Any]
	type ElementSerializer[T] = T => Attributes
	type ElementDeserializer[T] = Attributes => T
}
