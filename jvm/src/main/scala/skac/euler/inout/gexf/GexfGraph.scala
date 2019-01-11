package skac.euler.inout.gexf

import skac.euler.Graph
import skac.euler.General._
import AttrDef._
import java.io.Writer
import java.util
import scala.xml.NodeSeq
import scala.xml.Elem
import java.io.Reader
import java.io.Writer
import java.io.InputStream
import java.io.OutputStream

class GexfGraph extends skac.euler.impl.fastindex.immutable.Graph[GexfNode, GexfEdge] {

  var NodeAttributes: AttrDefs = _
  var EdgeAttributes: AttrDefs = _
  //private var nextId = 0

  private def createRootNode = {
      <gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
        <graph mode="static" defaultedgetype="directed">
		  {createAttrDefs()
		  <attributes class="node">
			 {for (attr_def <- NodeAttributes) yield
			  <attribute id={attr_def.Id.toString} title={attr_def.Title} type={attr_def.typeStr}>
			  	{if (attr_def.DefaultValue.isDefined)
			  	  <default>{attr_def.DefaultValue.get}</default>
			  	}
			  </attribute>
			 }
		  </attributes>
	      <attributes class="edge">
			 {for (attr_def <- EdgeAttributes) yield
			  <attribute id={attr_def.Id.toString} title={attr_def.Title} type={attr_def.typeStr}>
			  	{if (attr_def.DefaultValue.isDefined)
			  	  <default>{attr_def.DefaultValue.get}</default>
			  	}
			  </attribute>
			 }
	      </attributes>
		  }
    	  <nodes>
    		{for (n <- this.nodes) yield
    		  <node id={n.ID.toString} label={n.Data.Label}>
    		    <attvalues>
    		  	  {for (att <- n.Data.Attributes) yield
    		  	    <attvalue for={att._1} value={this.attToString(att._2)} />
    		  	  }
    		  	</attvalues>
    		  </node>
    		}
    	  </nodes>
          <edges>
    		{for (e <- this.edges) yield
    		  <edge id={e.ID.toString} label={e.Data.Label} source={node(e.SrcNode).get.ID.toString} target={node(e.DstNode).get.ID.toString}>
    		  	<attvalues>
    		  	  {for (att <- e.Data.Attributes) yield
    		  	    <attvalue for={att._1} value={this.attToString(att._2)} />
    		  	  }
    		  	</attvalues>
    		  </edge>
    		}
          </edges>
    	</graph>
      </gexf>
    }

  private def attToString(AttrValue: Any) = AttrValue match {
    // atrybut typu liststring
    case seq: Seq[_] if seq.isInstanceOf[Seq[String]] => seq.asInstanceOf[Seq[String]].reduceLeft {_.toString + "|" + _.toString}
    // pozostale typy atrybutow
    case a => a.toString
  }

  private def strToAttVal(AttDefs: AttrDefs)(AttValNode: NodeSeq) = (AttDefs find
    {att_def => att_def.Id == (AttValNode \ "@for").text}).get Type match {
      case AttrDef.ATTR_TYPE_BOOLEAN => (AttValNode \ "@value").text.toBoolean
      case AttrDef.ATTR_TYPE_STRING => (AttValNode \ "@value").text
      case AttrDef.ATTR_TYPE_INTEGER => (AttValNode \ "@value").text.toInt
      case AttrDef.ATTR_TYPE_FLOAT => (AttValNode \ "@value").text.toFloat
      case AttrDef.ATTR_TYPE_DOUBLE => (AttValNode \ "@value").text.toDouble}

//  def createNextId {
//    nextId = nextId + 1
//  }

  /**
   * Zapisuje graf gexf do pliku.
   */
  def save(Filename: String) {
	xml.XML.save(Filename, createRootNode, "UTF-8", true)
  }

  def save(Writer: Writer) {
    xml.XML.write(Writer, createRootNode, "UTF-8", true, null)
  }

  /**
   * Odczytuje graf gexf z pliku
   */
  def open(Filename: String) {
    val root_node = xml.XML.loadFile(Filename)
    readFromXML(root_node)
  }

  def read(Reader: Reader) {
    val root_node = xml.XML.load(Reader)
    readFromXML(root_node)
  }

  def read(Stream: InputStream) {
    val root_node = xml.XML.load(Stream)
    readFromXML(root_node)
  }

  private def readFromXML(RootNode: Elem) {
    // mapa stare_id -> nowe_id
    var id_map = scala.collection.mutable.Map[String, Any]()
    readAttrDefs(RootNode)
    readNodes(RootNode, id_map)
    readEdges(RootNode, id_map)
  }

  /**
   * Na podstawie zawartosci XML tworzy zbiory definicji atrybutow wezlow i krawedzi i zapisuje je
   * w polach NodeAttributes i EdgeAttributes.
   */
  private def readAttrDefs(RootNode: Elem) {
    NodeAttributes = (RootNode \ "graph" \ "attributes" find {n: NodeSeq => (n \ "@class").text == "node"}).get \
     "attribute" map {attr => AttrDef((attr \ "@id").text, (attr \ "@title").text,
                                      typeFromStr((attr \ "@type").text))} toSet

    EdgeAttributes = (RootNode \ "graph" \ "attributes" find {n: NodeSeq => (n \ "@class").text == "edge"}).get \
     "attribute" map {attr => AttrDef((attr \ "@id").text, (attr \ "@title").text,
                                      typeFromStr((attr \ "@type").text))} toSet
  }

  /**
   * Odczytuje dane wezlow z xml. Uzupelnia mape IdMap <xm_id_wezla> -> <id_wezla_w_grafie>, wykorzystywana
   * pozniej przez readEdges().
   */
  private def readNodes(RootNode: Elem, IdMap: scala.collection.mutable.Map[String, Any]) {
    RootNode \ "graph" \ "nodes" \ "node" foreach {xml_node => {
      val id = (xml_node \ "@id").text
      IdMap(id) = NewNodeID
      val label = id
      val att_val_fun = strToAttVal(NodeAttributes) _
      //println(xml_node \ "attvalues")
      val att_values = xml_node \ "attvalues" \ "attvalue" map {att_val =>
        ((att_val \ "@for").text -> att_val_fun(att_val))} toMap

      this.addNode(new GexfNode(label, att_values))
    }}
  }

  private def readEdges(RootNode: Elem, IdMap: scala.collection.mutable.Map[String, Any]) {
    RootNode \ "graph" \ "edges" \ "edge" foreach {xml_edge => {
      val src_id = (xml_edge \ "@source").text
      val target_id = (xml_edge \ "@target").text
      val label = ""
      val att_val_fun = strToAttVal(EdgeAttributes) _

      val att_values = xml_edge \ "attvalues" \ "attvalue" map {att_val =>
        ((att_val \ "@for").text -> att_val_fun(att_val))} toMap

      this.addEdge(new GexfEdge(label, att_values), IdMap(src_id).id, IdMap(target_id).id)
    }}
  }

  def write(Writer: Writer) {
    xml.XML.write(Writer, createRootNode, "UTF-8", true, null)
  }

  /**
    * Zwraca lancuch XML GEXF grafu.
	*/
  def getXML(): String = {
	val writer: java.io.StringWriter = new java.io.StringWriter()
	xml.XML.write(writer, createRootNode, "UTF-8", true, null)
	writer.toString
  }

	/**
	 * Tworzy nowy wezel. Atrybuty sa tworzone na podstawie definicji atrybutow dla wezla - z podanej w parametrze
	 * mapy dodawane sa tylko pozycje odpowiadajace istniejacym atrybutom. Wezel nie jest dodawany do grafu.
	 * Dzieki argumentowi Attributes mozliwa jest inicjacja wartosci atrybutow
	 */
//	def newNode(Label: String, Attributes: Attributes = Map[String, Any]()) =
//	  GexfNode(Label, Attributes filter {kv => NodeAttributes exists {_.Id == kv._1}})

//	def newEdge(Label: String, Attributes: Attributes = Map[String, Any]()) =
//	  GexfEdge(Label, Attributes filter {kv => EdgeAttributes exists {_.Id == kv._1}})

  /**
   * Tworzy definicje atrybutow wezlow i krawedzi na podstawie juz przypisanych do wezlow i krawedzi atrybutow.
   */
  private def createAttrDefs() {
	createNodeAttrDefs()
	createEdgeAttrDefs()
  }

  private def createNodeAttrDefs() {
	  NodeAttributes = Set()
	  // iteracja po wezlach
	  this.nodes foreach {
	    // iteracja po atrybutach wezlow - dla kazdego atrybutu sprawdza, czy w definicjach atrybutow wezla
	    // jest juz pozycja o odp. etykiecie i jesli nie, to dodaje do definicji
	    _.Data.Attributes foreach {attr: ((String, Any)) => {
		  if (!(NodeAttributes map {_.Title} contains attr._1)) {
		    NodeAttributes = NodeAttributes + AttrDef(attr._1, attr._1, AttrDef.typeFromAttr(attr._2))
		  }
	  }}}
  }

  private def createEdgeAttrDefs() {
	  EdgeAttributes = Set()
	  // iteracja po wezlach
	  this.edges foreach {
	    // iteracja po atrybutach wezlow - dla kazdego atrybutu sprawdza, czy w definicjach atrybutow wezla
	    // jest juz pozycja o odp. etykiecie i jesli nie, to dodaje do definicji
	    _.Data.Attributes foreach {attr: ((String, Any)) => {
		  if (!(EdgeAttributes map {_.Title} contains attr._1)) {
		    EdgeAttributes = EdgeAttributes + AttrDef(attr._1, attr._1, AttrDef.typeFromAttr(attr._2))
		  }
	  }}}
  }
}
