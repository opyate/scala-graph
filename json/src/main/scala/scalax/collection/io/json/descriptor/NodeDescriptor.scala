package scalax.collection.io.json
package descriptor

import net.liftweb.json._

import error.JsonGraphError._, error.JsonGraphWarning._

/** Provides information on how to extract node data from a JValue and how to
 * decompose the node to a JValue.
 * 
 * @tparam N type of nodes described with this descriptor which either the same type or
 *           a subtype of the node type parameter of the targeted graph.
 * @param typeId denotes the node type in a JSON text; defaults to `(default)`.
 * @param customSerializers list of list-json custom serializers to be invoked for node
 *        serialization; defaults to an empty list.
 * @param furtherManifests list of manifests to denote subtypes of `N` which are to be
 *        processed by this node descriptor; defaults to an empty list. 
 */
abstract class NodeDescriptor[+N]
              (override val typeId:   String = Defaults.defaultId,
               customSerializers:     Traversable[Serializer[_]] = List.empty[Serializer[_]],
               furtherManifests:      List[ClassManifest[_]]     = List.empty[ClassManifest[_]])
              (implicit nodeManifest: Manifest[N])
  extends TypeId(typeId)
{
  val manifests = nodeManifest :: furtherManifests
  implicit val formats = Serialization.formats(NoTypeHints) ++ customSerializers 
  def extract  (jsonNode: JValue): N = jsonNode.extract[N]
  def decompose(node: Any) : JValue  = Extraction.decompose(node)
  /**
   * Enables Graph for Scala JSON export/import to handle node references in JSON edge entries.
   * Without establishing such references, JSON edge entries would have to contain all node
   * data what would make JSON texts representing graphs explode in length.
   * 
   * Please exercise great care when designing the id method to return unique keys.
   * 
   * @param node a node of type `N` for which its unique id is to be returned.
   *             You can safely match `node` to the actual type argument `N`.  
   */
  def id(node: Any): String 
}
/**
 * Node descriptor extracting a String from any JValue and decomposing nodes
 * of any type to a JString. This object serves mainly test purposes. 
 */
object StringNodeDescriptor extends NodeDescriptor[String] {
  override def extract(jsonNode: JValue) = {
    def mkString(jValues: Traversable[JValue]): String = {
      (for (fld <- jValues) yield {
        fld match {
          case JString(s) => s
          case JInt   (_) |
               JDouble(_) => fld.extract[String]
          case JBool  (b) => b.toString
          case JArray (_) |
               JObject(_) => "(" + mkString(fld.children) + ")"
          case JField(n,v)=> "(" + n + "," + mkString(List(v)) + ")"
          case JNull      => "Null"
          case JNothing   => "Nothing" 
        }
      }) mkString ","
    }
    if (jsonNode.children.nonEmpty) mkString(jsonNode.children)
    else throw err(EmptyNodeFieldList)
  }
  override def decompose(node: Any) : JValue  = JArray(List(super.decompose(node.toString)))    
  override def id       (node: Any) = node.toString
}
