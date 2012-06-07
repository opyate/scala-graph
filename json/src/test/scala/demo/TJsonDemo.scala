package demo

import org.scalatest.Suite
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import scalax.collection._
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
import scalax.collection.edge._,
       scalax.collection.edge.Implicits._

import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.{DiHyper, Di}

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TJsonDemoTest
	extends	Suite
	with	ShouldMatchers
{
  val (programming, inDepth) = (
      Book("Programming in Scala", "978-0-9815316-2-5"),
      Book("Scala in Depth",       "978-1-9351827-0-2")
  )
  val (martin, lex, bill, josh) = (
      Author("Odersky", "Martin"),
      Author("Spoon", "Lex"),
      Author("Venners", "Bill"),
      Author("Suereth", "Joshua D.")
  )
  /*
   * Directed edges/hyperedges denote book-author-relationships.
   */
  val library = Graph[Library,HyperEdge](
                  programming ~> martin ~> lex ~> bill,
                  inDepth ~> josh
  )
  val bookDescriptor = new NodeDescriptor[Book](typeId = "Books") {
    def id(node: Any) = node match {
      case Book(_, isbn) => isbn
    }
  }
  val authorDescriptor = new NodeDescriptor[Author](typeId = "Authors"){
    def id(node: Any) = node match {
      case Author(surName, firstName) => "" + surName(0) + firstName(0) }
    }
  def test_export_1 {
    val quickJson = new Descriptor[Library](
      defaultNodeDescriptor = authorDescriptor,
      defaultEdgeDescriptor = DiHyper.descriptor[Library]()
    )
    val caught = 
      intercept[scalax.collection.io.json.error.JsonGraphError.JsonGraphException] {
        library.toJson(quickJson)
    }
    caught.msg should be (
      """No 'NodeDescriptor' capable of processing type "demo.Book" found.""")
  }
  def test_export_2 {
    val quickJson = new Descriptor[Library](
        defaultNodeDescriptor = authorDescriptor,
        defaultEdgeDescriptor = DiHyper.descriptor[Library](),
        namedNodeDescriptors  = Seq(bookDescriptor)
    )
    val caught = 
      intercept[scalax.collection.io.json.error.JsonGraphError.JsonGraphException] {
        library.toJson(quickJson)
    }
    caught.msg should be (
      """No 'EdgeDescriptor' capable of processing type "scalax.collection.GraphEdge$DiEdge" found."""
    )
  }
  object Named {
    val descriptor = new Descriptor[Library](
        defaultNodeDescriptor = authorDescriptor,
        defaultEdgeDescriptor = DiHyper.descriptor[Library](),
        namedNodeDescriptors  = Seq(bookDescriptor),
        namedEdgeDescriptors  = Seq(Di.descriptor[Library]())
    )
  }
  def test_export_named {
    val exported = library.toJson(Named.descriptor)

    import net.liftweb.json._
    val pretty = Printer.pretty(JsonAST.render(JsonParser.parse(exported)))
    //println(pretty)
    /*
      {
        "nodes":{
          "Books":[{
            "title":"Programming in Scala",
            "isbn":"978-0-9815316-2-5"
          },{
            "title":"Scala in Depth",
            "isbn":"978-1-9351827-0-2"
          }]
        },
        "nodes":[{
          "surName":"Odersky",
          "firstName":"Martin"
        },{
          "surName":"Spoon",
          "firstName":"Lex"
        },{
          "surName":"Suereth",
          "firstName":"Joshua D."
        },{
          "surName":"Venners",
          "firstName":"Bill"
        }],
        "edges":{
          "DiEdge":[{
            "n1":"978-1-9351827-0-2",
            "n2":"SJ"
          }]
        },
        "edges":[{
          "nodeIds":["978-0-9815316-2-5","OM","SL","VB"]
        }]
      }
     */
  }
  def test_imEx_named {
    val expLibrary = library.toJson(Named.descriptor)
    Graph.fromJson(expLibrary, Named.descriptor) should equal (library)
  }
  object LibraryPositionedNodeDescriptor {
    import net.liftweb.json._
    private final class AuthorSerializer extends Serializer[Author] { 
      private val clazz = classOf[Author] 
      override def deserialize(implicit format: Formats) = { 
        case (TypeInfo(clazz, _), json) => json match { 
          case JArray(JString(surName) :: JString(firstName) :: Nil) => 
            Author(surName, firstName) 
          case x => throw new MappingException(
                    "Can't convert " + x + " to " + clazz.getSimpleName) 
        } 
      } 
      override def serialize(implicit format: Formats) = { 
        case Author(surName, firstName) =>
          JArray(JString(surName) :: JString(firstName) :: Nil)  
      } 
    } 
    val author = new NodeDescriptor[Author](
                     typeId            = "Authors",
                     customSerializers = Seq(new AuthorSerializer)){
      def id(node: Any) = node match {
        case Author(surName, firstName) => "" + surName(0) + firstName(0) }
      }
    private final class BookSerializer extends Serializer[Book] { 
      private val clazz = classOf[Book] 
      override def deserialize(implicit format: Formats) = { 
        case (TypeInfo(clazz, _), json) => json match { 
          case JArray(JString(title) :: JString(isbn) :: Nil) => Book(title, isbn) 
          case x => throw new MappingException(
                    "Can't convert " + x + " to " + clazz.getSimpleName) 
        } 
      } 
      override def serialize(implicit format: Formats) = { 
        case Book(title, isbn) => JArray(JString(title) :: JString(isbn) :: Nil)  
      } 
    } 
    val book = new NodeDescriptor[Book](
                   typeId            = "Books",
                   customSerializers = Seq(new BookSerializer)) {
      def id(node: Any) = node match {
        case Book(_, isbn) => isbn
      }
    }
  }
  object Positioned {
    import scalax.collection.io.json.serializer.{HyperEdgeSerializer, EdgeSerializer}
    val descriptor = new Descriptor[Library](
        defaultNodeDescriptor = LibraryPositionedNodeDescriptor.author,
        defaultEdgeDescriptor = DiHyper.descriptor[Library](Some(new HyperEdgeSerializer)),
        namedNodeDescriptors  = Seq(LibraryPositionedNodeDescriptor.book),
        namedEdgeDescriptors  = Seq(Di.descriptor[Library](Some(new EdgeSerializer)))
    )
  }
  def test_export_positioned {
    val exported = library.toJson(Positioned.descriptor)

    import net.liftweb.json._
    val pretty = Printer.pretty(JsonAST.render(JsonParser.parse(exported)))
    //println(pretty)
    /*
      {
        "nodes":{
          "Books":[["Programming in Scala","978-0-9815316-2-5"],["Scala in Depth","978-1-9351827-0-2"]]
        },
        "nodes":[["Odersky","Martin"],["Spoon","Lex"],["Suereth","Joshua D."],["Venners","Bill"]],
        "edges":{
          "DiEdge":[["978-1-9351827-0-2","SJ"]]
        },
        "edges":[["978-0-9815316-2-5","OM","SL","VB"]]
      }
     */
  }
  def test_imEx_positioned {
    val expLibrary = library.toJson(Positioned.descriptor)
    Graph.fromJson(expLibrary, Positioned.descriptor) should equal (library)
  }
}
// --------------------------------- node types of academic library application
sealed trait Library 
case class Book  (val title: String, val isbn: String) extends Library
case class Author(val surName: String, val firstName: String) extends Library