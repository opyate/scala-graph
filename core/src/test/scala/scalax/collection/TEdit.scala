package scalax.collection

import org.scalatest.Suite
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import GraphPredef._, GraphEdge._
import generic.GraphFactory

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TEditRootTest
	extends Suites( 
			new TEdit[Graph]			    (Graph),
			new TEdit[immutable.Graph](immutable.Graph),
			new TEdit[  mutable.Graph](  mutable.Graph)
		)
	with ShouldMatchers
{
	// ---------------------------------------- type-specific tests
	def test_DefaultImmutable {
		val g = Graph()
		g.isInstanceOf[immutable.Graph[Nothing,Nothing]] should be (true)
	}
	// ---------------------------------------- mutable tests
	val mutableFactory = mutable.Graph 
  def test_PlusEq {
    val g = mutableFactory(1, 3)
    g += 2
    g should have size (3)
    for (i <- 1 to 3)
      g.contains(i) should be (true) //g should contain (i)
  }
  def test_MinusEq {
    val g = mutableFactory(1, 2, 2~3, 4)
    g remove 1 should be (true)
    g          should be (mutableFactory(2~3, 4))
    g remove 5 should be (false)
    g -?  2    should be (g)
    (g -?= 2)  should be (g)
    (g -= 2)   should be (mutableFactory(3, 4))
    g.clear
    g          should be ('empty)
	}
  def test_PlusEdgeEq {
    val g = mutableFactory(2~3)
    def n(i: Int) = g get i
    implicit val unDiFactory = UnDiEdge 
    g addEdge (n(3), n(2))  should be (false)
    (g +~=    (n(2), n(2))) should have size (4)

    g.addAndGetEdge(2, 3)(DiEdge).directed should be (true)
    g should have ('order (2), 'graphSize (3))

    n(3) +~ (4)
    g should have ('order (3), 'graphSize (4))

    (n(3) +~ n(2))(DiEdge)
    g should have ('order (3), 'graphSize (5))
  }
  def test_PlusHyperEdgeEq {
    implicit val factory = HyperEdge
    val h = mutableFactory(1~1~2)
    h should have ('order (2), 'graphSize (1))
    h +~= (0, 1, 2, 3)
    h should have ('order (4), 'graphSize (2))
  }
  def test_PlusWEdgeEq {
    val g = mutableFactory(2~3)
    implicit val f = edge.WUnDiEdge
    g.addWEdge (3,4)(2)
    g should have ('order (3), 'graphSize (2), 'totalWeight (3))
    (g +~%= (2,4))(3)
    g should have ('order (3), 'graphSize (3), 'totalWeight (6))
    // (g +~%= (0,1,2,3))(3)(edge.WHyperEdge) // must not compile
  }
  def test_PlusWHyperEdgeEq {
    implicit val factory = edge.WHyperEdge
    val h = mutableFactory(1~1~2)
    h should have ('order (2), 'graphSize (1))
    h.addWEdge (3,4,5)(2)
    h should have ('order (5), 'graphSize (2), 'totalWeight (3))
    (h +~%= (0,1,2,3))(3)
    h should have ('order (6), 'graphSize (3), 'totalWeight (6))
  }
  def test_PlusLEdgeEq {
    import edge.Implicits._
    import edge.{LUnDiEdge, LDiEdge}

    type StringLabel = Option[String]
    val str = "A"
    val label: StringLabel = Some(str)
    val g = mutableFactory(2~3, (2 ~+# 3)(label))

    import edge.LBase.{LEdgeImplicits}
    object StringLabelImplicit extends LEdgeImplicits[StringLabel]
    import StringLabelImplicit._
    for (e <- g.edges if e.isLabeled) {
      e.isDefined should be (true)
      e.get       should be (str)
    }

    type ListLabel = List[Int]
    object ListLabelImplicit extends LEdgeImplicits[ListLabel]
    import ListLabelImplicit._
    implicit val factory = LDiEdge
    val listLabel = List(1,0,1)
    g.addLEdge(3,4)(listLabel) should be (true)
    g should have ('order (3), 'graphSize (3))
    val findAdded = g.edges find (3~>4)
    findAdded should be ('isDefined)
    val added: g.EdgeT = findAdded.get
    added.directed should be (true)
    added.count(_ > 0) should be (List(1,0,1).count(_ > 0))
  }
  def test_PlusLHyperEdgeEq {
    import edge.Implicits._
    import edge.{LHyperEdge, LDiHyperEdge}

    type StringLabel = String
    val outerLabels = Seq("A", "BC", "CDE")
    val g = mutableFactory(1~2~3, (2 ~+# 3)(outerLabels(0)))

    implicit val factory = LHyperEdge
    (g +~+= (3,4,5))(outerLabels(1))
    g should have ('order (5), 'graphSize (3))
    g.addLEdge(4,5,6)(outerLabels(2)) should be (true)
    g should have ('order (6), 'graphSize (4))

    import edge.LBase.{LEdgeImplicits}
    object StringLabelImplicit extends LEdgeImplicits[StringLabel]
    import StringLabelImplicit._
    val innerLabels: collection.mutable.Set[_ >: StringLabel] =
      g.edges filter (_.isLabeled) map (_.label)
    innerLabels should have size (outerLabels.size)
    innerLabels forall (outerLabels contains _) should be (true) 
  }
  def test_pluPlusEq {
    val (gBefore, gAfter) = (mutableFactory(1, 2~3), mutableFactory(0, 1~2, 2~3)) 
    (gBefore ++= List[GraphParam[Int,UnDiEdge]](1~2, 2~3, 0)) should equal (gAfter)
    (gBefore ++= mutableFactory(0, 1~2))                      should equal (gAfter)
    (gBefore ++= mutableFactory(0) ++= mutableFactory(1~2))   should equal (gAfter)
  }
}
/**	This class contains tests for graph editing to be run for Graph instances created
 *	by the Graph factory and passed to the constructor. For instance,
 *	this allows the same tests to be run for mutable and immutable Graphs.
 */
class TEdit[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC[N,E]]]
			(val factory: GraphFactory[CC])
	extends	Suite
	with	ShouldMatchers
{
	// ---------------------------------------- tests common to any kind of graph
	val seq_1_3	 = Seq(1, 3)
	val gInt_1_3 = factory(seq_1_3: _*)
	val gString_A = factory("A")

	def test_0(info : Informer) {
		info("factory = " + factory.getClass)
	}
	def test_Empty {
		val eg = factory.empty 
		eg should be ('isEmpty)
		eg should have size (0)
		eg should equal (eg.empty)
	}
	def test_Factory {
		gInt_1_3 should not be ('isEmpty)
		gInt_1_3 should have size (2)
		gInt_1_3(0) should be (false)
		gInt_1_3(1) should be (true)
		gInt_1_3(2) should be (false)
		gInt_1_3(3) should be (true)

		val r = HyperEdge(1, 2, 3)
		val g = factory(1, r, 1~2)
		g.nodes	should have size (3)
		g.edges	should have size (2)
		g			  should have size (5)
		g.contains(r) should be (true)
		
		val h = factory(UnDiEdge(1, 2), UnDiEdge(2, 3))
		h.nodes	should have size (3)
		h.edges	should have size (2)
		h	  		should have size (5)
	}
	def test_Constructor {
	  val (n_start, n_end) = (11, 20)
	  val nodes = List.range(n_start, n_end)
	  val edges = List[DiEdge[Int]](14~>16, 16~>18, 18~>20, 20~>22)
	  val g = factory.from(nodes, edges)
	  g.nodes.size should be (nodes.size + 2)
	  g.edges.size should be (edges.size)
	}
	def test_Contain {
		seq_1_3 foreach {
			i => gInt_1_3.contains(i) should be (true) //gInt_1_3 should contain (i)
		}
		gInt_1_3.head.isInstanceOf[NodeOut[Int]] should be (true)
	}
	def test_toString {
	  val nodePrefix = NodeIn.stringPrefix
		gInt_1_3 .toString should fullyMatch regex ("""Graph\(""" + nodePrefix + """[13], """ + nodePrefix + """[13]\)""")
		gString_A.toString should fullyMatch regex ("""Graph\(""" + nodePrefix + """["A"]\)""")
	}
	def test_plusInt {
	  val g = factory(1, 2~3)
	  g + 1   should be (g)
	  g + 0   should be (Graph(0, 1, 2, 3, 2~3))
	  g + 0~1 should be (Graph(0, 1, 2, 3, 0~1, 2~3))
	  //g + "A" !!! // error: type mismatch
	}
	def test_PlusString {
		val g = gString_A + "B"
		g should have size (2)
		g.contains("A") should be (true) //g should contain ("A")
		g.contains("B") should be (true) //g should contain ("B")

		val hString_A = factory[String, UnDiEdge]("A")
		val h = hString_A + ("A"~"C")
		h.nodes		should have size (2)
		h.edges	should have size (1)
		h			should have size (3)
	}
	def test_PlusPlus {
		val g = gString_A + "B" + "C"
		g should have size (3)
		g.contains("A") should be (true) //g should contain ("A")
		g.contains("B") should be (true) //g should contain ("B")
		g.contains("C") should be (true) //g should contain ("C")

		val (gBefore, gAfter) = (factory(1, 2~3), factory(0, 1~2, 2~3)) 
		gBefore ++ List[GraphParam[Int,UnDiEdge]](1~2, 2~3, 0) should equal (gAfter)
		gBefore ++ factory(0, 1~2)                             should equal (gAfter)
    gBefore ++ factory(0) ++ factory(1~2)                  should equal (gAfter)
	}
	def test_Minus {
		var g = gString_A - "B"
		g should have size (1)

		g = gString_A - "A"
		g.contains("A") should be (false) //gMinus should not contain ("A")
		g should have size (0)
		g should be ('isEmpty)

		val h = factory(1, 2, 2~3)
		h  - 0 should be (h)
		h  - 1 should be (factory(2, 2~3))
    h -? 2 should be (h)
    h  - 2 should be (factory(1, 3))
	}
	def test_MinusMinus {
    val g = factory(1, 2~3, 3~4)
	  g --  List(2, 3~3) should be (factory(1, 3~4))
    g --  List(2, 3~4) should be (factory(1, 3, 4))
	  g --! List(1, 3~4) should be (factory(2~3))
	}
	def test_MapToIntNodes {
	  // in absence of NodeIn m would be of type Set[Int]
		val m = gInt_1_3 map {case n: NodeOut[Int] => NodeIn(n.value + 1)
		                      case _ => fail("Test assumption did not hold.")}
		m.isInstanceOf[Graph[Int,Nothing]] should be (true)
		m should have size (2)
		m.contains(2) should be (true) //m should contain (2)
		m.contains(4) should be (true) //m should contain (4)
	}
	def test_MapToStringNodes {
		val m = gInt_1_3 map {case NodeOut(n) => NodeIn(n.value.toString + ".")
                          case _ => fail("Test assumption did not hold.")}
		m.isInstanceOf[Graph[String,Nothing]] should be (true)
		m should have size (2)
		m.contains("1.") should be (true) //m should contain ("1.")
		m.contains("3.") should be (true) //m should contain ("3.")
	}
	def test_Eq {
		factory() should be === factory()
		gInt_1_3	should be === factory(seq_1_3: _*)
		gString_A	should be === factory("A")

		factory()	should not be === (factory(1))
		gInt_1_3	should not be === (factory(2, 3))
		gString_A	should not be === (factory("B"))

		gInt_1_3	should be === (factory(1) + 3) 
	}
	def test_EdgeAssoc {
    val e = 1 ~ 2 
    e.isInstanceOf[UnDiEdge[Int]] should be (true)
    val x = factory[Int,UnDiEdge](3~4).nodes
    // Error in Scala compiler: assertion failed
    // Graph(3).nodes contains 3 //should be (true)  

    val d = 1 ~> 2 
    d.isInstanceOf[DiEdge[Int]] should be (true)
    d.source should be (1)
    d.target should be (2)
    factory(1, d, 1~4).nodes should have size (3)

    val heNodes = List("A", "B", "C")
    val he = heNodes(0) ~ heNodes(1) ~ heNodes(2)
    he.isInstanceOf[HyperEdge[String]] should be (true)
    he.arity should be (heNodes.size)
    he._1    should be (heNodes(0))  
    he._2    should be (heNodes(1))
    for (i <- 0 to (heNodes.size - 1))
      he._n(i) should be (heNodes(i))  

    val dhe = "A" ~> "B" ~> "C" ~> "D"
    dhe.isInstanceOf[DiHyperEdge[String]] should be (true)
    dhe.arity should be (4)
    dhe._1    should be ("A")  
    dhe._n(3) should be ("D")
	}
  def test_diSuccessorsUnDi {
    val g = factory (1~1, 1~2, 1~3, 1~4)
    (g get 1 diSuccessors) should be (Set(2, 3, 4))
    (g get 2 diSuccessors) should be (Set(1))
  }
  def test_diSuccessorsDi {
    val g = factory (1~>1, 1~>2, 1~>3, 1~>4)
    (g get 1 diSuccessors) should be (Set(2, 3, 4))
    (g get 2 diSuccessors) should be (Set.empty)
  }
  def test_diSuccessorsDiHyper {
    val h = factory (1~>1~>5, 1~>2~>5, 1~>3~>5, 1~>4~>9)
    (h get 1 diSuccessors) should be (Set(2, 3, 4, 5, 9))
    (h get 2 diSuccessors) should be (Set(5))
    (h get 5 diSuccessors) should be (Set.empty)
  }
  def test_diPredecessorsUnDi {
    val g = factory (1~1, 1~2, 1~3, 1~4)
    (g get 1 diPredecessors) should be (Set(2, 3, 4))
    (g get 2 diPredecessors) should be (Set(1))
  }
  def test_diPredecessorsDi {
    val g = factory (1~>1, 1~>2, 1~>3, 1~>4)
    (g get 1 diPredecessors) should be (Set.empty)
    (g get 2 diPredecessors) should be (Set(1))
  }
  def test_diPredecessorsDiHyper {
    val h = factory (1~>1~>5, 1~>2~>5, 1~>3~>5, 1~>4~>9)
    (h get 1 diPredecessors) should be (Set.empty)
    (h get 2 diPredecessors) should be (Set(1))
    (h get 5 diPredecessors) should be (Set(1,2,3))
  }
  def test_neighborsUnDi {
    val g = factory (1~1, 1~2, 1~3, 1~4)
    (g get 1 neighbors) should be (Set(2, 3, 4))
    (g get 2 neighbors) should be (Set(1))
  }
  def test_neighborsDi {
    val g = factory (1~>1, 1~>2, 1~>3, 1~>4)
    (g get 1 neighbors) should be (Set(2,3,4))
    (g get 2 neighbors) should be (Set(1))
  }
  def test_neighborsDiHyper {
    val h = factory (1~>1~>5, 1~>2~>5, 1~>3~>5, 1~>4~>9)
    (h get 1 neighbors) should be (Set(2,3,4,5,9))
    (h get 2 neighbors) should be (Set(1,5))
    (h get 5 neighbors) should be (Set(1,2,3))
  }
	def test_degree {
	  val g = factory (1~1, 1~2, 1~3, 1~4 )
	  (g get 1 degree) should be (5)
    (g get 2 degree) should be (1)
	}
  def test_incoming {
    val uEdges = Seq[UnDiEdge[Int]](1~1, 1~2, 1~3, 1~4) // bug if no type param given
    val g = factory (uEdges(0), uEdges(1), uEdges(2), uEdges(3))
    (g get 1 incoming) should be (uEdges.toSet)
    (g get 2 incoming) should be (Set(uEdges(1)))
    
    val dEdges = Seq[DiEdge[Int]](1~>1, 1~>2, 1~>3, 1~>4)
    val h = factory (dEdges(0), dEdges(1), dEdges(2), dEdges(3))
    (h get 1 incoming) should be (Set(dEdges(0)))
    (h get 2 incoming) should be (Set(dEdges(1)))
  }
  def test_edgeAdjacents_UnDi_1 {
    val g = Graph(1~2, 2~3, 1~>3, 1~5, 3~5, 3~4, 4~>4, 4~>5)
    ((g get 4~>4) adjacents) should be (Set(3~4, 4~>5))
    ((g get 1~2)  adjacents) should be (Set(1~>3, 1~5, 2~3))
  }
  def test_predicate {
    val g = factory(2~>3, 3~>1, 5)
    // does not compile because of some problem with factory / implicit def
//  g  filter ((n: Int) => n > 1) should be (factory(2~>3, 5))
//  g  filter ((n: Int) => n < 2) should be (factory(1))
    g  filter g.having(node = _ <  2) should be (factory(1))
    g  filter g.having(node = _ >= 2) should be (factory(2~>3, 5))
    g  filter g.having(edge = _._1 == 2)    should be (factory(2~>3))
    g  filter g.having(edge = _ contains 2) should be (factory(2~>3))
  }
}