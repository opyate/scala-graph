package scalax.collection.io

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._,
       scalax.collection.{Graph, GraphLike, GraphAuxCompanion}
import scalax.collection.generic.GraphFactory

/**
 * Facilitates populating graphs with nodes/edges from JSON text
 * and exporting `Graph`instances to JSON text.
 * 
 * The '''Graph4Scala-JSON-UserGuide''' is available at 
 * [[http://www.assembla.com/spaces/scala-graph/documents]].
 *
 * @define DESCR top level JSON import/export descriptor to be filled with all `NodeDescriptor`s
 *         and `EdgeDescriptors`.
 * @author Peter Empen
 */
package object json {
  import scalax.collection.edge._,
         scalax.collection.edge. WBase._,
         scalax.collection.edge. LBase._,
         scalax.collection.edge.WLBase._
  type Descriptor[N] = descriptor.Descriptor[N] 
  type NodeDescriptor[N] = descriptor.NodeDescriptor[N] 
  type EdgeDescriptorBase[N,E[X]<:EdgeLikeIn[X],+C<:EdgeCompanionBase[E]] = descriptor.EdgeDescriptorBase[N,E,C]
  type   EdgeDescriptor[N,E[X]<:UnDiEdge[X],+C<:EdgeCompanion[E]]                                = descriptor.  EdgeDescriptor[N,E,C] 
  type  WEdgeDescriptor[N,E[X]<:UnDiEdge[X] with  WEdge[X],+C<: WEdgeCompanionBase[E]]           = descriptor. WEdgeDescriptor[N,E,C] 
  type  LEdgeDescriptor[N,E[X]<:UnDiEdge[X] with  LEdge[X],+C<: LEdgeCompanionBase[E],L<:AnyRef] = descriptor. LEdgeDescriptor[N,E,C,L] 
  type WLEdgeDescriptor[N,E[X]<:UnDiEdge[X] with WLEdge[X],+C<:WLEdgeCompanionBase[E],L<:AnyRef] = descriptor.WLEdgeDescriptor[N,E,C,L] 
  type   HyperEdgeDescriptor[N,E[X]<:  HyperEdge[X],+C<:HyperEdgeCompanion[E]]                            = descriptor.  HyperEdgeDescriptor[N,E,C]
  type  WHyperEdgeDescriptor[N,E[X]<: WHyperEdge[X] with  WEdge[X],+C<: WHyperEdgeCompanion[E]]           = descriptor. WHyperEdgeDescriptor[N,E,C] 
  type  LHyperEdgeDescriptor[N,E[X]<: LHyperEdge[X] with  LEdge[X],+C<: LHyperEdgeCompanion[E],L<:AnyRef] = descriptor. LHyperEdgeDescriptor[N,E,C,L] 
  type WLHyperEdgeDescriptor[N,E[X]<:WLHyperEdge[X] with WLEdge[X],+C<:WLHyperEdgeCompanion[E],L<:AnyRef] = descriptor.WLHyperEdgeDescriptor[N,E,C,L] 

  import imp._, imp.Parser.parse, imp.Stream.createStreams
  
  final class JsonGraphCompanion[+G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,G[N,E]]]
             (graphCompanion: GraphAuxCompanion[G])
  {
    /**
     * Creates a new Graph instance and populates it with all nodes/edges found in
     * the node/edge sections of a JSON text.
     * 
     * @param jsonText the JSON text to be parsed for node/edge sections
     * @param descriptor $DESCR 
     * @return new `Graph` instance populated from `jsonText`
     */
    def fromJson [N,E[X] <: EdgeLikeIn[X]](jsonText:   String,
                                           descriptor: Descriptor[N]) =
      fromJson[N,E] (parse(jsonText, descriptor), descriptor)
    /**
     * Creates a new Graph instance and populates it with all nodes/edges found in
     * `jsonLists`.
     * 
     * @param jsonLists node/edge lists usually attained by parsing a JSON text
     * @param descriptor $DESCR 
     * @return new `Graph` instance populated from `jsonText`
     */
    def fromJson [N,E[X] <: EdgeLikeIn[X]](jsonLists:  List[JsonList],
                                           descriptor: Descriptor[N]) = {
      val target = createStreams[N,E] (jsonLists, descriptor)
      graphCompanion.fromStream[N,E] (nodeStreams = target._1, edgeStreams = target._2)
    }
  }
  /** Enables calling `Graph.fromJson` with `Graph` being any Graph companion object.*/
  implicit def graphC2JsonGraphC[G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,G[N,E]]]
              (graphCompanion: GraphFactory[G] with GraphAuxCompanion[G]): JsonGraphCompanion[G] =
    new JsonGraphCompanion[G](graphCompanion)

  final class JsonGraph[N, E[X] <: EdgeLikeIn[X]] (graph: Graph[N,E]) {
    /**
     * Creates a JSON text including all nodes/edges in this graph.
     * 
     * @param descriptor $DESCR 
     * @return the JSON text
     */
    def toJson(descriptor: Descriptor[N]) = {
      val export = new exp.Export[N,E](graph, descriptor)
      import export._
      jsonText(jsonAST(jsonASTNodes ++ jsonASTEdges))
    }
  }
  /** Enables calling `graph.toJson` with `graph` being a Graph instance.*/
  implicit def graph2JsonGraph[N, E[X] <: EdgeLikeIn[X]] (graph: Graph[N,E]): JsonGraph[N,E] =
    new JsonGraph[N,E](graph)
    
  /**
   * Replaces all occurrences of `paramPlaceholder` in source with the elements
   * in `params` one by one. The result is guaranteed not to become longer than
   * `maxLength`.   
   */
  def replacePlaceholders(source:           String,
                          params:           Iterable[String],
                          maxLength:        Int = 50,
                          paramPlaceholder: String = "{}"    ): String = {
    var target = source
    val it = params.iterator
    var i = 0
    while ({i = target.indexOfSlice(paramPlaceholder); i >= 0} &&
           it.hasNext) {
      val param = it.next
      target = target patch (i, if (param.length < maxLength) param
                      else param.substring(0, maxLength - 3) + "...", 2)
    }
    target
  }
}
