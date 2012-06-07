package scalax.collection.io.edge

object Types {
  type EdgeNodeIds      = (String,String)
  type HyperEdgeNodeIds = List[String]
}
import Types._

sealed abstract class Parameters(nodeIds: Product)
class EdgeParameters(val n1: String, val n2: String) extends Parameters((n1, n2))
object EdgeParameters {
  def unapply(p: EdgeParameters): Option[EdgeNodeIds] = Some((p.n1, p.n2))
}

class WEdgeParameters(val n1:     String,
                      val n2:     String,
                      val weight: Long) extends Parameters((n1, n2))
object WEdgeParameters {
  def unapply(p: WEdgeParameters): Option[(EdgeNodeIds,Long)] = Some((p.n1, p.n2), p.weight)
}

class LEdgeParameters[L](val n1:    String,
                         val n2:    String,
                         val label: L) extends Parameters((n1, n2))
object LEdgeParameters {
  def unapply[L](p: LEdgeParameters[L]): Option[(EdgeNodeIds,L)] = Some((p.n1, p.n2), p.label)
}

class WLEdgeParameters[L](val n1:     String,
                          val n2:     String,
                          val weight: Long,
                          val label:  L) extends Parameters((n1, n2))
object WLEdgeParameters {
  def unapply[L](p: WLEdgeParameters[L]): Option[(EdgeNodeIds,Long,L)] =
    Some((p.n1, p.n2), p.weight, p.label)
}

class HyperEdgeParameters(val nodeIds: HyperEdgeNodeIds) extends Parameters(nodeIds)
object HyperEdgeParameters {
  def unapply(p: HyperEdgeParameters): Option[HyperEdgeNodeIds] = Some(p.nodeIds)
}
class WHyperEdgeParameters(val nodeIds: HyperEdgeNodeIds,
                           val weight:  Long) extends Parameters(nodeIds)
object WHyperEdgeParameters {
  def unapply(p: WHyperEdgeParameters): Option[(HyperEdgeNodeIds,Long)] = Some(p.nodeIds, p.weight)
}
class LHyperEdgeParameters[L](val nodeIds: HyperEdgeNodeIds,
                              val label:   L) extends Parameters(nodeIds)
object LHyperEdgeParameters {
  def unapply[L](p: LHyperEdgeParameters[L]): Option[(HyperEdgeNodeIds,L)] =
    Some(p.nodeIds, p.label)
}
class WLHyperEdgeParameters[L](val nodeIds: HyperEdgeNodeIds,
                               val weight:  Long,
                               val label:   L) extends Parameters(nodeIds)
object WLHyperEdgeParameters {
  def unapply[L](p: WLHyperEdgeParameters[L]): Option[(HyperEdgeNodeIds,Long,L)] =
    Some(p.nodeIds, p.weight, p.label)
}
