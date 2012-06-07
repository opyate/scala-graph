package scalax.collection.constrained

import org.scalatest.Suites

import scalax.collection.TEdit

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TEditRootTest
  extends Suites( 
      new TEdit[Graph]          (Graph),
      new TEdit[immutable.Graph](immutable.Graph),
      new TEdit[  mutable.Graph](  mutable.Graph))
