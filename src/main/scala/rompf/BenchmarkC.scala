package rompf

import scala.collection.mutable.ListBuffer
import scala.testing.Benchmark

/* Initially retrieved from http://lamp.epfl.ch/~rompf/vector2/ */
object SettingsC {
//  val N = 32 * 1024
  val N = 1000000
  override def toString = "[N="+N+"]"
}

object BenchCListHeadTail extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsC

  def run() {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsC.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

}

object BenchCArrayIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsC

  def run() {
    val x: AnyRef = "x"
    val buf = new java.util.ArrayList[AnyRef](SettingsC.N)
    var i = 0
    while (i < SettingsC.N) {
      buf.add(x)
      i += 1
    }
    buf.iterator()
  }

}

object BenchCVectorHeadTail extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsC

  def run() {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsC.N) {
      buf += x
      i += 1
    }
    buf.resultList
  }

}

object BenchCVectorForeach extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsC

  def run() {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsC.N) {
      buf += x
      i += 1
    }
    buf.result
  }

}


object BenchCVectorIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsC

  def run() {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsC.N) {
      buf += x
      i += 1
    }
    buf.result.pointer(0)
  }

}