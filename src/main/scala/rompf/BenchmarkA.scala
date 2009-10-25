package rompf

import scala.collection.mutable.ListBuffer
import scala.testing.Benchmark

/* Initially retrieved from http://lamp.epfl.ch/~rompf/vector2/ */
object SettingsA {
//  val N = 32*1024
  val N = 1000000
  override def toString = "[N="+N+"]"
}

object BenchAListHeadTail extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

  def run() {
    var list: List[AnyRef] = buf
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}

object BenchAListIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

  def run() {
    var it = buf.iterator
    while (it.hasNext) {
      if (it.next() eq null)
        println("strange element")
    }
  }

}

object BenchAListForeach extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

  def run() {
    var list: List[AnyRef] = buf
    for (x <- list) {
      if (x eq null)
        println("strange element")
    }
  }

}


object BenchAArrayIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new java.util.ArrayList[AnyRef](SettingsA.N)
    var i = 0
    while (i < SettingsA.N) {
      buf.add(x)
      i += 1
    }
    buf
  }
  
  def run() {
    var it = buf.iterator()
    while (it.hasNext) {
      if (it.next() eq null)
        println("strange element")
    }
  }

}

object BenchAArrayIndexed extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new java.util.ArrayList[AnyRef](SettingsA.N)
    var i = 0
    while (i < SettingsA.N) {
      buf.add(x)
      i += 1
    }
    buf
  }
  
  def run() {
    val it = buf
    var i = 0
    while (i < it.size) {
      if (it.get(i) eq null)
        println("strange element")
      i += 1
    }
  }

}

object BenchARawArrayIndexed extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new Array[AnyRef](SettingsA.N)
    var i = 0
    while (i < SettingsA.N) {
      buf(i) = x
      i += 1
    }
    buf
  }
  
  def run() {
    val it = buf
    var i = 0
    while (i < it.length) {
      if (it(i) eq null)
        println("strange element")
      i += 1
    }
  }

}

object BenchARawArrayForeach extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new Array[AnyRef](SettingsA.N)
    var i = 0
    while (i < SettingsA.N) {
      buf(i) = x
      i += 1
    }
    buf
  }
  
  def run() {
    val it = buf
    it.foreach(x => if (x eq null) println("strange element"))
  }
}

object BenchARawArrayForeachMega extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new Array[AnyRef](SettingsA.N)
    var i = 0
    while (i < SettingsA.N) {
      buf(i) = x
      i += 1
    }
    buf.foreach(x => if (x == "mmm") println("strange element"))
    buf.foreach(x => if (x == "yyy") println("strange element"))
    buf.foreach(x => if (x == "sss") println("strange element"))
    buf
  }
  
  def run() {
    val it = buf
    it.foreach(x => if (x eq null) println("strange element"))
  }
}

object BenchARawArrayIte extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new Array[AnyRef](SettingsA.N)
    var i = 0
    while (i < SettingsA.N) {
      buf(i) = x
      i += 1
    }
    buf
  }
  
  def run() {
    val it = buf.iterator
    while (it.hasNext)
      if (it.next eq null) println("strange element")
  }
}

object BenchAVectorForeach extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  def run() {
    var list: SPMVectorState[AnyRef] = buf
    for (x <- list) {
      if (x eq null)
        println("strange element")
    }
  }

}

object BenchAVectorForeachFast extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  def run() {
    var list: SPMVectorState[AnyRef] = buf
    list.foreachFast { x =>
      if (x eq null)
        println("strange element")
    }
  }

}

object BenchAVectorForeachFastProtect extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  def run() {
    var list: SPMVectorState[AnyRef] = buf
    list.foreachFast(new Function[AnyRef, Unit] {
      @noinline def apply(x: AnyRef) = {
        if (x eq null)
          println("strange element")
      }
    })
  }

}


object BenchAVectorIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  def run() {
    var it: SPMVectorIterator[AnyRef] = buf.pointer(0)
    while (it.hasNext) {
      if (it.next() eq null)
        println("strange element")
    }
  }

}

object BenchAVectorIndexed extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  def run() {
    val it: SPMVectorState[AnyRef] = buf
    var i = 0
    while (i < it.endIndex) {
      if (it.apply(i) eq null)
        println("strange element")
      i += 1
    }
  }

}

object BenchAVectorIndexedFast extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  def run() {
    val it: SPMVectorState[AnyRef] = buf
    var i = 0
    while (i < it.endIndex) {
      if (it.applyFast(i) eq null)
        println("strange element")
      i += 1
    }
  }

}


object BenchAVectorHeadTail extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf
  }
  
  def run() {
    var list: SPMVectorList[AnyRef] = buf.resultList // to get *any* output
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}


object BenchAVectorHeadTailAlt extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.resultListAlt
  }
  
  def run() {
    var list: SPMVectorListAlt[AnyRef] = buf
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}

object BenchAVectorHeadTailBlt extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.resultListBlt
  }
  
  def run() {
    var list: SPMVectorListBlt[AnyRef] = buf
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}

object BenchAVectorHeadTailClt extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.resultListClt
  }
  
  def run() {
    var list: SPMVectorListClt[AnyRef] = buf
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}

object BenchAVectorHeadTailStub extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf.resultListStub
  }
  
  def run() {
    var list: SPMVectorListStub[AnyRef] = buf
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}

object BenchAVectorHeadTailEsc extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsA

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsA.N) {
      buf += x
      i += 1
    }
    buf
  }
  
  def run() {
    var list: SPMVectorListEsc[AnyRef] = buf.resultListEsc
    while (!list.isEmpty) {
      if (list.head eq null)
        println("strange element")
      list = list.tail
    }
  }

}