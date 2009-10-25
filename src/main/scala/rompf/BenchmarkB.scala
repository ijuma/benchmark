package rompf

import scala.collection.mutable.ListBuffer
import scala.testing.Benchmark

/* Initially retrieved from http://lamp.epfl.ch/~rompf/vector2/ */
object SettingsB {
  val N = 1024
  val N2 = 1024
//  val N = 100000
//  val N2 = 32
  override def toString = "[N="+N+",N2="+N2+"]"
}

object BenchBListHeadTail extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

  val buf2 = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsB.N2) {
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
      var list2: List[AnyRef] = buf2
      while (!list2.isEmpty) {
        if (list2.head eq null)
          println("strange element")
        list2 = list2.tail
      }
      list2 = buf2
      while (!list2.isEmpty) {
        if (list2.head eq null)
          println("strange element")
        list2 = list2.tail
      }
      list2 = buf2
      while (!list2.isEmpty) {
        if (list2.head eq null)
          println("strange element")
        list2 = list2.tail
      }
      list = list.tail
    }
  }

}

object BenchBListIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

  val buf2 = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsB.N2) {
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
      var it2 = buf2.iterator
      while (it2.hasNext) {
        if (it2.next() eq null)
          println("strange element")
      }
      it2 = buf2.iterator
      while (it2.hasNext) {
        if (it2.next() eq null)
          println("strange element")
      }
      it2 = buf2.iterator
      while (it2.hasNext) {
        if (it2.next() eq null)
          println("strange element")
      }
    }
  }

}

object BenchBListForeach extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.toList
  }

  val buf2 = {
    val x: AnyRef = "x"
    val buf = new ListBuffer[AnyRef]
    var i = 0
    while (i < SettingsB.N2) {
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
      var list2: List[AnyRef] = buf2
      for (x2 <- list2) {
        if (x2 eq null)
          println("strange element")
      }
      list2 = buf2
      for (x2 <- list2) {
        if (x2 eq null)
          println("strange element")
      }
      list2 = buf2
      for (x2 <- list2) {
        if (x2 eq null)
          println("strange element")
      }
    }
  }

}



object BenchBVectorForeach extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  val buf2 = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N2) {
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
        var list2: SPMVectorState[AnyRef] = buf2
        for (x2 <- list2) {
          if (x2 eq null)
            println("strange element")
        }
        list2 = buf2
        for (x2 <- list2) {
          if (x2 eq null)
            println("strange element")
        }
        list2 = buf2
        for (x2 <- list2) {
          if (x2 eq null)
            println("strange element")
        }
    }
  }

}

object BenchBVectorForeachFast extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  val buf2 = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N2) {
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
        var list2: SPMVectorState[AnyRef] = buf2
        list2.foreachFast { x2 =>
          if (x2 eq null)
            println("strange element")
        }
        list2 = buf2
        list2.foreachFast { x2 =>
          if (x2 eq null)
            println("strange element")
        }
        list2 = buf2
        list2.foreachFast { x2 =>
          if (x2 eq null)
            println("strange element")
        }
    }
  }

}

object BenchBVectorForeachFastProtect extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  val buf2 = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N2) {
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
            var list2: SPMVectorState[AnyRef] = buf2
            list2.foreachFast(new Function[AnyRef, Unit] {
              @noinline def apply(x2: AnyRef) = {
                if (x2 eq null)
                  println("strange element")
              }
            })
            list2 = buf2
            list2.foreachFast(new Function[AnyRef, Unit] {
              @noinline def apply(x2: AnyRef) = {
                if (x2 eq null)
                  println("strange element")
              }
            })
            list2 = buf2
            list2.foreachFast(new Function[AnyRef, Unit] {
              @noinline def apply(x2: AnyRef) = {
                if (x2 eq null)
                  println("strange element")
              }
            })
        }
      }
    )
  }

}


object BenchBVectorIter extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.result
  }
  
  val buf2 = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N2) {
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
      var it2: SPMVectorIterator[AnyRef] = buf2.pointer(0)
      while (it2.hasNext) {
        if (it2.next() eq null)
          println("strange element")
      }
      it2 = buf2.pointer(0)
      while (it2.hasNext) {
        if (it2.next() eq null)
          println("strange element")
      }
      it2 = buf2.pointer(0)
      while (it2.hasNext) {
        if (it2.next() eq null)
          println("strange element")
      }
    }
  }
}

object BenchBVectorIndexedFast extends Benchmark {
  override def prefix = super.prefix + "/" + SettingsB

  val buf = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N) {
      buf += x
      i += 1
    }
    buf.result
  }

  val buf2 = {
    val x: AnyRef = "x"
    val buf = new SPMVectorBuilder[AnyRef]()
    var i = 0
    while (i < SettingsB.N2) {
      buf += x
      i += 1
    }
    buf.result
  }

  def run() {
    var it: SPMVectorState[AnyRef] = buf
    var i = 0
    while (i < it.endIndex) {
      if (it.applyFast(i) eq null)
        println("strange element")
      var it2: SPMVectorState[AnyRef] = buf2
      var i2 = 0
      while (i2 < it2.endIndex) {
        if (it2.applyFast(i2) eq null)
          println("strange element")
        i2 += 1
      }
      it2 = buf2
      i2 = 0
      while (i2 < it2.endIndex) {
        if (it2.applyFast(i2) eq null)
          println("strange element")
        i2 += 1
      }
      it2 = buf2
      i2 = 0
      while (i2 < it2.endIndex) {
        if (it2.applyFast(i2) eq null)
          println("strange element")
        i2 += 1
      }
      i += 1
    }
  }
}

