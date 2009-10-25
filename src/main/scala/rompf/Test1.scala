//-...
package rompf

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import scala.testing.Benchmark

/* Initially retrieved from http://lamp.epfl.ch/~rompf/vector2/ */

object Vector {
//  def empty[V]: Vector[V] = new EmptyVector[V](10)
}


final class SPMVectorState[+A](startIndex: Int, val endIndex: Int, depth: Int, val elems: Array[AnyRef]) {
  
  def foreach(f: A => Unit): Unit = {
    foreach0(0, (depth-1) * 5, elems)(f)
  }
  def foreach0(index: Int, level: Int, data: Array[AnyRef])(f: A => Unit): Unit = {
    if (data ne null) {
      if (level == 0) {
        var i = 0
        while (i < 32) {
          f(data(i).asInstanceOf[A])
          i += 1
        }
      } else {
        var i = 0
        while (i < 32) {
          foreach0(index + (i << level), level - 5, data(i).asInstanceOf[Array[AnyRef]])(f)
          i += 1
        }
      }
    }
  }

  def foreachIter(f: A => Unit): Unit = {
    var it = pointer(0)
    while (it.hasNext) {
      f(it.next())
    }
  }

  final def gotoZero(display: Array[Array[AnyRef]]) = (depth - 1) match { // goto pos zero
    case 5 =>
      display(5) = elems
      display(4) = display(5)(0).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)(0).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 4 =>
      display(4) = elems
      display(3) = display(4)(0).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 3 =>
      display(3) = elems
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 2 =>
      display(2) = elems
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 1 =>
      display(1) = elems
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 0 =>
      display(0) = elems
  }


  final def gotoPos(index: Int, xor: Int, display: Array[Array[AnyRef]]) = {
    if (xor < (1 << 10)) { // level = 1
      display(0) = display(1)((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 15)) { // level = 2
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 20)) { // level = 3
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 25)) { // level = 4
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 30)) { // level = 5
      display(4) = display(5)((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)(0).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    }// else { // level = 6
  }

  @inline final def foreachFast(f: A => Unit): Unit = {
    val display = new Array[Array[AnyRef]](6)

    ArrayHelper.gotoZero(display, depth, elems)
//    gotoZero(display)
    
    var blockIndex = 0
    var nextBlockIndex = blockIndex + 32
    while (endIndex >= nextBlockIndex) {
      val data = display(0)
      var lo = 0
      while (lo < 32) {
        f(data(lo).asInstanceOf[A])
        lo += 1
      }

      val xor = blockIndex ^ nextBlockIndex
      
      blockIndex = nextBlockIndex
      
      if (nextBlockIndex < endIndex) {
        ArrayHelper.gotoPos(nextBlockIndex, xor, display)
//        gotoPos(nextBlockIndex, xor, display)
      }
      
      nextBlockIndex += 32
    }
    
    val data = display(0)
    val endLo = endIndex - blockIndex // is < 32
    var lo = 0
    while (lo < endLo) {
      f(data(lo).asInstanceOf[A])
      lo += 1
    }
  }

/*
  final def foreachFast1(f: A => Unit): Unit = {
    var display5: Array[AnyRef] = null
    var display4: Array[AnyRef] = null
    var display3: Array[AnyRef] = null
    var display2: Array[AnyRef] = null
    var display1: Array[AnyRef] = null
    var display0: Array[AnyRef] = null

    (depth - 1) match { // goto pos zero
      case 5 =>
        display5 = elems
        display4 = display5(0).asInstanceOf[Array[AnyRef]]
        display3 = display4(0).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 4 =>
        display4 = elems
        display3 = display4(0).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 3 =>
        display3 = elems
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 2 =>
        display2 = elems
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 1 =>
        display1 = elems
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 0 =>
        display0 = elems
    }

    var blockIndex = 0
    var nextBlockIndex = blockIndex + 32
    while (endIndex >= nextBlockIndex) {
      var lo = 0
      while (lo < 32) {
        f(display0(lo).asInstanceOf[A])
        lo += 1
      }

      val xor = blockIndex ^ nextBlockIndex
      
      blockIndex = nextBlockIndex

      if (endIndex < nextBlockIndex) {
        if (xor < (1 << 10)) { // level = 1
          display0 = display1((nextBlockIndex >> 5) & 31).asInstanceOf[Array[AnyRef]]
        } else
        if (xor < (1 << 15)) { // level = 2
          display1 = display2((nextBlockIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
          display0 = display1(0).asInstanceOf[Array[AnyRef]]
        } else
        if (xor < (1 << 20)) { // level = 3
          display2 = display3((nextBlockIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
          display1 = display2(0).asInstanceOf[Array[AnyRef]]
          display0 = display1(0).asInstanceOf[Array[AnyRef]]
        } else
        if (xor < (1 << 25)) { // level = 4
          display3 = display4((nextBlockIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
          display2 = display3(0).asInstanceOf[Array[AnyRef]]
          display1 = display2(0).asInstanceOf[Array[AnyRef]]
          display0 = display1(0).asInstanceOf[Array[AnyRef]]
        } else
        if (xor < (1 << 30)) { // level = 5
          display4 = display5((nextBlockIndex >> 25) & 31).asInstanceOf[Array[AnyRef]]
          display3 = display4(0).asInstanceOf[Array[AnyRef]]
          display2 = display3(0).asInstanceOf[Array[AnyRef]]
          display1 = display2(0).asInstanceOf[Array[AnyRef]]
          display0 = display1(0).asInstanceOf[Array[AnyRef]]
        }// else { // level = 6
      }
      
      nextBlockIndex += 32
    }
    
    val endLo = endIndex - blockIndex // is < 32
    var lo = 0
    while (lo < endLo) {
      f(display0(lo).asInstanceOf[A])
      lo += 1
    }
  }
*/


/*  
  override def toString = mkString("SPMVectorState(",",",")")
  def mkString(a: String, b: String, c: String) = {
    val s = new StringBuilder
    s.append(a)
    var first = true
    for (x <- this) {
      if (first) first = false else s.append(b)
      s.append(x)
    }
    s.append(c)
    s.toString
  }
*/  
  
  def apply(index: Int): A = {
    var level = (depth-1) * 5
    var data = elems
    while (level > 0) {
      data = data((index >> level) & 31).asInstanceOf[Array[AnyRef]]
      level -= 5
    }
    data(index & 31).asInstanceOf[A]
  }
  
  def applyFast(index: Int): A = (depth-1) match {
    case 5 =>
      var data = elems
      data = data((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      data(index & 31).asInstanceOf[A]
    case 4 =>
      var data = elems
      data = data((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      data(index & 31).asInstanceOf[A]
    case 3 =>
      var data = elems
      data = data((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      data(index & 31).asInstanceOf[A]
    case 2 =>
      var data = elems
      data = data((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      data = data((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      data(index & 31).asInstanceOf[A]
    case 1 =>
      var data = elems
      data = data((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      data(index & 31).asInstanceOf[A]
    case 0 =>
      elems(index & 31).asInstanceOf[A]
  }
  
  def pointer(index: Int): SPMVectorIterator[A] = {
    var d = depth-1
    var level = d * 5
    val display = new Array[Array[AnyRef]](6)
    display(d) = elems
    while (level > 0) {
      display(d-1) = display(d)((index >> level) & 31).asInstanceOf[Array[AnyRef]]
      level -= 5
      d -= 1
    }
    new SPMVectorIterator[A](depth, index & ~31, index & 32, endIndex, display) // display
  }
  
  
}


final class SPMVectorIterator[+A] (
  private var depth: Int,
  private var blockIndex: Int,
  private var lo: Int,
  private var endIndex: Int,
  __display: Array[Array[AnyRef]]
) {

  private var display0 = __display(0)
  private var display1 = __display(1)
  private var display2 = __display(2)
  private var display3 = __display(3)
  private var display4 = __display(4)
  private var display5 = __display(5)

  private var endLo = Math.min(endIndex - blockIndex, 32)

//  private def requiredLevel(index: Int) = (31 - java.lang.Integer.numberOfLeadingZeros(index)) / 5 // could do it in a loop...

  private def requiredLevel(index: Int) = { // faster than Integer.numberOfLeadingZeros!
    if (index < (1 << 10)) 1 else
    if (index < (1 << 15)) 2 else
    if (index < (1 << 20)) 3 else
    if (index < (1 << 25)) 4 else
    if (index < (1 << 30)) 5 else 6
  }
  
  def hasNext = _hasNext
  
  private var _hasNext = blockIndex + lo < endIndex
  
  def next(): A = {
    if (!_hasNext) throw new NoSuchElementException("reached iterator end")
    
    val res = display0(lo).asInstanceOf[A]
    lo += 1
    
    if (lo == endLo) {
      if (blockIndex + lo < endIndex) {
        val newBlockIndex = blockIndex+32
  //      gotoBlockStart(newBlockIndex, requiredLevel(blockIndex ^ newBlockIndex))
        bigHairyGoto(newBlockIndex, blockIndex ^ newBlockIndex)

        blockIndex = newBlockIndex
        endLo = Math.min(endIndex - blockIndex, 32)
        lo = 0
      } else {
        _hasNext = false
      }
    }

    res
  }


  private def bigHairyGoto(index: Int, xor: Int): Unit = {
    if (xor < (1 << 10)) { // level = 1
      display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 15)) { // level = 2
      display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 20)) { // level = 3
      display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 25)) { // level = 4
      display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 30)) { // level = 5
      display4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      display3 = display4(0).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    } else { // level = 6
//      display5 = display6((index >> 30) & 31).asInstanceOf[Array[AnyRef]]
      display4 = display5(0).asInstanceOf[Array[AnyRef]]
      display3 = display4(0).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    }
  }


/*
  private def gotoBlockStart(index: Int, level: Int): Unit = {
    val shift = level * 5
    display(level-1) = display(level)((index >> shift) & 31).asInstanceOf[Array[AnyRef]]
    gotoZero(level-1)
  }
*/
/*
  private def gotoZero(level: Int): Unit = level match {
    case 6 =>
      display(5) = display(6)(0).asInstanceOf[Array[AnyRef]]
      display(4) = display(5)(0).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)(0).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 5 =>
      display(4) = display(5)(0).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)(0).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 4 =>
      display(3) = display(4)(0).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 3 =>
      display(2) = display(3)(0).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 2 =>
      display(1) = display(2)(0).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 1 =>
      display(0) = display(1)(0).asInstanceOf[Array[AnyRef]]
    case 0 =>
  }
*/
/*
  private def gotoPos(index: Int, level: Int): Unit = level match {
    case 6 =>
      display(5) = display(6)((index >> 30) & 31).asInstanceOf[Array[AnyRef]]
      display(4) = display(5)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 5 =>
      display(4) = display(5)((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 4 =>
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 3 =>
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 2 =>
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 1 =>
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
  }
*/
/*
  private def gotoPosA(index: Int, level: Int): Unit = {
    var d = level
    var shift = d * 5
    while (d > 0) {
      display(d-1) = display(d)((index >> shift) & 31).asInstanceOf[Array[AnyRef]]
      shift -= 5
      d -= 1
    }
  }
*/

}


final class SPMBlockListState[+A](
  private val depth: Int,
  val blockIndex: Int,
  val endLo: Int,
  val endIndex: Int,
  var display0: Array[AnyRef],
  private val displayN: Array[AnyRef]
) {

  def this(depth: Int, endIndex: Int, topLevel: Array[AnyRef]) = {
    this(depth, 0, Math.min(endIndex, 32), endIndex, null, topLevel)
    gotoZero(depth - 1)
  }

//  println("create block list: " + depth + ","+blockIndex+","+endLo+","+endIndex)


  def tail() = {
    bigHairyGoto(blockIndex + 32)
  }

  private def bigHairyGoto(index: Int): SPMBlockListState[A] = depth match {
    case 2 => // level = 1
      val display1 = this.displayN
      val display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      new SPMBlockListState[A](depth, index, Math.min(endIndex-index, 32), endIndex, display0, display1)
    case 3 => // level = 2
      val display2 = this.displayN
      val display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      val display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      new SPMBlockListState[A](depth, index, Math.min(endIndex-index, 32), endIndex, display0, display2)
    case 4 => // level = 3
      val display3 = this.displayN
      val display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      val display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      val display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      new SPMBlockListState[A](depth, index, Math.min(endIndex-index, 32), endIndex, display0, display3)
    case 5 => // level = 4
      val display4 = this.displayN
      val display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      val display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      val display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      val display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      new SPMBlockListState[A](depth, index, Math.min(endIndex-index, 32), endIndex, display0, display4)
    case 6 => // level = 5
      val display5 = this.displayN
      val display4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      val display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      val display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      val display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      val display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      new SPMBlockListState[A](depth, index, Math.min(endIndex-index, 32), endIndex, display0, display5)
  }

  private def gotoZero(level: Int): Unit = level match {
    case 5 =>
      val display5 = displayN
      val display4 = display5(0).asInstanceOf[Array[AnyRef]]
      val display3 = display4(0).asInstanceOf[Array[AnyRef]]
      val display2 = display3(0).asInstanceOf[Array[AnyRef]]
      val display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 4 =>
      val display4 = displayN
      val display3 = display4(0).asInstanceOf[Array[AnyRef]]
      val display2 = display3(0).asInstanceOf[Array[AnyRef]]
      val display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 3 =>
      val display3 = displayN
      val display2 = display3(0).asInstanceOf[Array[AnyRef]]
      val display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 2 =>
      val display2 = displayN
      val display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 1 =>
      val display1 = displayN
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 0 =>
  }

}

final class SPMVectorListBlt[+A] (
  private val lo: Int,
  private val block: SPMBlockListState[A]
) {

  def this(depth: Int, endIndex: Int, topLevel: Array[AnyRef]) = {
    this(0, new SPMBlockListState(depth, endIndex, topLevel))
  }

    
  def isEmpty = lo == block.endLo

  def head: A = {
    block.display0(lo).asInstanceOf[A]
  }
  
  def tail: SPMVectorListBlt[A] = {
    if (lo + 1 < block.endLo) {
      new SPMVectorListBlt[A](lo + 1, block)
    } else if (lo + 1 == block.endLo) {
      val newBlockIndex = block.blockIndex+32
      if (newBlockIndex < block.endIndex) {

/*
        val level = requiredLevel(blockIndex ^ newBlockIndex)
        val res = new SPMVectorList[A](depth, newBlockIndex, 0, endIndex, display)
        System.arraycopy(display, 0, res.display, 0, 6)
        res.gotoBlockStart(newBlockIndex, level)
        res
*/
        val nextBlock = block.tail
        new SPMVectorListBlt[A](0, nextBlock)
        
      } else {
        new SPMVectorListBlt[A](32, new SPMBlockListState(1, 0, 32, 32, new Array[AnyRef](32), null))
      }
    } else {
      throw new NoSuchElementException("tail of empty vector")
    }
  }
  
}
/*
final class SPMVectorListStub[+A] (
  var lo: Int,
//  iter: SPMVectorState[A]
  val endIndex: Int
) extends Cloneable {
*/
/*
final class SPMVectorList[+A] {
  private var lo: Int = _
  private var endIndex: Int = _
*/

final class SPMVectorListStub[+A] {
  var lo: Int = _
  var endIndex: Int = _
  
  final def this(depth: Int, _endIndex: Int, topLevel: Array[AnyRef]) = {
//    this(0, _endIndex)
    // this(0, endIndex) //(new SPMVectorState(0, endIndex, depth, topLevel)))
    this()
    lo = 0
    endIndex = _endIndex
  }

  
  final def isEmpty = lo == endIndex //iter.endIndex

  final def head: A = {
    "iter(lo)".asInstanceOf[A]
  }
  
//  @noinline final def copy = clone.asInstanceOf[SPMVectorListStub[A]] // work around inlining!
  
  final def tail = {
    val res = new SPMVectorListStub[A] //copy
    res.lo = lo + 1
    res.endIndex = endIndex
    res
  }
  
}


final class SPMVectorList[+A] (
  var index: Int,
  state: SPMVectorState[A]
)  extends Cloneable {

  final def isEmpty = index == state.endIndex

  final def head: A = {
    state.applyFast(index)
  }
  
  @noinline final def copy = clone().asInstanceOf[SPMVectorList[A]] // work around inlining!
  
  final def tail = {
    val t = this.copy
    t.index += 1
    t
//    new SPMVectorList[A](index+1,state)
  }
  
}


/*
@inline final class SPMVectorListEsc[+A] {

  var index: Int = _
  var endIndex: Int = _
  var state: AnyRef = _ //SPMVectorState[A] = _

  @inline def isEmpty = {
    val i = index
    val e = endIndex
    i == e
  }

  @inline def head: A = {
    state.asInstanceOf[SPMVectorState[A]].applyFast(index)
  }
  
  @inline def tail = {
    val cur = index
    val s = state
    val e = endIndex
    val t = new SPMVectorListEsc[A]
    t.index = cur + 1
    t.endIndex = e
    t.state = s
    t
  }
  
}
*/

@inline final class SPMVectorListEsc[+A] {

  var index: Int = _
  var endIndex: Int = _
  var state: AnyRef = _ //SPMVectorState[A] = _

  @inline def isEmpty = {
    val i = index
    val e = endIndex
    i >= e
  }

  @inline def head: A = {
    // assert that iterator is at pos index, otherwise create new state
    // should really cache 
    state.asInstanceOf[SPMVectorIterator[A]].next()
  }
  
  @inline def tail = {
    val n = index
    val s = state
    val e = endIndex
    val t = new SPMVectorListEsc[A]
    t.index = n + 1
    t.endIndex = e
    t.state = s
    t
  }
  
}




final class SPMVectorListAlt[+A] (
  private val depth: Int,
  private val blockIndex: Int,
  private var lo: Int,
  private val endLo: Int,
  private val endIndex: Int,
  private var display0: Array[AnyRef],
  private var display1: Array[AnyRef],
  private var display2: Array[AnyRef],
  private var display3: Array[AnyRef],
  private var display4: Array[AnyRef],
  private var display5: Array[AnyRef]
) extends Cloneable {



  def this(depth: Int, endIndex: Int, topLevel: Array[AnyRef]) = {
    this(depth, 0, 0, Math.min(endIndex, 32), endIndex, null, null, null, null, null, null)
    (depth - 1) match {
      case 0 => display0 = topLevel
      case 1 => display1 = topLevel
      case 2 => display2 = topLevel
      case 3 => display3 = topLevel
      case 4 => display4 = topLevel
      case 5 => display5 = topLevel
    }
    gotoZero(depth - 1)
  }

    
  def isEmpty = lo == endLo

  def head: A = {
    display0(lo).asInstanceOf[A]
  }
  
  @noinline final def copy = clone().asInstanceOf[SPMVectorListAlt[A]] // work around inlining!
  
  def tail: SPMVectorListAlt[A] = {
    if (lo + 1 < endLo) {
//      new SPMVectorListAlt[A](depth, blockIndex, lo + 1, endLo, endIndex, display0, display1, display2, display3, display4, display5)
      val t = copy
      t.lo = lo + 1
      t
    } else if (lo + 1 == endLo) {
      val newBlockIndex = blockIndex+32
      if (newBlockIndex < endIndex) {
/*
        val level = requiredLevel(blockIndex ^ newBlockIndex)
        val res = new SPMVectorList[A](depth, newBlockIndex, 0, endIndex, display)
        System.arraycopy(display, 0, res.display, 0, 6)
        res.gotoBlockStart(newBlockIndex, level)
        res
*/
        bigHairyGoto(newBlockIndex, blockIndex ^ newBlockIndex)
        
      } else {
        new SPMVectorListAlt[A](1, 0, 32, 32, 32, new Array[AnyRef](32), null, null, null, null, null)
      }
    } else {
      throw new NoSuchElementException("tail of empty vector")
    }
  }
  
  
  private def bigHairyGoto(index: Int, xor: Int): SPMVectorListAlt[A] = {
    if (xor < (1 << 10)) { // level = 1
      val display5 = this.display5
      val display4 = this.display4
      val display3 = this.display3
      val display2 = this.display2
      val display1 = this.display1
      val display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      new SPMVectorListAlt[A](depth, index, 0, Math.min(endIndex-index, 32), endIndex, display0, display1, display2, display3, display4, display5)
    } else
    if (xor < (1 << 15)) { // level = 2
      val display5 = this.display5
      val display4 = this.display4
      val display3 = this.display3
      val display2 = this.display2
      val display1 = this.display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      val display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
      new SPMVectorListAlt[A](depth, index, 0, Math.min(endIndex-index, 32), endIndex, display0, display1, display2, display3, display4, display5)
    } else
    if (xor < (1 << 20)) { // level = 3
      val display5 = this.display5
      val display4 = this.display4
      val display3 = this.display3
      val display2 = this.display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      val display1 = this.display2(0).asInstanceOf[Array[AnyRef]]
      val display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
      new SPMVectorListAlt[A](depth, index, 0, Math.min(endIndex-index, 32), endIndex, display0, display1, display2, display3, display4, display5)
    } else
    if (xor < (1 << 25)) { // level = 4
      val display5 = this.display5
      val display4 = this.display4
      val display3 = this.display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      val display2 = this.display3(0).asInstanceOf[Array[AnyRef]]
      val display1 = this.display2(0).asInstanceOf[Array[AnyRef]]
      val display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
      new SPMVectorListAlt[A](depth, index, 0, Math.min(endIndex-index, 32), endIndex, display0, display1, display2, display3, display4, display5)
    } else
    if (xor < (1 << 30)) { // level = 5
      val display5 = this.display5
      val display4 = this.display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      val display3 = this.display4(0).asInstanceOf[Array[AnyRef]]
      val display2 = this.display3(0).asInstanceOf[Array[AnyRef]]
      val display1 = this.display2(0).asInstanceOf[Array[AnyRef]]
      val display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
      new SPMVectorListAlt[A](depth, index, 0, Math.min(endIndex-index, 32), endIndex, display0, display1, display2, display3, display4, display5)
    } else { // level = 6
      throw new NoSuchElementException("cannot handle more than (1<<30) items")
    }
  }
  
  
  
  
/*
  private def gotoBlockStart(index: Int, level: Int): Unit = {
    val shift = level * 5
    display(level-1) = display(level)((index >> shift) & 31).asInstanceOf[Array[AnyRef]]
    gotoZero(level-1)
  }
*/

  private def gotoZero(level: Int): Unit = level match {
    case 5 =>
      display4 = display5(0).asInstanceOf[Array[AnyRef]]
      display3 = display4(0).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 4 =>
      display3 = display4(0).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 3 =>
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 2 =>
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 1 =>
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 0 =>
  }

/*
  private def gotoPos(index: Int, level: Int): Unit = level match {
    case 6 =>
      display(5) = display(6)((index >> 30) & 31).asInstanceOf[Array[AnyRef]]
      display(4) = display(5)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 5 =>
      display(4) = display(5)((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 4 =>
      display(3) = display(4)((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 3 =>
      display(2) = display(3)((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 2 =>
      display(1) = display(2)((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
    case 1 =>
      display(0) = display(1)((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
  }


  private def gotoPosA(index: Int, level: Int): Unit = {
    var d = level
    var shift = d * 5
    while (d > 0) {
      display(d-1) = display(d)((index >> shift) & 31).asInstanceOf[Array[AnyRef]]
      shift -= 5
      d -= 1
    }
  }
*/
}


final class SPMVectorListClt[+A] {

  private var depth: Int = _
  private var blockIndex: Int = _
  private var lo: Int = _
  private var endLo: Int = _
  private var endIndex: Int = _
  private var display0: Array[AnyRef] = _
  private var display1: Array[AnyRef] = _
  private var display2: Array[AnyRef] = _
  private var display3: Array[AnyRef] = _
  private var display4: Array[AnyRef] = _
  private var display5: Array[AnyRef] = _


  def this(depth: Int, endIndex: Int, topLevel: Array[AnyRef]) = {
//    this(depth, 0, 0, Math.min(endIndex, 32), endIndex, null, null, null, null, null, null)
    this()
    this.depth = depth
    this.blockIndex = 0
    this.lo = 0
    this.endLo = Math.min(endIndex, 32)
    this.endIndex = endIndex
    (depth - 1) match {
      case 0 => display0 = topLevel
      case 1 => display1 = topLevel
      case 2 => display2 = topLevel
      case 3 => display3 = topLevel
      case 4 => display4 = topLevel
      case 5 => display5 = topLevel
    }
    gotoZero(depth - 1)
  }

    
  def isEmpty = lo == endLo

  def head: A = {
    display0(lo).asInstanceOf[A]
  }
  
  def tail = {
    val res = new SPMVectorListClt[A]
    res.depth = this.depth
    res.endIndex = endIndex
    
    if (lo + 1 < endLo) {
//      new SPMVectorListClt[A](depth, blockIndex, lo + 1, endLo, endIndex, display0, display1, display2, display3, display4, display5)
      res.blockIndex = blockIndex
      res.lo = lo + 1
      res.endLo = endLo
      
      res.display0 = display0
      res.display1 = display1
      res.display2 = display2
      res.display3 = display3
      res.display4 = display4
      res.display5 = display5

    } else if (lo + 1 == endLo) {
      val newBlockIndex = blockIndex+32
      if (newBlockIndex < endIndex) {

        res.blockIndex = newBlockIndex
//      res.lo = 0
        res.endLo = Math.min(endIndex-newBlockIndex, 32)
        bigHairyGoto(res, newBlockIndex, blockIndex ^ newBlockIndex)
        
      } else {
//        new SPMVectorListClt[A](1, 0, 32, 32, 32, new Array[AnyRef](32), null, null, null, null, null)
        res.depth = 1
        res.blockIndex = 0
        res.lo = 32
        res.endLo = 32
        res.endIndex = 32
        res.display0 = new Array[AnyRef](32)
      }
    } else {
      throw new NoSuchElementException("tail of empty vector")
    }
    res
  }
  
  
  private def bigHairyGoto[B](res: SPMVectorListClt[B], index: Int, xor: Int): Unit = {
    
    if (xor < (1 << 10)) { // level = 1
      res.display5 = this.display5
      res.display4 = this.display4
      res.display3 = this.display3
      res.display2 = this.display2
      res.display1 = this.display1
      res.display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 15)) { // level = 2
      res.display5 = this.display5
      res.display4 = this.display4
      res.display3 = this.display3
      res.display2 = this.display2
      res.display1 = this.display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      res.display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 20)) { // level = 3
      res.display5 = this.display5
      res.display4 = this.display4
      res.display3 = this.display3
      res.display2 = this.display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      res.display1 = this.display2(0).asInstanceOf[Array[AnyRef]]
      res.display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 25)) { // level = 4
      res.display5 = this.display5
      res.display4 = this.display4
      res.display3 = this.display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      res.display2 = this.display3(0).asInstanceOf[Array[AnyRef]]
      res.display1 = this.display2(0).asInstanceOf[Array[AnyRef]]
      res.display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
    } else
    if (xor < (1 << 30)) { // level = 5
      res.display5 = this.display5
      res.display4 = this.display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      res.display3 = this.display4(0).asInstanceOf[Array[AnyRef]]
      res.display2 = this.display3(0).asInstanceOf[Array[AnyRef]]
      res.display1 = this.display2(0).asInstanceOf[Array[AnyRef]]
      res.display0 = this.display1(0).asInstanceOf[Array[AnyRef]]
    } else { // level = 6
      throw new NoSuchElementException("cannot handle more than (1<<30) items")
    }
  }
  
  private def gotoZero(level: Int): Unit = level match {
    case 5 =>
      display4 = display5(0).asInstanceOf[Array[AnyRef]]
      display3 = display4(0).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 4 =>
      display3 = display4(0).asInstanceOf[Array[AnyRef]]
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 3 =>
      display2 = display3(0).asInstanceOf[Array[AnyRef]]
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 2 =>
      display1 = display2(0).asInstanceOf[Array[AnyRef]]
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 1 =>
      display0 = display1(0).asInstanceOf[Array[AnyRef]]
    case 0 =>
  }

}



final class SPMVectorBuilder[A]() {
  
  private val display = new Array[Array[AnyRef]](6)
  display(0) = new Array[AnyRef](32)
  
  var depth = 1
  var blockIndex = 0
  var lo = 0
  
  private def requiredLevel(index: Int) = (31 - java.lang.Integer.numberOfLeadingZeros(index)) / 5 // could do it in a loop...
  
  def += (elem: A): this.type = {
    
    if (lo == 32) {
      val newBlockIndex = blockIndex+32
      val level = requiredLevel(blockIndex ^ newBlockIndex)
      
      if (level == depth) { // need to create
        display(level) = new Array[AnyRef](32)
        display(level)(0) = display(level-1)
        depth = level + 1
      }
      
      val hi = (newBlockIndex >> (level * 5)) & 31
      display(level-1) = new Array[AnyRef](32)
      display(level)(hi) = display(level-1)

      var i = level-1
      while (i > 0) {
        display(i-1) = new Array[AnyRef](32)
        display(i)(0) = display(i-1)
        i -= 1
      }
      
      blockIndex = newBlockIndex
      lo = 0
    }
    
    display(0)(lo) = elem.asInstanceOf[AnyRef]
    lo += 1
    this
  }

  def result: SPMVectorState[A] = new SPMVectorState(0, blockIndex + lo, depth, display(depth-1))
  def resultList: SPMVectorList[A] = new SPMVectorList(0, result)
  def resultListAlt: SPMVectorListAlt[A] = new SPMVectorListAlt(depth, blockIndex + lo, display(depth-1))
  def resultListBlt: SPMVectorListBlt[A] = new SPMVectorListBlt(depth, blockIndex + lo, display(depth-1))
  def resultListClt: SPMVectorListClt[A] = new SPMVectorListClt(depth, blockIndex + lo, display(depth-1))
  def resultListStub: SPMVectorListStub[A] = new SPMVectorListStub(depth, blockIndex + lo, display(depth-1))
  @inline final def resultListEsc: SPMVectorListEsc[A] = {
    val t = new SPMVectorListEsc[A]
    t.index = 0
    t.endIndex = blockIndex + lo
    t.state = result.pointer(0)
    t
  }
}



/*

class SPMVector[+A](private val prefixLength: Int, private val suffixLength: Int,
  private val prefixState: SPMVectorState[A], private val suffixState: SPMVectorState[A]) extends Vector[A] {

  def this(ignore: Unit, elem0: A, elem1: A, elem2: A, elem3: A) = this(4, 0, new SPMVectorState(elem3, elem2, elem1, elem0), null)
  def this(elem0: A, elem1: A, elem2: A, elem3: A, ignore: Unit) = this(0, 4, null, new SPMVectorState(elem0, elem1, elem2, elem3))

  def length = prefixLength + suffixLength
  
  def apply(index: Int) = {
//    if (length < Vector.spmStats.length) {
//      Vector.applyStats(length) += 1
//    }
    if (0 <= index && index < prefixLength + suffixLength) {
      if (index < prefixLength)
        prefixState(prefixLength - index - 1)
      else
        suffixState(index - prefixLength)
    }
    else
      throw new IndexOutOfBoundsException(index.toString)
  }

  def :+[B >: A](elem: B) = {
    if (suffixLength == 0)
      new SPMVector(prefixLength, 1, prefixState, new SPMVectorState(elem))
    else
      new SPMVector(prefixLength, suffixLength + 1, prefixState, suffixState.append(suffixLength, elem))
  }
  
  def +:[B >: A](elem: B) = {
    if (prefixLength == 0)
      new SPMVector(1, suffixLength, new SPMVectorState(elem), suffixState)
    else
      new SPMVector(prefixLength + 1, suffixLength, prefixState.append(prefixLength, elem), suffixState)
  }

  def elemsEq[B>:A](that: List[B]): Boolean = that match {
    case that: SPMVector[a] =>
      (prefixLength == that.prefixLength) &&
      (suffixLength == that.suffixLength) &&
      (prefixState eq that.prefixState) &&
      (suffixState eq that.suffixState)
    case _ => false
  }
  def elemsNe[B>:A](that: List[B]): Boolean = that match {
    case that: SPMVector[a] =>
      (prefixLength != that.prefixLength) ||
      (suffixLength != that.suffixLength) ||
      (prefixState ne that.prefixState) ||
      (suffixState ne that.suffixState)
    case _ => true
  }

  // TODO: implement take, drop, etc...

  override def head = apply(0)

  override def tail = {
    if (length > 1) {
      if (prefixLength > 0) {
  //      Vector.tailStats(0) += 1
  //      val t0 = System.nanoTime
        val res = new SPMVector(prefixLength - 1, suffixLength, prefixState, suffixState) // could potentially drop front elements...
  //      Vector.tailStats(2) += ((System.nanoTime-t0) / 10).toInt
        res
      } else { // suffixLength > 0
  //      Vector.tailStats(1) += 1
  //      val t0 = System.nanoTime
        var res = Nil: List[A]
        suffixState.reverseForeach({ x:A =>
          res = res.::[A](x)
        }, suffixLength)
        res = res.tail
  //      Vector.tailStats(3) += ((System.nanoTime-t0) / 10).toInt
        res.asInstanceOf[Vector[A]]
      }
    } else if (length == 1) {
      Nil
    } else {
      throw new NoSuchElementException("tail of empty SPM vector -- shouldn't be here!")
    }
  }
  
  override def foreach[B](f: A=>B): Unit = {
//    if (length < Vector.spmStats.length) {
//      Vector.foreachStats(length) += 1
//    }
  
    if (prefixLength > 0) {
      prefixState.reverseForeach(f, prefixLength)
    }
    if (suffixLength > 0) {
      suffixState.foreach(f, suffixLength)
    }
  }

  override def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, Vector[A]]): That = {
//      bf.asInstanceOf[List.ListBuilderFactory[_]]
    val pNew = if (prefixLength > 0) {
      prefixState.reverseMap(f, prefixLength)
    } else null
    val sNew = if (suffixLength > 0) {
      suffixState.map(f, suffixLength)
    } else null
    (new SPMVector(prefixLength, suffixLength, pNew, sNew)).asInstanceOf[That]
  }



}


class SPMVectorState[+A](initialLength: Int, depth: Int, elems: Array[AnyRef] = new Array[AnyRef](32)) { //extends AtomicInteger(initialLength) {
  val length = new AtomicInteger(initialLength)
//    val length = this
//  Vector.spmStats(initialLength) += 1

  
  def this(elem0: A) = this(1, 0, {
    val elems = new Array[AnyRef](32)
    elems(0) = elem0.asInstanceOf[AnyRef]
    elems
  })

  def this(elem0: A, elem1: A) = this(2, 0, {
    val elems = new Array[AnyRef](32)
    elems(0) = elem0.asInstanceOf[AnyRef]
    elems(1) = elem1.asInstanceOf[AnyRef]
    elems
  })

  def this(elem0: A, elem1: A, elem2: A) = this(3, 0, {
    val elems = new Array[AnyRef](32)
    elems(0) = elem0.asInstanceOf[AnyRef]
    elems(1) = elem1.asInstanceOf[AnyRef]
    elems(2) = elem2.asInstanceOf[AnyRef]
    elems
  })

  def this(elem0: A, elem1: A, elem2: A, elem3: A) = this(4, 0, {
    val elems = new Array[AnyRef](32)
    elems(0) = elem0.asInstanceOf[AnyRef]
    elems(1) = elem1.asInstanceOf[AnyRef]
    elems(2) = elem2.asInstanceOf[AnyRef]
    elems(3) = elem3.asInstanceOf[AnyRef]
    elems
  })
  
  
  def foreach[B](f: A=>B, expectedLength: Int): Unit = {
    foreach0(f, 0, expectedLength-1, depth, elems)
  }
  def reverseForeach[B](f: A=>B, expectedLength: Int): Unit = {
    reverseForeach0(f, 0, expectedLength-1, depth, elems)
  }

  private def foreach0[B](f: A=>B, blockOffset: Int, maxIndex: Int, level: Int, data: Array[AnyRef]): Unit = {
    val max = Math.min(32, ((maxIndex-blockOffset) >> level)+1)
    var i = 0
    if (level != 0) {
      while (i < max) {
        foreach0(f, blockOffset + (i<<level), maxIndex, level - 5, data(i).asInstanceOf[Array[AnyRef]])
        i += 1
      }
    } else {
      while (i < max) {
        f(data(i).asInstanceOf[A])
        i += 1
      }
    }
  }

  private def reverseForeach0[B](f: A=>B, blockOffset: Int, maxIndex: Int, level: Int, data: Array[AnyRef]): Unit = {
    val max = Math.min(32, ((maxIndex-blockOffset) >> level)+1)
//    println("reverseForeach: blockOffset="+blockOffset+" maxIndex="+maxIndex+" level="+level+" max="+max+" data="+data)

    var i = max - 1
    if (level != 0) {
      while (i >= 0) {
//        println("descend at " + i + " -> " + data(i))
        reverseForeach0(f, blockOffset + (i<<level), maxIndex, level - 5, data(i).asInstanceOf[Array[AnyRef]])
        i -= 1
      }
    } else {
      while (i >= 0) {
        f(data(i).asInstanceOf[A])
        i -= 1
      }
    }
  }

  def map[B](f: A=>B, expectedLength: Int): SPMVectorState[A] = {
    val elemsNew = map0(f, 0, expectedLength-1, depth, elems)
    new SPMVectorState(length.get, depth, elemsNew)
  }
  def reverseMap[B](f: A=>B, expectedLength: Int): SPMVectorState[A] = {
    val elemsNew = reverseMap0(f, 0, expectedLength-1, depth, elems)
    new SPMVectorState(length.get, depth, elemsNew)
  }

  private def map0[B](f: A=>B, blockOffset: Int, maxIndex: Int, level: Int, data: Array[AnyRef]): Array[AnyRef] = {
    val max = Math.min(32, ((maxIndex-blockOffset) >> level)+1)
    var i = 0
    val dataNew = new Array[AnyRef](32)
    if (level != 0) {
      while (i < max) {
        dataNew(i) = map0(f, blockOffset + (i<<level), maxIndex, level - 5, data(i).asInstanceOf[Array[AnyRef]])
        i += 1
      }
    } else {
      while (i < max) {
        dataNew(i) = f(data(i).asInstanceOf[A]).asInstanceOf[AnyRef]
        i += 1
      }
    }
    dataNew
  }

  private def reverseMap0[B](f: A=>B, blockOffset: Int, maxIndex: Int, level: Int, data: Array[AnyRef]): Array[AnyRef] = {
    val max = Math.min(32, ((maxIndex-blockOffset) >> level)+1)
    var i = max - 1
    val dataNew = new Array[AnyRef](32)
    if (level != 0) {
      while (i >= 0) {
        dataNew(i) = reverseMap0(f, blockOffset + (i<<level), maxIndex, level - 5, data(i).asInstanceOf[Array[AnyRef]])
        i -= 1
      }
    } else {
      while (i >= 0) {
        dataNew(i) = f(data(i).asInstanceOf[A]).asInstanceOf[AnyRef]
        i -= 1
      }
    }
    dataNew
  }
  
  
  
  // contract: 0 <= index < length
  def apply(index: Int): A = {
    var level = depth
    var data = elems
    while (level > 0) {
      data = data((index >> level) & 31).asInstanceOf[Array[AnyRef]]
      level -= 5
    }
    data(index & 31).asInstanceOf[A]
  }
  
  // contract: 0 <= expectedLength <= length
  def append[B >: A](expectedLength: Int, elem: B): SPMVectorState[B] = {
    if ((expectedLength >> depth) != 32) {
      if (length.compareAndSet(expectedLength, expectedLength + 1)) {
        // can update mutably - we're on our own here
//        if (expectedLength + 1 < Vector.spmStats.length) {
//          Vector.spmStats(expectedLength) -= 1
//          Vector.spmStats(expectedLength+1) += 1
//        }
        append0(expectedLength, elem)
      } else {
        // need to do immutable update
        // we know that length > expectedLength, so we never need to add another level
      
        new SPMVectorState[B](expectedLength + 1, depth, update0(expectedLength, expectedLength, depth, elems, elem))
      }
    } else {
      if (length.get == expectedLength) {
        // grow one more level - need copying to assure read consistency
        val elemsNew = new Array[AnyRef](32)
        elemsNew(0) = elems
        new SPMVectorState[B](expectedLength + 1, depth + 5, elemsNew) append0 (expectedLength, elem)
      } else {
        // length > expectedLength
        new SPMVectorState[B](expectedLength + 1, depth, update0(expectedLength, expectedLength, depth, elems, elem))
      }
    }
  }
  
  private def append0[B >: A](index: Int, elem: B): SPMVectorState[B] = {
    var level = depth
    var data = elems
    while (level > 0) {
      val hi = (index >> level) & 31
      val dataLo = data(hi)
      if (dataLo ne null) {
        data = dataLo.asInstanceOf[Array[AnyRef]]
      } else {
        val dataNew = new Array[AnyRef](32)
        data(hi) = dataNew
        data = dataNew
      }
      level -= 5
    }
    data(index & 31) = elem.asInstanceOf[AnyRef]
    this
  }
  
  
  // contract: 0 <= index < length
  def update[B >: A](expectedLength: Int, index: Int, elem: B): SPMVectorState[B] = {
    val depthNew = depth // FIXME: actual depth --> can only reduce here!
    new SPMVectorState(expectedLength, depthNew, update0(index, expectedLength, depth, elems, elem))
  }
  
  // contract: 0 <= index < length
  private[this] def update0[B >: A](index: Int, maxIndex: Int, level: Int, data: Array[AnyRef], elem: B): Array[AnyRef] = {

    val hi = (index >> level) & 31
    val hiMax = Math.min(32, (maxIndex >> level)+1)

    // TODO: can we use @specialized to actually create an Array[Int] ?
    val dataNew = new Array[AnyRef](32)
    if (data ne null) {
      System.arraycopy(data, 0, dataNew, 0, hiMax)
    }
      
    if (level != 0) {
      dataNew(hi) = update0(index, maxIndex, level - 5, dataNew(hi).asInstanceOf[Array[AnyRef]], elem)
    } else
      dataNew(hi) = elem.asInstanceOf[AnyRef]
    
    // NOTE: We could do additional checks to prevent copying if elements/subtries
    // do not change or are all null. Not sure if that's worthwile.
    
    dataNew
  }
  
  
}

*/


object Test {
  
    def vector(label: String, n: Int): SPMVectorState[String] = {
      val a = new SPMVectorBuilder[String]
      for (i <- 0 until n)
        a += (label + i)

//      assert(a.length == n, a.length+"!="+n+" ("+a+")")
      val res = a.result
      for (i <- 0 until n)
        assert(res(i) == (label + i))
      res
    }

    def vectorList(label: String, n: Int): SPMVectorList[String] = {
      val a = new SPMVectorBuilder[String]
      for (i <- 0 until n)
        a += (label + i)

//      assert(a.length == n, a.length+"!="+n+" ("+a+")")
      val res = a.resultList
//      for (i <- 0 until n)
//        assert(res(i) == (label + i))
      res
    }

/*
    def assertSliceEquals[V](a: Vector[V], a0: Int, b: Vector[V], b0: Int, n: Int) = {
      for (i <- 0 until n) {
        traced("assert","assert " + a + ".get("+(a0+i)+")\n==     " + b + ".get("+(b0+i)+")") {
        val x = a(a0 + i)
        val y = b(b0 + i)
        assert(x == y, (a0+i) + "->"+ x + " != " + (b0+i) + "->" + y + " in " + a)
        }
      }
    }
*/

/*
    def testedSlice[V](a: Vector[V], left: Int, right: Int) = {
      val b = a.asInstanceOf[AMTVector[V]].materialize(left, right)
      val r = b.length
      assert(r == right - left, r+"!="+right+"-"+left)
      assertSliceEquals(b, 0, a, left, right - left - 1)
//      val b2 = b.materialize()
//      assertSliceEquals(b2, 0, a, left, right - left - 1)
      b
      /*
      val b = a.slice(left, right)
      val r = b.length
      assert(r == right - left, r+"!="+right+"-"+left)
      assertSliceEquals(b, 0, a, left, right - left - 1)
//      val b2 = b.materialize()
//      assertSliceEquals(b2, 0, a, left, right - left - 1)
      b
      */
    }
*/

  def test1() = {
    println("===== test1 =====")
    
    val N = 150000
    val a = vector("a", N)

    {
      var i = 0
      val it = a.pointer(0)
      while (i < N) {
        assert(it.hasNext)
        val x = it.next()
        assert(x == "a"+i)
        i += 1
      }
      assert(!it.hasNext)
    }

//    println(a)
  }

  def test2() = {
    println("===== test2 =====")

    val N = 150000
    val a = vectorList("a", N)

    {
      var i = 0
      var it = a
      while (i < N) {
        assert(!it.isEmpty)
        val x = it.head
        assert(x == "a"+i)
        it = it.tail
        i += 1
      }
      assert(it.isEmpty)
    }

//    println(a)
  }

  def main(args: Array[String]) = {

    test1()
    test2()


    println("done")
  }

}

