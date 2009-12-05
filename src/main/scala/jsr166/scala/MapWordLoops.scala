/*
 * Original Java version Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 * 
 * jatran used for initial translation to Scala
 */

package jsr166.scala

import _root_.scala.collection._

object MapWordLoops {
  val WORDS_FILES = Array("kw.txt", "class.txt", "dir.txt", "ids.txt", "testwords.txt")
  val MAX_WORDS = 500000
  val pinsert = 60
  val premove = 2
  val NOPS = 8000000
  val numTests = 3
  
  def main(args: Array[String]) {
    var mapClass: Class[_ <: mutable.Map[String, String]] = null
    try {
      mapClass = Class.forName(args(0)).asInstanceOf[Class[mutable.Map[String, String]]]
    } catch { 
      case e: ClassNotFoundException => 
        throw new RuntimeException("Class " + args(0) + " not found.")

    }
    System.out.println("Testing " + mapClass.getName())

    for (s <- 0 until WORDS_FILES.length) tests(mapClass, numTests, s)


    var s = WORDS_FILES.length - 1
    while (s >= 0) {
      tests(mapClass, numTests, s)
      s -= 1
    }
  }
  
  def tests(mapClass: Class[_ <: mutable.Map[String, String]], numTests: Int, sizeIndex: Int) {
    try {
      val key = readWords(sizeIndex)
      val size = key.length
      print("n = " + LoopHelpers.rightJustify(size) + " : ")
      
      var least = java.lang.Long.MAX_VALUE
      var i = 0
      while (i < numTests) {
        var m = newMap(mapClass)
        val t = doTest(i, mapClass.getName, m, key)
        if (t < least) least = t
        m.clear
        m = null
        i += 1
      }

      val nano = Math.round(1000000.0 * least / NOPS)
      println(LoopHelpers.rightJustify(nano) + " ns per op")
    } catch { 
      case ignore: java.io.IOException => 
        return  // skip test if can't read file

    }
  }


  def newMap(cl:Class[_ <: mutable.Map[String, String]]): mutable.Map[String, String] = {
    try {
      cl.newInstance
    } catch { 
      case e:Exception => 
        throw new RuntimeException("Can't instantiate " + cl + ": " + e)

    }
  }

  def pause():Unit = {
    try {
      Thread.sleep(100)
    } catch { 
      case ie:InterruptedException => 
        return

    }
  }
  
  def readWords(sizeIndex:Int):Array[String] = {
    var l = new Array[String](MAX_WORDS)
    var array: Array[String] = null
    try {
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(
          currentThread.getContextClassLoader.getResourceAsStream(WORDS_FILES(sizeIndex))))
      
      var k = 0
      var line: String = null
      while ({line = reader.readLine; line} ne null) {
        l({k += 1; k - 1}) = line
      }

      array = new Array[String](k)
      var i:Int = 0
      while (i < k) {
        array(i) = l(i)
        l(i) = null
        i += 1
      }
      l = null
      reader.close
    } catch { 
      case ex: java.io.IOException => 
        System.out.println("Can't read words file:" + ex)
        throw ex

    }
    array
  }

  def doTest(id: Int, name: String, m: mutable.Map[String, String], key: Array[String]): Long = {
    val runner = new Runner(id, m, key)
    val startTime = System.currentTimeMillis
    runner.run
    val afterRun = System.currentTimeMillis
    val runTime = afterRun - startTime
    val np = runner.total
    if (runner.total == runner.hashCode) 
      println("Useless Number" + runner.total)

    val sz = runner.maxsz
    if (sz == runner.hashCode) println("Useless Number" + sz)

    runTime
  }

  class Runner(id: Int, val map: mutable.Map[String, String], key: Array[String]) extends Runnable {
    var nputs = 0
    var npgets = 0
    var nagets = 0
    var nremoves = 0
    @volatile var total: Int = _
    var maxsz: Int = _
    val pctrem = ((premove.toLong * (Integer.MAX_VALUE / 2).toLong) / 50).toInt
    val pctins = ((pinsert.toLong * (Integer.MAX_VALUE / 2).toLong) / 50).toInt
    val rng = new LoopHelpers.SimpleRandom((id + 1) * 8862213513L)
    
    def oneStep(_j: Int): Int = {
      var j = _j
      val n = key.length
      val r = rng.next & 0x7FFFFFFF
      val jinc = (r & 7)
      j += jinc - 3
      if (j >= n) j -= n
      if (j < 0) j += n

      var l = n / 4 + j
      if (l >= n) l -= n

      val k = key(j)
      val x = map.get(k) match {
        case None =>
          nagets += 1
          if (r < pctins) {
            map.put(k, key(l))
            nputs += 1
            val csz = nputs - nremoves
            if (csz > maxsz) maxsz = csz
          }
        case Some(x) =>
          if (k== x) npgets += 1
          if (r < pctrem) {
            map.remove(k)
            nremoves += 1
            j += ((r >>> 8) & 7) +  n / 2
            if (j >= n) j -= n
          }
      }
      j
    }
    
    def run {
      var j = key.length / 2
      var i = 0
      while (i < NOPS) {
        j = oneStep(j)
        i += 1
      }
      total = nputs + npgets + nagets + nremoves
    }
  }
}
