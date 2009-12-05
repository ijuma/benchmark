/*
 * Original Java version Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 * 
 * jatran used for initial translation to Scala
 */

package jsr166.scala

import _root_.scala.collection._

object DenseMapMicroBenchmark {
  val ITERS_PER_TEST = 4
  val NANOS_PER_JOB = 4L * 1000L * 1000L * 1000L // 4 sec
  val NANOS_PER_WARMUP = 500L * 1000L * 1000L // 0.5 sec
  
  // map operations per iteration -- change if hasher.work changed
  val OPS_PER_ITER = 4 // put + get + iter + remove
  var SIZE = 50000 // may be replaced by program arg
  
  abstract class Job(val name: String) {
    var nanos: Long = _
    var runs: Int = _
    def work: Unit
  }
  
  /**
   * Runs each job for at least NANOS_PER_JOB seconds.
   * Returns array of average times per job per run.
   */
  def time0(nanos: Long, jobs: Job*) {
    for (i <- 0 until jobs.length) {
      Thread.sleep(50)
      val t0 = System.nanoTime()
      var t = 0L
      var j = 0
      do {
        j += 1
        jobs(i).work
      } while ({t = System.nanoTime() - t0; t} < nanos);
      jobs(i).nanos = t / j
      jobs(i).runs = j
    }
  }

  def time(jobs: Job*) {
    time0(NANOS_PER_JOB, jobs: _*)
    val nameHeader = "Method"
    val nameWidth = jobs.map(_.name.length).foldLeft(nameHeader.length)(Math.max(_, _))

    val itemsPerTest = SIZE * OPS_PER_ITER * ITERS_PER_TEST
    val timeHeader = "Nanos/item"
    val timeWidth = timeHeader.length
    val ratioHeader = "Ratio"
    val ratioWidth = ratioHeader.length
    val format = "%%-%ds %%%dd %%.3f%%n".format(nameWidth, timeWidth)
    val headerFormat = "%%-%ds %%-%ds %%-%ds%%n".format(nameWidth, timeWidth, ratioWidth)
    printf(headerFormat, "Method", "Nanos/item", "Ratio")

    // Print out absolute and relative times, calibrated against first job
    {
      var i:Int = 0
      while (i < jobs.length) {
        val time = jobs(i).nanos / itemsPerTest
        val ratio = jobs(i).nanos.toDouble / jobs(0).nanos.toDouble 
        printf(format, jobs(i).name, time, ratio)
        i += 1
      }
    }

  }

  def toLongs(ints: Array[java.lang.Integer]): Array[java.lang.Long] =
    ints.map(i => new java.lang.Long(i.longValue))

  def toStrings(ints: Array[java.lang.Integer]): Array[java.lang.String] = ints.map(_.toString)
  
  def toFloats(ints: Array[java.lang.Integer]): Array[java.lang.Float] =
    ints.map(i => new java.lang.Float(i.floatValue))
  
  def toDoubles(ints: Array[java.lang.Integer]): Array[java.lang.Double] =
    ints.map(i => new java.lang.Double(i.doubleValue))

  def main(args:Array[String]):Unit = {
    var mc: Class[_ <: mutable.Map[AnyRef, AnyRef]] = classOf[mutable.HashMap[AnyRef, AnyRef]]
    if (args.length > 0) 
      mc = Class.forName(args(0)).asInstanceOf[Class[mutable.Map[AnyRef, AnyRef]]]
    if (args.length > 1) 
      SIZE = Integer.parseInt(args(1))


    System.out.print("Class " + mc.getName())
    System.out.print(" size " + SIZE)
    System.out.println()
    val seq = Array.tabulate[java.lang.Integer](SIZE)(new java.lang.Integer(_))

    val shf = java.util.Arrays.copyOf(seq, seq.length)
    java.util.Collections.shuffle(java.util.Arrays.asList(shf))
    val jobs = Seq(
      new Hasher("Integer sequential", seq, mc),
      new Hasher("Integer shuffled", shf, mc),
      new Hasher("Long    sequential", toLongs(seq), mc),
      new Hasher("Long    shuffled", toLongs(shf), mc),
      new Hasher("Float   sequential", toFloats(seq), mc),
      new Hasher("Float   shuffled", toFloats(shf), mc),
      new Hasher("Double  sequential", toDoubles(seq), mc),
      new Hasher("Double  shuffled", toDoubles(shf), mc),
      new Hasher("String  sequential", toStrings(seq), mc),
      new Hasher("String  shuffled", toStrings(shf), mc)
    )
    System.out.print("warmup...")
    time0(NANOS_PER_WARMUP, jobs: _*)
    time0(NANOS_PER_WARMUP, jobs: _*)

    {
      var i = 0
      while (i < 2) {
        System.gc()
        Thread.sleep(50)
        System.runFinalization()
        Thread.sleep(50)
        i += 1
      }
    }

    System.out.println("starting")
    time(jobs: _*)
  }
  
  final class Hasher(name: String, elts: Array[_ <: AnyRef],
      mapClass: Class[_ <: mutable.Map[AnyRef, AnyRef]]) extends Job(name) {
    @volatile var matches: Int = _
    
    def work {
      val m = mapClass.newInstance
      val len = elts.length
      
      var j = 0
      while (j < ITERS_PER_TEST) {
        var i = 0
        while (i < elts.length) {
          val x = elts(i)
          if (m.put(x, x).isDefined) throw new Error
          i += 1
        }
        
        if (m.size != len) throw new Error
        
        var ng = 0
        i = 0
        while (i < elts.length) {
          val x = elts(i)
          if (m(x) == x) ng += 1
          i += 1
        }
        matches += ng
        
        m.keySet.foreach(e => if (m.get(e).get == e) ng -= 1)
        
        if (ng != 0) throw new Error

        i = 0
        while (i < elts.length) {
          val x = elts(i)
          if (m.remove(x).get != x) throw new Error
          i += 1
        }
        
        if (!m.isEmpty) throw new Error
        j += 1
      }
      
      if (matches != len * ITERS_PER_TEST) throw new Error
      matches = 0
    }
  }
}
