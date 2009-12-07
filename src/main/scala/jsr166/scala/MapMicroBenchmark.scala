/*
 * Original Java version Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 * 
 * jatran used for initial translation to Scala
 */

package jsr166.scala

import jsr166._
import _root_.scala.collection._

/**
 * A micro-benchmark with key types and operation mixes roughly
 * corresponding to some real programs.
 *
 * The main results are a table of approximate nanoseconds per
 * element-operation (averaged across get, put etc) for each type,
 * across a range of map sizes. It also includes category "Mixed"
 * that includes elements of multiple types including those with
 * identical hash codes.
 *
 * The program includes a bunch of microbenchmarking safeguards that
 * might underestimate typical performance. For example, by using many
 * different key types and exercising them in warmups it disables most
 * dynamic type specialization.  Some test classes, like Float and
 * BigDecimal are included not because they are commonly used as keys,
 * but because they can be problematic for some map implementations.
 *
 * By default, it creates and inserts in order dense numerical keys
 * and searches for keys in scrambled order. Use "s" as second arg to
 * instead insert and search in unscrambled order.
 *
 * For String keys, the program tries to use file "testwords.txt", which
 * is best used with real words.  We can't check in this file, but you
 * can create one from a real dictionary (1 line per word) and then run
 * linux "shuf" to randomize entries. If no file exists, it uses
 * String.valueOf(i) for element i.
 */
object MapMicroBenchmark {
  val wordFile = "testwords.txt"
  var mapClass: Class[Map[_, _]] = _
  var randomSearches = true
  
  // Nanoseconds per run
  val NanosPerJob = 6L * 1000L * 1000L * 1000L
  val NanosPerWarmup = 100L * 1000L* 1000L
  
  // map operations per item per iteration -- change if job.work changed
  val OpsPerIter = 11
  val MinItersPerTest = 3 // must be > 1
  val MaxItersPerTest = 1000000 // avoid runaway
  
  // sizes are at halfway points for HashMap default resizes
  val firstSize = 9
  val sizeStep = 4 // each size 4X last
  val nsizes = 9
  val sizes = new Array[Int](nsizes)
  val rng = new java.util.Random(3122688)
  
  def main(args: Array[String]) {
    if (args.length == 0) {
      println("Usage: java MapMicroBenchmark className [r|s]keys [r|s]searches")
      return
    }

    mapClass = Class.forName(args(0)).asInstanceOf[Class[Map[_, _]]]
    if (args.length > 1) {
      if (args(1).startsWith("s")) 
        randomSearches = false
      else if (args(1).startsWith("r"))
        randomSearches = true
    }

    print("Class " + mapClass.getName())
    if (randomSearches) print(" randomized searches")
    else print(" sequential searches")

    println()
    var n = firstSize
    
    for (i <- 0 until (nsizes - 1)) {
      sizes(i) = n
      n *= sizeStep
    }

    sizes(nsizes - 1) = n
    val njobs = 10
    val jobs = new Array[Job](njobs)

    val os = Array.tabulate(n)(_ => new Object)
    jobs(0) = new Job("Object    ", os, classOf[Object])
    
    val ss = new Array[Object](n)
    initStringKeys(ss, n)
    jobs(1) = new Job("String    ", ss, classOf[String])
    
    val is = Array.tabulate[Object](n)(java.lang.Integer.valueOf)
    jobs(2) = new Job("Integer   ", is, classOf[Integer])
    
    // Avoid overload with ints as Scala equality between ints and longs is different from Java
    val ls = Array.tabulate[Object](n)(i => java.lang.Long.valueOf(n + i.toLong))
    jobs(3) = new Job("Long      ", ls, classOf[java.lang.Long])
    
    val fs = Array.tabulate[Object](n)(i => java.lang.Float.valueOf(i.toFloat))
    jobs(4) = new Job("Float     ", fs, classOf[java.lang.Float])
 
    val ds = Array.tabulate[Object](n)(i => java.lang.Double.valueOf(i.toDouble))
    jobs(5) = new Job("Double     ", ds, classOf[java.lang.Double])
    
    var b: Long = -n // include some negatives
    val bs = Array.tabulate[Object](n)(i => java.math.BigInteger.valueOf({b += 3; b}))
    jobs(6) = new Job("BigInteger", bs, classOf[java.math.BigInteger])
    
    var d = Integer.MAX_VALUE.toLong // include crummy codes
    val es = Array.tabulate[Object](n)(i => java.math.BigDecimal.valueOf({d += 65536; d}))
    jobs(7) = new Job("BigDecimal", es, classOf[java.math.BigDecimal])
    
    val rs = Array.tabulate[Object](n)(_ => new RandomInt)
    jobs(8) = new Job("RandomInt ", rs, classOf[RandomInt])
    
    val ms = new Array[Object](n);
    {
      var i = 0
      while (i < n) {
        var r = rng.nextInt(njobs - 1)
        ms(i) = jobs(r).items(i)
        r += 1
        // include some that will have same hash but not .equal
        if (r >= njobs - 1) r = 0

        ms(i + 1) = jobs(r).items(i)
        i += 2
      }
    }
    jobs(9) = new Job("Mixed     ", ms, classOf[Object])

    val mixed = jobs(9)
    warmup1(mixed)
    warmup2(jobs)
    warmup1(mixed)
    warmup3(jobs)
    Thread.sleep(500)
    time(jobs)
  }

  def runWork(jobs: Array[Job], minIters: Int, maxIters: Int, timeLimit:Long) {
    for (k <- 0 until nsizes) {
      val len = sizes(k)
      for (i <- 0 until jobs.length) {
        Thread.sleep(50)
        jobs(i).nanos(k) = jobs(i).work(len, minIters, maxIters, timeLimit)
        print(".")
      }
    }
    println()
  }

  // First warmup -- run only mixed job to discourage type specialization
  def warmup1(job: Job) {
    (0 until nsizes).foreach(k => job.work(sizes(k), 1, 1, 0))
  }

  // Second, run each once
  def warmup2(jobs: Array[Job]) {
    print("warm up")
    runWork(jobs, 1, 1, 0)
    val ck = jobs(0).checkSum

    var i = 1
    while (i < jobs.length - 1) {
      if (jobs(i).checkSum != ck) throw new Error("CheckSum")
      i += 1
    }
  }
  
  // Third: short timed runs
  def warmup3(jobs: Array[Job]) {
    print("warm up")
    runWork(jobs, 1, MaxItersPerTest, NanosPerWarmup)
  }

  def time(jobs: Array[Job]) {
    print("running")
    runWork(jobs, MinItersPerTest, MaxItersPerTest, NanosPerJob)
    print("Type/Size:")

    {
      var k = 0
      while (k < nsizes) {
        printf("%7d", sizes(k))
        k += 1
      }
    }

    println()
    val aves = new Array[Long](nsizes)
    val njobs = jobs.length

    {
      var i = 0
      while (i < njobs) {
        print(jobs(i).name)

        var k = 0
        while (k < nsizes) {
          val nanos = jobs(i).nanos(k)
          printf("%7d", nanos)
          aves(k) += nanos
          k += 1
        }

        println()
        i += 1
      }
    }

    println()
    print("average   ")

    {
      var k = 0
      while (k < nsizes) {
        printf("%7d", aves(k) / njobs)
        k += 1
      }
    }

    println("\n")
  }

  // Shuffle the subarrays for each size. This doesn't fully
  // randomize, but the remaining partial locality is arguably a bit
  // more realistic
  def scramble(a: Array[AnyRef]) {
    for (k <- 0 until sizes.length) {
      val origin = if (k == 0) 0 else sizes(k - 1)

      var i = sizes(k)
      while (i > origin + 1) {
        val t = a(i - 1)
        val r = rng.nextInt(i - origin) + origin
        a(i - 1) = a(r)
        a(r) = t
        i = i - 1
      }
    }
  }
  
  // plain array shuffle
  def shuffle(a: Array[AnyRef], size: Int) {
    var i = size
    while (i > 1) {
      val t = a(i - 1)
      val r = rng.nextInt(i)
      a(i - 1) = a(r)
      a(r) = t
      i -= 1
    }
  }
  
  // swap nswaps elements
  def shuffleSome(a:Array[Object], size:Int, nswaps:Int):Unit = {
    var s = 0
    while (s < nswaps) {
      val i = rng.nextInt(size)
      val r = rng.nextInt(size)
      val t = a(i)
      a(i) = a(r)
      a(r) = t
      s += 1
    }
  }

  // Read in String keys from file if possible
  def initStringKeys(keys: Array[AnyRef], n: Int) {
    try {
      val in = new java.io.BufferedReader(new java.io.InputStreamReader(
          currentThread.getContextClassLoader.getResourceAsStream(wordFile)))
      var line: String = null
      var k = 0
      while(k < n && {line = in.readLine; line != null}) {
        keys(k) = line
        k += 1
      }
      in.close()
    
      // fill up remaining keys with path-like compounds of previous pairs
      var j:Int = 0
      while (k < n) keys({k += 1; k - 1}) = keys({j += 1; j - 1}).asInstanceOf[String]  + "/" + keys(j).asInstanceOf[String] 
    } catch { 
      case ex: java.io.IOException => 
        println("No word file. Using String.valueOf(i)")
        (0 until n).foreach(i => keys(i) = String.valueOf(i))
    }
  }
  
  object RandomInt {
    var seed = 3122688 // a non-xorshift, 2^32-period RNG
    def next: Int = {
      val x = seed
      var lo = 16807 * (x & 0xFFFF)
      var hi = 16807 * (x >>> 16)
      lo += (hi & 0x7FFF) << 16
      if ((lo & 0x80000000) != 0) {
        lo &= 0x7fffffff
        lo += 1
      }
      lo += hi >>> 15
      if (lo == 0 || (lo & 0x80000000) != 0) {
        lo &= 0x7fffffff
        lo += 1
      }
      seed = lo
      x
    }
  }
  
  // Integer-like class with random hash codes
  final class RandomInt {
    val value = RandomInt.next
    override def hashCode = value
    override def equals(o: Any): Boolean = o match {
      case ri: RandomInt => ri.value == value
      case _ => false
    }
  }
  
  class Job(val name: String, val items: Array[AnyRef], val elementClass: Class[_]) {
    val nanos = new Array[Long](nsizes)
    
    val searches = {
      if (randomSearches) {
        scramble(items)
        val s = java.util.Arrays.copyOf(items, items.length)
        scramble(s)
        s
      }
      else items
    }
    
    @volatile var checkSum: Long = _
    @volatile var lastSum: Int = _
    
    def work(len: Int, minIters: Int, maxIters: Int, timeLimit: Long): Long = {
      val m = mapClass.newInstance().asInstanceOf[mutable.Map[AnyRef, AnyRef]]
      val ins = items
      val keys = searches
      
      if (ins.length < len || keys.length < len) throw new Error(name)
      
      val half = len / 2
      val quarter = half / 2
      var sum = lastSum
      val startTime = System.nanoTime
      var elapsed = 0L
      var j = 0
      
      var finished = false
      while (!finished) {
        var i = 0
        while (i < half) {
          val x = ins(i)
          if (m.put(x, x).isEmpty) sum += 1
          i += 1
        }
        checkSum += sum ^ (sum << 1) // help avoid loop merging
        sum += len - half
        
        i = 0
        while (i < len) {
          val x = keys(i)
          val v = m.get(x)
          if (v.isDefined && elementClass.isInstance(v.get)) // touch v
            sum += 1
          i += 1
        }
        checkSum += sum ^ (sum << 2)
        i = half
        while (i < len) {
          val x = ins(i)
          if (m.put(x, x).isEmpty) sum += 1
          i += 1
        }
        checkSum += sum ^ (sum << 3);
        for (e <- m.keySet) {
          if (elementClass.isInstance(e)) sum += 1
        }
        checkSum += sum ^ (sum << 4);
        for (e <- m.valuesIterable) {
          if (elementClass.isInstance(e)) sum += 1
        }
        checkSum += sum ^ (sum << 5);
        i = len - 1
        while (i >= 0) {
          val x = keys(i);
          val v = m.get(x);
          if (v.isDefined && elementClass.isInstance(v.get)) sum += 1
          i -= 1
        }
        checkSum += sum ^ (sum << 6);
        i = 0
        while (i < len) {
          val x = ins(i);
          val v = m.get(x);
          if (v.isDefined && elementClass.isInstance(v.get)) sum += 1
          i += 1
        }
        checkSum += sum ^ (sum << 7);
        i = 0
        while (i < len) {
          val x = keys(i)
          val v = ins(i)
          val prev = m.put(x, v)
          if (prev.isDefined && (prev.get eq x)) sum += 1
          i += 1
        }
        checkSum += sum ^ (sum << 8);
        i = 0
        while (i < len) {
          val x = keys(i)
          val v = ins(i)
          val r = m.get(x)
          if (r.isDefined && (r.get eq v)) sum += 1
          i += 1
        }
        checkSum += sum ^ (sum << 9);
        i = len - 1
        while (i >= 0) {
          val x = ins(i);
          val v = m.get(x);
          if (v.isDefined && elementClass.isInstance(v.get)) sum += 1
          i -= 1
        }
        checkSum += sum ^ (sum << 10);
        i = len - 1
        while (i >= 0) {
          val x = keys(i)
          val v = ins(i);
          val r = m.get(x)
          if (r.isDefined && (r.get eq v)) sum += 1
          i -= 1
        }
        checkSum += sum ^ (sum << 11);
        i = 0
        while (i < quarter) {
          val x = keys(i)
          if (m.remove(x).isDefined) sum += 1
          i += 1
        }
        i = 0
        while (i < quarter) {
          val x = keys(i)
          if (m.put(x, x).isEmpty) sum += 1
          i += 1
        }
        m.clear;
        sum += len - (quarter * 2);
        checkSum += sum ^ (sum << 12);
        if (j == 0 && sum != lastSum + len * OpsPerIter) {
//          println("name " + name)
//          println("j " + j)
//          println("sum " + sum)
//          println("lastSum " + lastSum)
//          println("len " + len)
//          println("OpsPerIter " + OpsPerIter)
//          println("expectedSum " + (lastSum + len * OpsPerIter))
//          println("items " + java.util.Arrays.toString(items.slice(0, 10)))
          throw new Error(name);
        }

        elapsed = System.nanoTime() - startTime;
        j += 1
        if (j >= minIters && (j >= maxIters || elapsed >= timeLimit))
          finished = true
        // non-warmup - swap some keys for next insert
        else if (minIters != 1 && randomSearches) shuffleSome(ins, len, len >>> 3)
      }
      
      val ops = j.toLong * len * OpsPerIter
      lastSum = sum
      elapsed / ops
    }
  }
}