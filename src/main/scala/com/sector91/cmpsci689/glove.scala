package com.sector91.cmpsci689

import java.io.{FileOutputStream, PrintStream, PrintWriter}
import java.nio.file.{Files, Paths, Path}
import java.text.SimpleDateFormat
import java.util.Calendar

import breeze.linalg._

import scala.collection.mutable.AnyRefMap
import scala.collection.parallel.ParMap

object glove {

  type V = DenseVector[Double]

  def cos(a: V, b: V): Double = {
    val num: Double = a dot b
    val den: Double = Math.sqrt(norm(a)) * Math.sqrt(norm(b))
    num / den
  }

  def scos(a: V, b: V): Double = {
    val num: Double = a dot b
    val den: Double = norm(a) * norm(b)
    ((num / den) + 1.0) / 2.0
  }

  def argmax[K <: AnyRef, V](src: ParMap[K, V])(fn: V => Double): K =
    src.aggregate((null.asInstanceOf[K], Double.NegativeInfinity))((last, t) => {
      val next = fn(t._2)
      if (next > last._2) t._1 -> next else last
    }, (a, b) => if (a._2 > b._2) a else b)._1

  def cosaddAnalogy(lines: AnyRefMap[String, V])(a: String, b: String, x: String): String = {
    val av = lines(a)
    val bv = lines(b)
    val xv = lines(x)
    argmax((lines - a - b - x).par)(yv => cos(yv, xv - av + bv))
  }

  def cosmulAnalogy(lines: AnyRefMap[String, V])(a: String, b: String, x: String): String = {
    val av = lines(a)
    val bv = lines(b)
    val xv = lines(x)
    argmax((lines - a - b - x).par)(yv =>
      (scos(yv, bv) * scos(yv, xv)) / (scos(yv, av) + Double.MinPositiveValue)
    )
  }

  // ------------------------------------------------------------

  def vectorsFromFile(path: Path): AnyRefMap[String, V] = {
    val map = new AnyRefMap[String, V]()
    scala.io.Source.fromFile(path.toUri)(scala.io.Codec.UTF8).getLines() foreach { line =>
      val arr = line split "\\s"
      map.put(arr.head, DenseVector(arr.tail map java.lang.Double.parseDouble))
    }
    map
  }

  def analogiesFromFolder(path: Path): Iterator[(String, String, String, String)] =
    path.toFile.list().iterator.flatMap(file =>
      scala.io.Source.fromFile(path.resolve(file).toUri)(scala.io.Codec.UTF8).getLines() map { line =>
        val words = line split "\\s+"
        try { (words(0), words(1), words(2), words(3)) } catch {
          case ex: IndexOutOfBoundsException =>
            throw new IllegalStateException(s"Line '$line' is invalid", ex)
        }
      }
    )

  // ------------------------------------------------------------

  private val format = new SimpleDateFormat("h:mm:ss a")
  private def now = format.format(Calendar.getInstance().getTime)

  def analogiesPercentage(vectorFile: String, analogiesFolder: String): (Double, Double) = {
    val analogies = analogiesFromFolder(Paths get s"./data" resolve analogiesFolder).toArray
    val count = analogies.length
    val vectorPath = Paths get "./data" resolve vectorFile
    val vectors = vectorsFromFile(vectorPath)
    val result = analogies.foldLeft(DenseVector(0, 0, 0))({ (accum, analogy) =>
      if (accum(0) % 50 == 0) {
        println(s"Completed ${accum(0)}/$count analogies (${((accum(0).toDouble / count.toDouble) * 100.0).toInt}%) - $now")
      }
      analogy match {
        case (a, b, x, y) =>
          val addMatch = cosaddAnalogy(vectors)(a, b, x)
          val mulMatch = cosmulAnalogy(vectors)(a, b, x)
          //println(s"$a:$b::$x:$y (add: $addMatch, mul: $mulMatch) - $now")
          accum + DenseVector(1, if (addMatch == y) 1 else 0, if (mulMatch == y) 1 else 0)
      }
    })
    val total = result(0)
    val add = result(1)
    val mul = result(2)
    ((add.toDouble / total.toDouble) * 100.0, (mul.toDouble / total.toDouble) * 100.0)
  }

  def runMSR(vectorFile: String, out: PrintStream = System.out): Unit = {
    out.println(s"STARTED at $now")
    val (addPercent, mulPercent) = analogiesPercentage(vectorFile, "msr")
    out.println(s"ENDED at $now")
    out.println(s"MSR analogies COSADD performance: $addPercent%")
    out.println(s"MSR analogies COSMUL performance: $mulPercent%")
  }

  def runGoogle(vectorFile: String, out: PrintStream = System.out): Unit = {
    out.println(s"STARTED at $now")
    val (addPercent, mulPercent) = analogiesPercentage(vectorFile, "google")
    out.println(s"ENDED at $now")
    out.println(s"Google analogies COSADD performance: $addPercent%")
    out.println(s"Google analogies COSMUL performance: $mulPercent%")
  }

  def runAllAndSaveToFile(vectorFile: String, outFile: String): Unit = {
    val outStream = new PrintStream(Files newOutputStream (Paths get outFile))
    runMSR(vectorFile, outStream)
    runGoogle(vectorFile, outStream)
    outStream.close()
  }
}
