package com.sector91.cmpsci689

import java.nio.file.{Paths, Path}
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.LinkedHashMap

import breeze.linalg._

import scala.collection.JavaConversions._

object glove {

  type V = DenseVector[Double]

  def cos(a: V, b: V): Double = {
    val num: Double = a dot b
    val den: Double = Math.sqrt(norm(a)) * Math.sqrt(norm(b))
    num / den
  }

  def argmax[T](src: Iterator[T])(fn: T => Double): T = {
    val first = src.next()
    src.foldLeft(first -> fn(first))((last, t) => {
      val next = fn(t)
      if (next > last._2) t -> next else last
    })._1
  }

  def cosaddAnalogy(lines: LinkedHashMap[String, V])(a: String, b: String, x: String): String = {
    val av = lines get a
    val bv = lines get b
    val xv = lines get x
    argmax(lines.entrySet.iterator)(entry => cos(entry.getValue, xv - av + bv)).getKey
  }

  def cosmulAnalogy(lines: LinkedHashMap[String, V])(a: String, b: String, x: String): String = {
    val av = lines get a
    val bv = lines get b
    val xv = lines get x
    argmax(lines.entrySet.iterator) { entry =>
      val yv = entry.getValue
      (cos(yv, bv) * cos(yv, xv)) / (cos(yv, av) + Double.MinPositiveValue)
    }.getKey
  }

  // ------------------------------------------------------------

  def vectorsFromFile(path: Path): LinkedHashMap[String, V] = {
    val map = new LinkedHashMap[String, V]()
    scala.io.Source.fromFile(path.toUri).getLines() foreach { line =>
      val arr = line split "\\s"
      map.put(arr.head, DenseVector(arr.tail map java.lang.Double.parseDouble))
    }
    map
  }

  def analogiesFromFolder(path: Path): Iterator[(String, String, String, String)] =
    path.toFile.list().iterator.flatMap(file =>
      scala.io.Source.fromFile(path.resolve(file).toUri).getLines() map { line =>
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

  def googleAnalogiesPercentage(vectorFile: String): (Double, Double) = {
    val analogies = analogiesFromFolder(Paths get "./data/google").toArray
    val count = analogies.length
    val vectorPath = Paths get "./data" resolve vectorFile
    val vectors = vectorsFromFile(vectorPath)
    val result = analogies.par.aggregate(DenseVector(0, 0, 0))({ (accum, analogy) =>
      if (accum(0) % 50 == 0) {
        println(s"Completed ${accum(0)}/$count analogies (${((accum(0).toDouble / count.toDouble) * 100.0).toInt}%) - $now")
      }
      analogy match {
        case (a, b, x, y) =>
          println(s"#${accum(0)} - $a:$b::$x:$y - $now")
          val addMatch = cosaddAnalogy(vectors)(a, b, x) == y
          val mulMatch = cosmulAnalogy(vectors)(a, b, x) == y
          accum + DenseVector(1, if (addMatch) 1 else 0, if (mulMatch) 1 else 0)
      }
    }, _+_)
    val total = result(0)
    val add = result(1)
    val mul = result(2)
    ((add.toDouble / total.toDouble) * 100.0, (mul.toDouble / total.toDouble) * 100.0)
  }

  def runGoogle(vectorFile: String): Unit = {
    println(s"STARTED at $now")
    val (addPercent, mulPercent) = googleAnalogiesPercentage(vectorFile)
    println(s"ENDED at $now")
    println(s"Google analogies COSADD performance: $addPercent%")
    println(s"Google analogies COSMUL performance: $mulPercent%")
  }
}
