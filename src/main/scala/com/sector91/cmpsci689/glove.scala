package com.sector91.cmpsci689

import java.nio.charset.Charset
import java.nio.file.{Paths, Files, Path}
import java.text.SimpleDateFormat
import java.util.Calendar

import breeze.linalg._

import scala.collection.JavaConversions._

object glove {

  type V = DenseVector[Double]

  def cos(a: V, b: V): Double = (a.t * b) / (Math.sqrt(norm(a)) * Math.sqrt(norm(b)))

  def argmax[T](src: Seq[T])(fn: T => Double): T = {
    val first = src.head
    src.tail.par.aggregate(first -> fn(first))((last, t) => {
      val next = fn(t)
      if (next > last._2) t -> next else last
    }, (a, b) => if (b._2 > a._2) b else a)._1
  }

  private def find3(lines: Iterator[(String, V)])(a: String, b: String, x: String): (V, V, V) = {
    var av = Option.empty[V]
    var bv = Option.empty[V]
    var xv = Option.empty[V]
    var searching = true
    lines takeWhile (_ => searching) collect {
      case (l, v) if l == a =>
        av = Some(v)
        bv.isEmpty || xv.isEmpty
      case (l, v) if l == b =>
        bv = Some(v)
        av.isEmpty || xv.isEmpty
      case (l, v) if l == x =>
        xv = Some(v)
        av.isEmpty || bv.isEmpty
    } foreach (searching = _)
    (
      av getOrElse (throw new RuntimeException(s"Word $a not found in dataset")),
      bv getOrElse (throw new RuntimeException(s"Word $b not found in dataset")),
      xv getOrElse (throw new RuntimeException(s"Word $x not found in dataset"))
    )
  }

  def cosaddAnalogy(lines: Seq[(String, V)])(a: String, b: String, x: String): String = {
    val (av, bv, xv) = find3(lines.iterator)(a, b, x)
    argmax(lines)(line => cos(line._2, xv - av + bv))._1
  }

  def cosmulAnalogy(lines: Seq[(String, V)])(a: String, b: String, x: String): String = {
    val (av, bv, xv) = find3(lines.iterator)(a, b, x)
    argmax(lines) { case (_, yv) =>
      (cos(yv, bv) * cos(yv, xv)) / (cos(yv, av) + Double.MinPositiveValue)
    }._1
  }

  // ------------------------------------------------------------

  def vectorFromLine(line: String): (String, V) = {
    val arr = line split "\\s"
    (arr.head, DenseVector(arr.tail map java.lang.Double.parseDouble))
  }

  def vectorsFromFile(path: Path): Iterator[(String, V)] = {
    val reader = Files.newBufferedReader(path, Charset.defaultCharset())
    Iterator.continually(reader.readLine()).takeWhile(line =>
      if (line == null) { reader.close(); false } else true
    ).map(vectorFromLine)
  }

  def analogiesFromFolder(path: Path): Iterator[(String, String, String, String)] =
    path.toFile.list().iterator.flatMap { file =>
      val reader = Files.newBufferedReader(path resolve file, Charset.defaultCharset())
      Iterator.continually(reader.readLine()).takeWhile(line =>
        if (line == null) { reader.close(); false } else true
      ) map { line =>
        val words = line split "\\s+"
        try { (words(0), words(1), words(2), words(3)) } catch {
          case ex: IndexOutOfBoundsException =>
            throw new IllegalStateException(s"Line '$line' is invalid", ex)
        }
      }
    }

  // ------------------------------------------------------------

  def googleAnalogiesPercentage(vectorFile: String): (Double, Double) = {
    val analogies = analogiesFromFolder(Paths get "./data/google").toArray
    val count = analogies.length
    val vectorPath = Paths get "./data" resolve vectorFile
    val storedVectors = vectorsFromFile(vectorPath).toArray
    val result = analogies.foldLeft(DenseVector(0, 0, 0)) { (accum, analogy) =>
      if (accum(0) % 50 == 0) {
        println(s"Completed ${accum(0)}/$count analogies (${((accum(0).toDouble / count.toDouble) * 100.0).toInt}%)")
      }
      analogy match {
        case (a, b, x, y) =>
          val addMatch = cosaddAnalogy(storedVectors)(a, b, x) == y
          val mulMatch = cosmulAnalogy(storedVectors)(a, b, x) == y
          accum + DenseVector(1, if (addMatch) 1 else 0, if (mulMatch) 1 else 0)
      }
    }
    val total = result(0)
    val add = result(1)
    val mul = result(2)
    ((add.toDouble / total.toDouble) * 100.0, (mul.toDouble / total.toDouble) * 100.0)
  }

  def runGoogle(vectorFile: String): Unit = {
    val format = new SimpleDateFormat("h:mm:ss a")
    println(s"STARTED at ${format.format(Calendar.getInstance().getTime)}")
    val (addPercent, mulPercent) = googleAnalogiesPercentage(vectorFile)
    println(s"ENDED at ${format.format(Calendar.getInstance().getTime)}")
    println(s"Google analogies COSADD performance: $addPercent%")
    println(s"Google analogies COSMUL performance: $mulPercent%")
  }
}
