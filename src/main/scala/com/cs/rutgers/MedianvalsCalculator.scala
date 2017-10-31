package com.cs.rutgers

import java.io._
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import com.cs.rutgers.Median.median

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}
/**
  * Created by zxj on 10/25/17.
  */
object MedianvalsCalculator {
  val monthDayMap = mutable.HashMap(
    "01" -> "31",
    "02" -> "28",
    "03" -> "31",
    "04" -> "30",
    "05" -> "31",
    "06" -> "30",
    "07" -> "31",
    "08" -> "31",
    "09" -> "30",
    "10" -> "31",
    "11" -> "30",
    "12" -> "31"
  )


  def main (args: Array[String]): Unit = {
    val inputPath = args(0)
    val outputPath = args(1)
    process(inputPath, outputPath)
  }

  def process(inputPath: String, outputRoot: String) = {
    val iter = Source.fromFile(inputPath).getLines()
    val personContribIter = iter.map { s =>
      val stringList = s.split("\\|", -1).map(_.trim)
      PersonalContribution(stringList(0), stringList(10), stringList(13), stringList(14), stringList(15))
    }
    val validData = personContribIter.filter{ contrib =>
      contrib.cmteID.nonEmpty && !contrib.transactionAmout.isEmpty && contrib.otherID.isEmpty
    }

    val validList = validData.toList
    val valid1 = validList.iterator
    val valid2 = validList.iterator

    val resultByZiP = calculateMedianvalsByZiP(valid1.toIterable)
    outputToPath(resultByZiP, Paths.get(outputRoot, "medianvals_by_zip.txt").toString)

    val resultByDate = calculateMedianvalsByDate(valid2.toIterable)
    outputToPath(resultByDate, Paths.get(outputRoot, "medianvals_by_date.txt").toString)

  }

  def outputToPath(content: TraversableOnce[String], path: String) = {
    try {
      val writer = new BufferedWriter(new FileWriter(path))
      content.foreach (line => writer.write(line + "\n"))
      writer.close()
    }
    catch {
      case fileNotFound: FileNotFoundException => {
        fileNotFound.printStackTrace()
      }
      case ioException: IOException => {
        ioException.printStackTrace()
      }
    }
  }


  def getTransactionAmount(contrib: PersonalContribution): Option[Int] = {
    val value = Try[Int](contrib.transactionAmout.toInt)
    value match {
      case Success(value) => if (value > 0) Some(value) else None
      case Failure(_) => None
    }
  }

  def calculateMedianvals(
    personContribIter: Iterable[PersonalContribution],
    predicate: PersonalContribution => Boolean,
    transformation: PersonalContribution => ((String, String), Option[Int])): Seq[String] = {
    val zipIterable = personContribIter
      .filter(predicate)
      .map(changeZipCode)
      .map(transformation)
      .filter(_._2.nonEmpty)
      .map(tup => (tup._1, TransactionPerPerson(Array(tup._2.get), 1)))

    val zipPair = new PairedIterable[(String, String), TransactionPerPerson](zipIterable)
    zipPair.reduceByKey(reduceTransactionPerPerson)
      .map(tup => keyValueEntryToList(tup._1, tup._2))
      .toSeq
        .sortBy(arr => (arr(0), formatDate(arr(1))))
      .map(_.mkString("|"))

  }

  def formatDate(date: String) = date.takeRight(4) + date.take(2) + date.drop(2).take(2)

  def calculateMedianvalsByZiP(personContribIter: Iterable[PersonalContribution]) = {
    val zipIDMap = new mutable.HashMap[(String, String), MixHeap]()
    val transformation = (item: PersonalContribution) => ((item.cmteID, item.zipCode), getTransactionAmount(item))
    val zipIterable = personContribIter
      .filter(validZipCode)
      .map(changeZipCode)
      .map(transformation)
      .filter(_._2.nonEmpty)
        .map{ tup =>
          val value = zipIDMap.get(tup._1)
          if (value.isEmpty) {
            zipIDMap.put(tup._1, new MixHeap(tup._2.get))
            Array(tup._1._1, tup._1._2, tup._2.get, 1, tup._2.get).mkString("|")
          } else {
              val mixedHeap = value.get
              mixedHeap.insert(tup._2.get)
              Array(tup._1._1, tup._1._2,
              mixedHeap.calculateMedian(),
              mixedHeap.numCount,
              mixedHeap.total).mkString("|")
          }
        }
    zipIterable

  }

  def calculateMedianvalsByDate(personContribIter: Iterable[PersonalContribution]) = {
    calculateMedianvals(personContribIter = personContribIter,
      predicate = validDate,
      transformation = (item: PersonalContribution) => ((item.cmteID, item.transactionDate),
        getTransactionAmount(item))
    )

  }

  def reduceTransactionPerPerson(first: TransactionPerPerson, second: TransactionPerPerson) = {
    TransactionPerPerson(first.transactions ++ second.transactions, first.numContribution + second.numContribution)
  }

  def keyValueEntryToList(key: (String, String), value: TransactionPerPerson) = {
    val total = value.transactions.sum
    val median_trans = median(value.transactions)
    Array(key._1, key._2, median_trans.toString, value.numContribution.toString, total.toString)
  }


  def takeFirstFiveNumber(input: String): String = if (input.length > 5) input.take(5) else input

  def validZipCode(personalContrib: PersonalContribution): Boolean = {
     personalContrib.zipCode.length == 5 || personalContrib.zipCode.length == 9
  }

  def validDate(personalContrib: PersonalContribution) = {
    def isLeapYear(year: Int) = (year % 4, year % 100, year % 400) match {
      case (_, _, 0) => true
      case (_, 0, _) => false
      case (0, _, _) => true
      case _         => false
    }

    def validMonth(month: String, day: String): Boolean = {
      monthDayMap.contains(month) && day.compareTo(monthDayMap(month)) <= 0
    }

    val date = personalContrib.transactionDate

    val result = date.length == 8
    if (! result) {
      false
    }
    else {
      val optYear = Try(date.takeRight(4).toInt)
      val month = date.take(2)
      val day = date.drop(2).take(2)
      optYear match {
        case Success(value) => {
          val febDay = if (isLeapYear(value)) "29" else "28"
          monthDayMap.update("02", febDay)
          validMonth(month, day)
        }
        case Failure(_) => false
      }
    }
  }

  def changeZipCode(personalContrib: PersonalContribution): PersonalContribution = {
    val newZipCode = takeFirstFiveNumber(personalContrib.zipCode)
    PersonalContribution(
      personalContrib.cmteID,
      newZipCode,
      personalContrib.transactionDate,
      personalContrib.transactionAmout,
      personalContrib.otherID)
  }
}

case class PersonalContribution(cmteID: String, zipCode: String, transactionDate: String, transactionAmout: String,
  otherID: String)

case class TransactionPerPerson(transactions: Array[Int], numContribution: Int)