package com.cs.rutgers

/**
  * Created by zxj on 10/31/17.
  */
import scala.collection.mutable


class MixHeap(var value: Int,
  maxHeap: mutable.PriorityQueue[Int],
  minHeap: mutable.PriorityQueue[Int]) {
  var total: Int = value
  var numCount: Int = 1

  def this(value: Int) = this (value,
    mutable.PriorityQueue.empty,
    mutable.PriorityQueue.empty(MinOrder))

  def insert(input: Int) = {
    input - value match {
      case x if x >= 0 => minHeap.enqueue(input)
      case _ => maxHeap.enqueue(input)
    }
    total += input
    numCount += 1
    balance()
  }

  def balance() = {
    val difference = numberDifference()
    if (difference >= 2) {
      minHeap.enqueue(value)
      value = maxHeap.dequeue()
    }

    if (difference <= -2){
      maxHeap.enqueue(value)
      value = minHeap.dequeue()
    }
  }

  def calculateMedian() = {
    numberDifference() match {
      case 0 => value
      case x if x > 0 => scala.math.round((value + maxHeap.head) / 2.0f)
      case _ => scala.math.round((value + minHeap.head) / 2.0f)
    }
  }

  def numberDifference() = maxHeap.size - minHeap.size

}

object MinOrder extends Ordering[Int] {
  def compare(x:Int, y:Int) = y compare x
}
