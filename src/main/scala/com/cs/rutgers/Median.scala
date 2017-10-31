package com.cs.rutgers

import scala.annotation.tailrec

/**
  * Created by zxj on 10/26/17.
  */
object Median {
  def quickselect(k: Int, xs: Array[Int]): Option[Int] = {
    def randint(lo: Int, hi: Int): Int =
      lo + util.Random.nextInt( (hi - lo) + 1 )

    @inline def swap(xs: Array[Int],i: Int, j: Int): Unit = {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }

    def partition(xs: Array[Int], l: Int, r: Int): Int = {
      var pivotIndex = randint(l, r)
      val pivotValue = xs(pivotIndex)
      swap(xs, r, pivotIndex)
      pivotIndex = l

      var i = l
      while (i <= r-1) {
        if (xs(i) < pivotValue) {
          swap(xs, i, pivotIndex)
          pivotIndex = pivotIndex + 1
        }
        i = i + 1
      }
      swap(xs, r, pivotIndex)
      pivotIndex
    }

    @tailrec
    def quickselect0(xs: Array[Int], l: Int, r: Int, k: Int): Int = {
      if (l == r) {
        xs(l)
      } else {
        val pivotIndex = partition(xs, l, r)
        k compare pivotIndex match {
          case 0 => xs(k)
          case -1 => quickselect0(xs, l, pivotIndex - 1, k)
          case 1 => quickselect0(xs, pivotIndex + 1, r, k)
        }
      }
    }

    xs match {
      case _ if xs.isEmpty => None
      case _ if k < 0 || k >= xs.length => None
      case _ => Some(quickselect0(xs, 0, xs.length - 1, k))
    }
  }

  def median(xs: Array[Int]): Int = {
    if (xs.length %2 == 0){
      val first = quickselect(xs.length / 2 - 1, xs).get
      val second = quickselect(xs.length /2, xs).get
      Math.round((first + second) / 2.0f)
    }
    else {
      quickselect((xs.length - 1)/2, xs).get
    }
  }
}
