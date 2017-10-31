package com.cs.rutgers

import scala.annotation.tailrec
import scala.collection.mutable


class PairedIterable[K, V](x: Iterable[(K, V)]) {
  def reduceByKey(func: (V,V) => V): mutable.HashMap[K, V] = {
    val map = new mutable.HashMap[K, V]()

    @tailrec
    def reduce(it: Iterable[(K, V)]): mutable.HashMap[K, V] = {
      it match {
        case Nil => map
        case (k, v) #:: tail =>
          val old = map.get(k)
          map.put(k, if (old.isEmpty) v else func(old.get, v))
          reduce(tail)
      }
    }
    reduce(x)
  }
}
