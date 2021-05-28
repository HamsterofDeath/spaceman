package me.spaceman.common

import java.io.File

object Words {
  def loadAllWords = {
    val stream = Words.getClass.getResource("dictionary.txt").openStream()
    val data = new String(stream.readAllBytes(), "UTF-8").linesIterator.toSet.groupBy(_.length)
    stream.close()
    data
  }
}
