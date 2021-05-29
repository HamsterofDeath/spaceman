package me.spaceman.common

import java.io.File
import scala.util.Random

object Words {
  lazy val (minWordChars, maxWordChars) =
    val sizes = loadAllWords.keys.toList
    (sizes.min, sizes.max)
    
  private val random = new Random()
    
  def randomWordLength = random.between(minWordChars, maxWordChars+1)
  
  lazy val loadAllWords = {
    val stream = Words.getClass.getResource("dictionary.txt").openStream()
    val data = new String(stream.readAllBytes(), "UTF-8").linesIterator.toSet.groupBy(_.length)
    stream.close()
    data
  }

  def wordMatchesPattern(pattern: String, word: String, alreadyRevealed:Set[Char]) = {
    pattern.zip(word).forall { case (patternChar, testChar) =>
      patternChar == testChar || (patternChar == '_' && !alreadyRevealed(testChar))
    }
  }

}
