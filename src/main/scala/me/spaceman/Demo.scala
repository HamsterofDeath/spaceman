package me.spaceman

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object Demo {
  def main(args: Array[String]): Unit = {
    var leben = 5
    val wort = "gogogaga"
    val scanner = new Scanner(System.in)
    val geraten = ArrayBuffer.empty[Char]
    while (leben > 0) {
      def hinweis = {
        wort.map { buchstabe =>
          if (geraten.contains(buchstabe)) {
            buchstabe
          } else {
            '_'
          }
        }
      }

      println(s"Du hast noch $leben Versuche, hinweis: $hinweis")
      print("Bitte gib einen Buchstaben ein -> ")
      val eingegeben = scanner.nextLine()
      println(s"Du hast $eingegeben eingegeben")

      val istRichtig = wort.toLowerCase.contains(eingegeben.toLowerCase)
      if (istRichtig) {
        println("Das war richtig")
        geraten ++= eingegeben
      } else {
        println("Das war falsch")
        leben -= 1
      }


    }
  }
}
