package me.scala3features

import me.scala3features.Stuff.TriLean

def outside = "hello"

object Stuff {
  enum TriLean {
    case Yes
    case No
    case Maybe
  }

  val text = outside

  class FakeString(val s:String) {
    override def toString = s"FakeString(s=$s)"
  }

  extension(fs:FakeString)
    def reverse = FakeString(fs.s.reverse)

}

@main def reverseIt: Unit = {
  println(Stuff.FakeString("hi").reverse)
  import Stuff.TriLean._
  val t:TriLean = Yes
  println(Stuff.TriLean.Maybe)


  println(List(1,2,3,4,5).sum)
}
