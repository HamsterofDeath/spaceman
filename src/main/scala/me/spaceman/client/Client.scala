package me.spaceman.client

import me.spaceman.network.commands.Commands.{GameCommand, GameStart, Guess, JoinGame, LeaveGame, SpielZustand}
import me.spaceman.server.Server

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import java.util.Scanner

object ClientFactory {

  class Context(update:SpielZustand, me:String) {
    def leave =
      val cmd = new LeaveGame
      cmd.gameID = update.gameID
      cmd.playerId = me
      cmd

    def makeGuess(c:Char) =
      val cmd = new Guess
      cmd.gameId = update.gameID
      cmd.playerId = me
      cmd.guess = c
      cmd

    def isFinished = update.endMessage!=null && update.endMessage.nonEmpty
    def isOpen = !isFinished
  }
  trait Client {
    def onUpdateReceived(update:Context):GameCommand
  }

  object Client {
    def consoleClient =
      new Client {
        val scanner = new Scanner(System.in)
        override def onUpdateReceived(update: Context): GameCommand =
          if (update.isOpen)
            print("Guess: ")
            val line = scanner.nextLine()
            update.makeGuess(line.head)
          else
            update.leave
      }

    def simpleGuess(word:String) =
      val guessed = collection.mutable.HashSet.empty[Char]
      new Client {
        override def onUpdateReceived(ctx: Context): GameCommand =
          val guessThis = word.find(e => !guessed(e))
          guessThis match {
            case Some(c) =>
              guessed += c
              ctx.makeGuess(c)
            case _ => ctx.leave
          }
    }
  }

  def defaultClient(gameId:Int, targetIp:String) = {
    new GameClient(targetIp)
  }

  trait JoinMatch
  sealed class NewGame(val gameId:Option[Int]) extends JoinMatch
  sealed class JoinExistingGame(val gameId:Int)extends JoinMatch
  class GameClient(targetIp:String) {
    lazy val socket = new Socket(targetIp, Server.serverPort)
    lazy val oos = new ObjectOutputStream(socket.getOutputStream)
    lazy val ois = new ObjectInputStream(socket.getInputStream)

    def runWith(where:JoinMatch, logic:Client) =
      val me = "Scala client"
      val cmd = where match {
        case jg:JoinExistingGame =>
          val cmd = new JoinGame
          cmd.gameID = jg.gameId
          cmd.playerId = me
          cmd

        case ng: NewGame =>
          val cmd = new GameStart
          cmd.gameID = ng.gameId.map(Integer.valueOf(_)).getOrElse(null)
          cmd.username = me
          cmd
      }
      oos.writeObject(cmd)
      oos.flush()

      while (true) {
        val in = ois.readObject().asInstanceOf[SpielZustand]
        println(s"Received from server: $in")
        val out = logic.onUpdateReceived(new Context(in, me))
        println(s"Sending to server: $out")
        oos.writeObject(out)
        oos.flush()
      }
  }
}
