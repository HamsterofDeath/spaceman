import me.spaceman.client.ClientFactory
import me.spaceman.client.ClientFactory.{Client, NewGame}
import me.spaceman.common.Words
import me.spaceman.server.Server
import me.spaceman.server.Server.EvilWord

import java.io._
import java.net.{ServerSocket, Socket}

@main def automaticClient: Unit =
  ClientFactory.defaultClient(1, "127.0.0.1")
  .runWith(new NewGame(None), Client.simpleGuess("apple"))

@main def manualClient: Unit =
  ClientFactory.defaultClient(1, "127.0.0.1")
  .runWith(new NewGame(None), Client.consoleClient)

@main def server: Unit =
  Server.listenEternally()

@main def test:Unit =
  val evil = new EvilWord(6)
  evil.debugGuess('a')
  evil.debugGuess('e')
  evil.debugGuess('i')
  evil.debugGuess('o')
  evil.debugGuess('u')
  evil.debugGuess('s')
  evil.debugGuess('d')
  println(evil.isCorrect('e'))
