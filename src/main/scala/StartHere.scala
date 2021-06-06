import me.spaceman.client.ClientFactory
import me.spaceman.client.ClientFactory.{Client, NewGame}
import me.spaceman.common.Words
import me.spaceman.server.Server
import me.spaceman.server.Server.{EvilWord, MaxAverageGroupSize, SmartEvilWordChoooser}

import java.io._
import java.net.{ServerSocket, Socket}

@main def simpleAutomaticClient: Unit =
  ClientFactory.defaultClient(1, "127.0.0.1")
  .runWith(new NewGame(None), Client.simpleGuess("apple"))

@main def dictionaryClient: Unit =
  ClientFactory.defaultClient(1, "127.0.0.1")
  .runWith(new NewGame(None), Client.dictionaryClient)

@main def manualClient: Unit =
  ClientFactory.defaultClient(1, "127.0.0.1")
  .runWith(new NewGame(None), Client.consoleClient)

@main def server: Unit =
  Server.listenEternally()

@main def testCase:Unit =
  val pool = Words.loadAllWords(11)
  val mine = SmartEvilWordChoooser.mostEvilWord(pool, Nil, 'a')
  val theirs = MaxAverageGroupSize.mostEvilWord(pool, Nil, 'a')
  println(s"$mine vs $theirs")