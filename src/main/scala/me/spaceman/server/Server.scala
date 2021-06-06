package me.spaceman.server

import me.spaceman.common.Words
import me.spaceman.network.commands.Commands._
import me.spaceman.network.commands.GameStateMulti
import me.spaceman.server.Server.gameById

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}
import java.util.Collections
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success, Try}

object Server {
  def serverPort = 1234

  private val runningGames = ArrayBuffer.empty[OpenGame]
  private val mainExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  private def maxMoveCount = 7

  trait Reader {
    def readNextObject: GameCommand
  }

  trait Writer {
    def writeResponse(state: SpielZustand): Unit
  }

  trait TargetWord {
    def onCorrectGuess(c: Char): Unit = {}

    def onIncorrectGuess(c: Char): Unit = {}

    def isRevealed(guessed: Set[Char]) = !showString(guessed).contains("_")

    def isCorrect(char: Char): Boolean

    def showString(guessed: Set[Char]): String = {
      val word = targetWord
      word.map {
        case show if show.isWhitespace | guessed(show.toLower) => show
        case _ => '_'
      }
    }

    def targetWord: String
  }

  class SimpleWord(val target: String) extends TargetWord {
    override def targetWord: String = target

    override def isCorrect(char: Char): Boolean = target.exists(_.toLower == char.toLower)
  }

  private def display(word: String, visible: Set[Char]) =
    word.map { c =>
      if (visible(c.toLower)) c else '_'
    }.mkString

  trait EvilWordChooser {
    def mostEvilWord(pool: Traversable[String], guessesInOrder: List[Char], newGuess: Char): String
  }

  object SmartEvilWordChoooser extends EvilWordChooser {
    override def mostEvilWord(pool: Traversable[String], guessesInOrder: List[Char], newGuess: Char): String =
      val newRevealed = (mutable.HashSet.empty ++= guessesInOrder += newGuess).toSet
      val allMatches =
        pool.groupBy { candidate =>
          display(candidate, newRevealed)
        }.maxBy { case (_, options) =>
          val minimumUnguessedChars = options.minBy(_.toSet.count(e => !newRevealed(e)))
          minimumUnguessedChars
        }._2.toList
      allMatches.head
  }

  class EvilWord(length: Int, strategy: EvilWordChooser) extends TargetWord {
    private val guessesInOrder = mutable.ArrayBuffer.empty[Char]
    private val wordPool = new Random().shuffle((mutable.HashSet.empty ++= Words.loadAllWords.get(length).getOrElse {
      throw new RuntimeException(s"Invalid word length")
    }.toList))


    def debugGuess(c: Char) = {
      if (isCorrect(c)) {
        onCorrectGuess(c)
      } else {
        onIncorrectGuess(c)
      }
      val evilWord = mostEvilWord(c)
      println(s"Guessed $c, pattern ${display(evilWord, guessesInOrder.toSet)}, ${wordPool.size} left, secret: ${evilWord}")
    }

    private def mostEvilWord(newGuess: Char): String =
      strategy.mostEvilWord(wordPool, guessesInOrder.toList, newGuess)

    private var lastPattern = "".padTo(length, '_')

    override def onCorrectGuess(c: Char): Unit =
      super.onCorrectGuess(c)
      onAnyGuess(c)
      val revealed = guessesInOrder.toSet
      val newPattern = display(mostEvilWord(c), revealed)
      wordPool.filterInPlace { word =>
        Words.wordMatchesPattern(newPattern, word, revealed)
      }
      lastPattern = newPattern

    override def onIncorrectGuess(c: Char): Unit =
      super.onIncorrectGuess(c)
      onAnyGuess(c)
      wordPool.filterInPlace(!_.contains(c))

    private def onAnyGuess(c: Char) = {
      guessesInOrder += c
    }

    override def isCorrect(char: Char): Boolean = mostEvilWord(char).contains(char)

    override def targetWord: String = guessesInOrder.lastOption match {
      case Some(c) => mostEvilWord(c)
      case _ => wordPool.headOption.getOrElse(throw new RuntimeException("Wordpool empty"))
    }
  }

  class GameState(
                   private var guessesLeft: Int,
                   private val guessHistory: collection.mutable.Set[Char],
                   private val targetWord: TargetWord
                 ) {
    def isWordRevealed =
      targetWord.isRevealed(guessedAlready)

    def wordToDisplay =
      targetWord.showString(guessedAlready)

    private def guessedAlready = {
      guessHistory.toSet
    }

    def forfeit() =
      guessesLeft = 0

    def isRunning =
      guessesLeft > 0 && !isWordRevealed

    def movesLeft =
      guessesLeft

    def guess(char: Char) = {
      val low = char.toLower
      if (guessHistory(low)) {
        // noop
      } else if (targetWord.isCorrect(char)) {
        targetWord.onCorrectGuess(char)
        guessHistory += low
      } else {
        guessHistory += low
        targetWord.onIncorrectGuess(char)
        guessesLeft -= 1
      }
    }
  }

  class Connection(val in: Reader, val out: Writer, val socket: Socket)

  class Player(val name: String, val connection: Connection)

  class OpenGame(val players: ArrayBuffer[Player], val gameId: Int) {
    private var currentPlayer = Option.empty[Player]
    private val targetWord = new EvilWord(Words.randomWordLength, SmartEvilWordChoooser)
    private val state = new GameState(maxMoveCount, mutable.HashSet.empty, targetWord)
    if (targetWord.isCorrect(' ')) {
      targetWord.onCorrectGuess(' ')
    }

    def isOpen = state.isRunning

    def wordToDisplay = if (isLost) targetWord.targetWord else state.wordToDisplay

    def isWon = state.isWordRevealed

    def isLost = movesLeft == 0

    def nativeStatus =
      if (state.isRunning) {
        GameStateMulti.RUNNING
      } else {
        GameStateMulti.FINISHED
      }

    def currentPlayerId = currentPlayer.map(_.name)

    def movesLeft = state.movesLeft

    private def nextPlayer() =
      currentPlayer = currentPlayer match {
        case Some(player) if players.nonEmpty => Some(players((players.indexWhere(_.name == player.name) + 1) % players.size))
        case _ => players.headOption
      }

    def leave(userId: String) = {
      val indexOfCurrentPlayer = players.indexWhere(_.name == userId)
      if (currentPlayer.exists(_.name == userId))
        nextPlayer()
      players.remove(indexOfCurrentPlayer)
    }

    def forfeit(userId: String) = {
      if (currentPlayer.exists(_.name == userId)) {
        state.forfeit()
      } else {
        error(s"Player $userId tried to forfeit but it's ${currentPlayer.map(_.name + "'s").getOrElse("nobody's")} turn")
      }
    }

    def guess(userId: String, guess: Char) = {
      if (!currentPlayer.exists(_.name == userId)) {
        error(s"Player $userId tried to guess a character but it's ${currentPlayer.map(_.name + "'s").getOrElse("nobody's")} turn")
      } else if (!state.isRunning) {
        error(s"Player $userId tried to guess a character but the game is over")
      } else {
        state.guess(guess)
      }
    }

    def join(client: Connection, userId: String) =
      if (players.exists(_.name == userId)) {
        throw new RuntimeException(s"User $userId is already in game ${gameId}")
      }
      players += new Player(userId, client)
      if (currentPlayer.isEmpty) {
        currentPlayer = Some(players.head)
      }
  }

  private def handle(client: Connection, command: GameCommand) =
    println(s"Incoming message from ${client.socket.getInetAddress}:\n$command")
    runInOrder {
      val response = Try {
        command match {
          case obj: GameStart => start(client, obj)
          case obj: JoinGame => join(client, obj)
          case obj: Forfeit => forfeit(client, obj)
          case obj: LeaveGame => leave(client, obj)
          case obj: Guess => guess(client, obj)
        }
      } match {
        case Failure(e) => error(e.getLocalizedMessage)
        case Success(succ) => succ
      }
      runningGames
        .iterator
        .filter(_.players.exists(_.connection == client))
        .flatMap(_.players.map(_.connection))
        .foreach { con =>
          println(s"Sending response $response to ${con.socket.getInetAddress}")
          con.out.writeResponse(response)
        }
      runningGames.filterInPlace(_.isOpen)
    }


  private def runInOrder(logic: => Unit) =
    mainExecutor.execute(new Runnable {
      override def run(): Unit = logic
    })

  private def gameById(id: Int): OpenGame = runningGames.find(_.gameId == id) match {
    case Some(game) => game
    case _ => error(s"Game with ID $id not running")
  }

  private def join(client: Connection, joinGame: JoinGame) = {
    val game = gameById(joinGame.gameID)
    if (game.players.exists(_.name == joinGame.playerId)) {
      error(s"Player ${joinGame.playerId} is already in game ${joinGame.gameID}")
    } else {
      game.join(client, joinGame.playerId)
      response(game, s"Player ${joinGame.playerId} joined")
    }
  }

  private def forfeit(client: Connection, forfeit: Forfeit) = {
    val game = gameById(forfeit.gameId)
    game.forfeit(forfeit.playerId)
    response(game, s"${forfeit.playerId} gave up")
  }

  private def guess(client: Connection, guess: Guess) = {
    val game = gameById(guess.gameId)
    game.guess(guess.playerId, guess.guess)
    response(game, s"Letter ${guess.guess} guessed by ${guess.playerId}")
  }

  private def leave(client: Connection, leaveGame: LeaveGame) = {
    val game = gameById(leaveGame.gameID)
    game.leave(leaveGame.playerId)
    val ret = response(game, s"Player ${leaveGame.playerId} quit")
    ret.kickedPlayer = leaveGame.playerId
    ret.status = GameStateMulti.LEFT
    ret
  }

  private def start(client: Connection, start: GameStart) =
    if (runningGames.exists(_.gameId == start.gameID)) {
      error(s"Game ID ${start.gameID} already exists")
    } else {
      def freeGameId: Int = runningGames.iterator.map(_.gameId).maxOption.map(_ + 1).getOrElse(1)

      val gameIdOfNewGame: Int = if (start.gameID == null) freeGameId else start.gameID
      val newGame = new OpenGame(ArrayBuffer.empty, gameIdOfNewGame)
      runningGames += newGame
      newGame.join(client, start.username)
      response(newGame, "Game created")
    }

  private def response(game: OpenGame, message: String) =
    val ret = new SpielZustand
    ret.messages = message
    ret.gameID = game.gameId
    ret.currentCountdown = game.movesLeft
    ret.whoseTurn = game.currentPlayerId.getOrElse(null)
    ret.initialCountdownValue = maxMoveCount
    ret.status = game.nativeStatus
    ret.wordToGuessDisplay = game.wordToDisplay
    if (game.isWon) {
      ret.endMessage = "You won."
    } else if (game.isLost) {
      ret.endMessage = "You lost."
    }
    ret

  private def error(msg: String) =
    throw new RuntimeException(msg)

  def listenEternally(): Unit =
    val serverSocker = new ServerSocket(serverPort)
    while (true) {
      println("Waiting for next client to connect")
      val socket = serverSocker.accept()
      val reader = new Reader {
        lazy val in = new ObjectInputStream(socket.getInputStream)

        override def readNextObject: GameCommand =
          in.readObject().asInstanceOf[GameCommand]
      }
      val writer = new Writer {
        lazy val out = new ObjectOutputStream(socket.getOutputStream)

        override def writeResponse(state: SpielZustand): Unit =
          out.writeObject(state)
          out.flush()
      }
      val con = new Connection(reader, writer, socket)
      println(s"New connection from ${socket.getInetAddress}")
      implicit val ec = ExecutionContext.global
      Future {
        while (true) {
          handle(con, con.in.readNextObject)
        }
      }
    }
}
