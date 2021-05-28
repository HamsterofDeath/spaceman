package me.spaceman.network.commands

object Commands {

  @SerialVersionUID(0L)
  class GameStart extends Serializable {
    var username: String = null
    var gameID: Integer = null

    override def toString = s"GameStart(username=$username, gameID=$gameID)"
  }

  @SerialVersionUID(0L)
  class Forfeit extends Serializable {
    var gameId = 0
    var playerId: String = null

    override def toString = s"Forfeit(gameId=$gameId, playerId=$playerId)"
  }

  @SerialVersionUID(0L)
  class LeaveGame extends Serializable {
    var playerId: String = null
    var gameID = 0

    override def toString = s"LeaveGame(playerId=$playerId, gameID=$gameID)"
  }

  @SerialVersionUID(0L)
  class JoinGame extends Serializable {
    var playerId: String = null
    var gameID = 0

    override def toString = s"JoinGame(playerId=$playerId, gameID=$gameID)"
  }

  @SerialVersionUID(0L)
  class Guess extends Serializable {
    var guess = ' '
    var gameId = 0
    var playerId: String = null

    override def toString = s"Guess(guess=$guess, gameId=$gameId, playerId=$playerId)"
  }

  @SerialVersionUID(0L)
  class SpielZustand extends Serializable {
    var gameID = 0
    var whoseTurn: String = null
    var messages = ""
    var errorMessages = ""
    var status: GameStateMulti = null
    var wordToGuessDisplay: String = null
    var initialCountdownValue = 0
    var currentCountdown = 0
    var endMessage: String = null
    var kickedPlayer: String = null

    override def toString = s"SpielZustand(gameID=$gameID, whoseTurn=$whoseTurn, messages=$messages, errorMessages=$errorMessages, status=$status, wordToGuessDisplay=$wordToGuessDisplay, initialCountdownValue=$initialCountdownValue, currentCountdown=$currentCountdown, endMessage=$endMessage, kickedPlayer=$kickedPlayer)"
  }

  type GameCommand = GameStart | JoinGame | Forfeit | LeaveGame | Guess


}
