package wes.mongochess

import com.novus.salat._
import com.novus.salat.annotations._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import scala.math.abs
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import java.util.Date
import ictk.boardgame.chess._
import ictk.boardgame.chess.io._
import ictk.boardgame.chess.ui.cli.TxChessBoardDisplay
import wes.mongochess.model._
import scala.collection.mutable.Map

package object analyze {
  implicit val ctx = new Context {
    val name = "Never-TypeHint-Context"
    override val typeHintStrategy = NeverTypeHint
  }

  val san = new SAN
  val fen = new FEN
  val stockfish = new Stockfish(4096, 100)

  val mongoConn = MongoConnection(options = MongoOptions(autoConnectRetry = true), replicaSetSeeds =
  new com.mongodb.ServerAddress("mongo1.skeweredrook.com") :: new com.mongodb.ServerAddress("mongo2.skeweredrook.com") :: Nil);
  //val mongoConn = MongoConnection()
  val positionsColl = mongoConn("mongochess")("positions");

  var bestFen: String = null;
  var lastBestFen: String = null;
  var maxDepth = 10;
  val maxThreshhold = 999.0;

  def main(args: Array[String]): Unit = {
    stockfish.start()
    while (true) {
      // get next position to analyze
      val unanalyzed = getNextPosition()
      if (unanalyzed.size > 0) {
        for (positionDB <- unanalyzed) {
          val startTime = System.currentTimeMillis
          var position = grater[Position].asObject(positionDB);
          if (position.maxDepth == maxDepth) {
            positionsColl.update(MongoDBObject("_id" -> position.id), MongoDBObject("$set" -> MongoDBObject("parentDeepen" -> true)))
            println("already analyzed " + position.fen + "; setting to deepen")
            bestFen = null;
          } else if (position.forcedDraw.getOrElse(false)) {
            bestFen = null;
          } else {
            maxDepth = if(abs(position.bestScore) > 20) 20 
                       else 10
            println("analyzing to depth " + maxDepth + " cur best: " + position.bestScore + ": " + position.fen);
            position = readUntilBestMove(position)
            if (position != null) {
              val time = System.currentTimeMillis - startTime
              if (time > 10000) {
                new TxChessBoardDisplay(fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]).print
              }
              println("analyzed in " + time + "ms; best score: " + position.bestScore + ".");
              println("bestFen: " + bestFen)
              deepenParent(position)
              if (abs(position.bestScore) > 999.0 || position.forcedDraw.getOrElse(false)) {
                println("position is solved!")
                prunePosition(position)
              } else {
                savePosition(position)
                createChildren(position)
              }
            }
          }
        }
      }
    }
    stockfish.stop();
  }

  // this isn't quite right. it's not getting the mates and -mates moved up sometimes
  // need to troubleshoot why
  // draws seem to work
  def deepenParent(child: Position): Unit = {
    println("deepening from child: " + child.fen + " ; " + child.id + "; bestScore: "+child.bestScore)
    val parents = positionsColl.find(MongoDBObject("moves.fen" -> child.fen))
    for (parentdb <- parents) {
      val parent = grater[Position].asObject(parentdb)
      println("deepening parent: " + parent.fen + " ; " + parent.id + "; bestScore: "+parent.bestScore)
      val startingBestScore = parent.bestScore
      val startingForcedDraw = parent.forcedDraw
      //find move that matches
      var idx = findMoveIdxByFen(parent, child.fen)
      val move = parent.moves(idx)
      println("replacing move score: "+move.score + " with child score: "+getChildScoreParentPerspective(child.bestScore))
      // copy moves/scores/bestScore from child record
      // set depth for parent move to child depth + 1
      // need to swap signs for scores from the child
      // maybe the child forced draw is being set weird...
      val newMoves = 
        parent.moves.updated(idx,
          move.copy(depth = child.maxDepth + 1,
            score = getChildScoreParentPerspective(child.bestScore),
            scores = Seq(parent.moves(idx).scores(0)) ++ child.moves(0).scores.map { s => s * -1.0 },
            bestMoves = Seq(child.moves(0).move) ++ child.moves(0).bestMoves,
            endFen = child.moves(0).endFen,
            forcedDraw = child.forcedDraw))
      var newParent:Position = parent.copy(moves = newMoves)
      // find lowest depth that isn't mate (max complete search depth so far for this node)
      // get best score
      // get forcedDraw
      var minDepth = 10000;
      var bestScore = getBestScore(newParent)
      var forcedDraw: Option[Boolean] = Some(true)
      newMoves.map(m => {
        if (!m.forcedDraw.getOrElse(false) && m.score > -999.0) { forcedDraw = None; }
        if (m.depth < minDepth && abs(m.score) < 999.0) minDepth = m.depth;
      })
      if (bestScore != 0.0) forcedDraw = None

      // if we came back with a mate or draw, let's try another option
      if (!forcedDraw.getOrElse(false) && (child.bestScore > 999.0 || child.forcedDraw.getOrElse(false))) {
        var nextBest = -1000002.0
        var nextFen: String = null
        for (m <- parent.moves if m.fen != child.fen) {
          if (m.score > -999.0 && !m.forcedDraw.getOrElse(false)) {
            if (nextBest < m.score) {
              nextBest = m.score
              nextFen = m.fen
            }
          }
        }
        if (nextFen != null) {
          println("setting priority for : " + nextFen)
          positionsColl.update(MongoDBObject("fen" -> nextFen), MongoDBObject("$set" -> MongoDBObject("priority" -> true)))
        }
      }
      println("deepened parent: " + parent.fen + " ; " + parent.id + "; new bestScore: "+bestScore)
      newParent = newParent.copy(forcedDraw = forcedDraw, bestScore = bestScore, maxDepth = minDepth, moves = newMoves.sortWith(lt = (a: Move, b: Move) => { a.score > b.score }))
      savePosition(newParent)
      if (startingBestScore != bestScore || startingForcedDraw.getOrElse(false) != forcedDraw.getOrElse(false)) {
        deepenParent(newParent)
      }

      if (abs(bestScore) > 999.0 || forcedDraw.getOrElse(false)) {
        deepenParent(newParent)
        prunePosition(newParent)
      }
    }
  }
  
  def getBestScore(pos:Position):Double = {
    var bestScore = -100000.0;
    //println("calculating bestScore for: "+pos.fen)
    for(move <- pos.moves) {
      //println("comparing cur best: " + bestScore + " to " + move.score)
      if(move.score > bestScore) bestScore = move.score
    }
    bestScore
  }
  
  def getChildScoreParentPerspective(childScore:Double) = {            
    // some funky math to negate the score (non-mate and mate)
    if (abs(childScore) < 999.0) childScore * -1.0 else childScore * -1.0 + 1.0 * childScore / abs(childScore)
  }

  def prunePosition(prune: Position): Unit = {
    println("pruning: (" + prune.id + ")" + prune.fen)
    if (prune.moves != null && prune.moves.size != 0) {
      for (move <- prune.moves if move.fen != null) {
        pruneFEN(move.fen)
      }
    }
    positionsColl.remove(MongoDBObject("_id" -> prune.id))
  }

  def pruneFEN(fenStr: String) = {
    val posget = positionsColl.findOne(MongoDBObject("fen" -> fenStr))
    if (posget != None) {
      val pos: Position = grater[Position].asObject(posget.get)
      prunePosition(pos)
    }
  }

  def findMoveIdxByFen(position: Position, fenPos: String): Int = {
    position.moves.zipWithIndex.foreach {
      case (move, idx) =>
        if (move.fen == fenPos) {
          return idx;
        }
    }
    return -1;
  }

  def sortAndUpdatePosition(pos: Position): Position = {
    var position = pos.copy()
    bestFen = null;
    var bestScore = -1000000.0;
    var posForcedDraw: Option[Boolean] = Some(true);
    // add fen, endfen, sort by score, and calculate scoreDepth
    position.moves.zipWithIndex.foreach {
      case (move, idx) => {
        val board = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]
        board.playMove(san.stringToMove(board, move.move))
        var forcedDraw = isDrawn(board)
        val startFen = fen.boardToString(board)
        for (m <- move.bestMoves) {
          val chessMove = san.stringToMove(board, m)
          board.playMove(chessMove);
        }

        var stored = false
        val childopt = positionsColl.findOne(MongoDBObject("fen" -> startFen))
        if (childopt != None) {
          val child = grater[Position].asObject(childopt.get)
          if (!forcedDraw.getOrElse(false)) forcedDraw = child.forcedDraw
          stored = true
        }

        //println("best score < move.score: " + bestScore + "; "+ move.score)
        if (bestScore < move.score) {
          bestScore = move.score
          bestFen = startFen
        }

        position = position.copy(
          moves = position.moves.updated(idx,
            move.copy(fen = startFen,
              endFen = fen.boardToString(board),
              forcedDraw = forcedDraw,
              stored = stored)))

        if (forcedDraw == None && move.score > -999.0) { posForcedDraw = None; }
      }
    }

    position = position.copy(bestScore = bestScore,
      forcedDraw = posForcedDraw,
      moves = position.moves.sortWith(lt = (a: Move, b: Move) => { a.score > b.score }))

    return position
  }

  def createChildren(position: Position): Unit = {
    if (abs(position.bestScore) < 999.0 && !position.forcedDraw.getOrElse(false)) {
      for (move <- position.moves) {
        if (!move.stored && abs(move.score) < 999.0 && !move.forcedDraw.getOrElse(false)) {
          val shortenedFen = getPositionFEN(move.fen)
          println("creating new position for " + move.move + ": " + move.fen + "; score: " + move.score)
          savePosition(Position(
            fen = move.fen,
            pos = shortenedFen,
            minMoves = position.minMoves + 1,
            bestScore = move.score * -1.0,
            maxDepth = 0))
        }
      }
    }
  }

  def getPositionFEN(fen: String) = {
    val fenSplit = fen.split(" ");
    var res = "";
    for (i <- 0 to 3) {
      res += fenSplit(i) + " ";
    }
    res.substring(0, res.length - 1);
  }

  def savePosition(position: Position) {
    positionsColl.save(grater[Position].asDBObject(position))
  }

  def isDrawn(board: ChessBoard):Option[Boolean] = {
    if (board.getUnCapturedPieces(false).size + board.getUnCapturedPieces(true).size > 2 && !board.isStalemate() && !board.is50MoveRuleApplicible) {
      None
    } else if (board.getUnCapturedPieces(false).size + board.getUnCapturedPieces(true).size == 3) {
      for(piece <- board.getUnCapturedPieces(board.getUnCapturedPieces(true).size == 2)) {
        if(piece.isBishop() || piece.isKnight()) 
          return Some(true)
      }
      None
    } else {
      Some(true)
    }
  }

  def getNextPosition() = {
    if (bestFen == null || bestFen == lastBestFen) {
      //positionsColl.findAndModify(MongoDBObject("maxDepth" -> MongoDBObject("$lt" -> maxDepth), "forcedDraw" -> MongoDBObject("$exists" -> false), "claimed" -> MongoDBObject("$exists" -> false)),
      positionsColl.findAndModify(MongoDBObject("maxDepth" -> 0, "forcedDraw" -> MongoDBObject("$exists" -> false), "claimed" -> MongoDBObject("$exists" -> false)),
        MongoDBObject("priority" -> -1, "minMoves" -> -1, "bestScore" -> 1),
        MongoDBObject("$set" -> MongoDBObject("claimed" -> true, "ts" -> new Date)))
    } else {
      lastBestFen = bestFen;
      positionsColl.findAndModify(MongoDBObject("fen" -> bestFen, "forcedDraw" -> MongoDBObject("$exists" -> false), "claimed" -> MongoDBObject("$exists" -> false)),
        MongoDBObject("$set" -> MongoDBObject("claimed" -> true, "ts" -> new Date)));
    }
  }

  def readUntilBestMove(pos: Position): Position = {
    var position = pos.copy()
    var lastDepth = 0;
    stockfish.setFEN(pos.fen)
    stockfish.goToDepth(maxDepth)
    while (true) {
      val line = stockfish.readLine
      if (line.startsWith("info") && line.contains("seldepth") && !line.contains("bound")) {
        val split = line.split(" ");
        val depth = split(2).toInt;
        if (depth > lastDepth) {
          lastDepth = depth; print("D" + depth + " ");
        }
        if (depth >= lastDepth) {
          val score =
            if (split(6) == "mate") {
              10000.0 * split(7).toInt / abs(split(7).toInt) - split(7).toInt;
            } else {
              split(7).toInt / 100.0;
            }
          val board: ChessBoard = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]
          val chessMove = algToCM(board, split(17));
          val moveStr = san.moveToString(chessMove);
          board.playMove(chessMove);
          val moveList = getMoveList(split(17), split.slice(18, split.size), position.fen)

          if (position.moves == null) {
            position = position.copy(moves = Seq[Move]())
          }
          position = updatePositionMoves(position, moveStr, moveList, score, depth)
        }
      } else if (line.startsWith("bestmove")) {
        return sortAndUpdatePosition(position);
      }
    }
    return sortAndUpdatePosition(position);
  }

  /* gets an array of SAN move strings from a list of algebraic coord strings */
  def getMoveList(move: String, moves: Array[String], fenStr: String) = {
    val b = fen.stringToBoard(fenStr).asInstanceOf[ChessBoard]
    b.playMove(algToCM(b, move));
    for (m <- moves) yield {
      val cm = algToCM(b, m);
      b.playMove(cm)
      san.moveToString(cm);
    }
  }

  /* returns an updated copy of the position */
  def updatePositionMoves(position: Position, moveStr: String, bestMoves: Seq[String],
    score: Double, depth: Int): Position = {
    val idx = findMoveIdxByStr(position, moveStr)
    if (idx == -1) {
      position.copy(moves = position.moves :+
        Move(move = moveStr, bestMoves = bestMoves, score = score, depth = depth, scores = Seq(score)))
    } else {
      val scores = if (position.moves(idx).scores.length >= depth) {
        position.moves(idx).scores.updated(depth - 1, score)
      } else {
        position.moves(idx).scores :+ score
      }
      position.copy(
        maxDepth = depth,
        moves = position.moves.updated(
          idx,
          position.moves(idx).copy(
            bestMoves = bestMoves,
            score = score,
            depth = depth,
            scores = scores)))
    }
  }

  /* find the index of the move in a position by the move string */
  def findMoveIdxByStr(pos: Position, moveStr: String): Int = {
    pos.moves.zipWithIndex.foreach {
      case (move, i) =>
        if (move.move == moveStr) return i;
    }
    return -1;
  }

  /* convert an algebraic coordinate move to a ChessMove */
  def algToCM(b: ChessBoard, moveStr: String): ChessMove = {
    if (moveStr.length == 4) {
      new ChessMove(b,
        san.fileToNum(moveStr(0)),
        san.rankToNum(moveStr(1)),
        san.fileToNum(moveStr(2)),
        san.rankToNum(moveStr(3)));
    } else {
      // handle promotions
      val pieceIdx = moveStr.substring(4, 5) match {
        case "q" => Queen.INDEX;
        case "n" => Knight.INDEX;
        case "b" => Bishop.INDEX;
        case "r" => Rook.INDEX;
      }
      new ChessMove(b,
        san.fileToNum(moveStr(0)),
        san.rankToNum(moveStr(1)),
        san.fileToNum(moveStr(2)),
        san.rankToNum(moveStr(3)),
        pieceIdx);
    }
  }

}
