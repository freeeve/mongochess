/*package wes.mongochess

import com.novus.salat._
import com.novus.salat.annotations._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import scala.collection.mutable.HashSet
import java.util.Date
import ictk.boardgame.chess._
import ictk.boardgame.chess.io._
import ictk.boardgame.chess.ui.cli.TxChessBoardDisplay
import wes.mongochess.model._

package object prune {

  implicit val ctx = new Context {
    val name = "Never-TypeHint-Context"

    override val typeHintStrategy = NeverTypeHint

  }

  def abs(n: Double): Double = {
    if (n < 0) return n * -1.0;
    return n;
  }

  def main(args: Array[String]): Unit = {
    //val mongoConn = MongoConnection(options = MongoOptions(autoConnectRetry = true), replicaSetSeeds =
    //new com.mongodb.ServerAddress("mongo1.skeweredrook.com") :: new com.mongodb.ServerAddress("mongo2.skeweredrook.com") :: Nil);
    val mongoConn = MongoConnection();
    val fen = new FEN;
    val positionsColl = mongoConn("mongochess")("positions");
    while (true) {
      val toPrune = positionsColl.findAndModify(
        MongoDBObject("possiblePrune" -> true, "claimed" -> MongoDBObject("$exists" -> false), "parentDeepen" -> MongoDBObject("$exists" -> false), "minMoves" -> MongoDBObject("$gt" -> 0)),
        MongoDBObject("minMoves" -> -1),
        MongoDBObject("$set" -> MongoDBObject("claimed" -> true)))
      if (toPrune != None)
        for (prunedb <- toPrune) {
          val prune = grater[Position].asObject(prunedb)
          println("pruning: " + prune.fen)
          // clear links 
          if (prune.moves != null) {
            if (prune.moves.size == 0) {
              positionsColl.remove(MongoDBObject("_id" -> prune.id))
            } else {
              for (move <- prune.moves if move.fen != null) {
                println("setting to remove: " + move.fen)
                positionsColl.update(MongoDBObject("fen" -> move.fen), MongoDBObject("$set" -> MongoDBObject("prune" -> true)))
              }
              val pruneCopy = prune.copy(possiblePrune = None)
              positionsColl.save(grater[Position].asDBObject(pruneCopy))
            }
          } else {
            val pruneCopy = prune.copy(possiblePrune = None)
            positionsColl.save(grater[Position].asDBObject(pruneCopy))
          }

        }
      val toRemove = positionsColl.findAndModify(
        MongoDBObject("prune" -> true, "claimed" -> MongoDBObject("$exists" -> false), "parentDeepen" -> MongoDBObject("$exists" -> false), "minMoves" -> MongoDBObject("$gt" -> 0)),
        MongoDBObject("minMoves" -> 1),
        MongoDBObject("$set" -> MongoDBObject("claimed" -> true)))
      if (toRemove != None)
        for (prunedb <- toRemove) {
          val prune = grater[Position].asObject(prunedb)
          println("attempting to remove: " + prune.fen)
          val parents = positionsColl.find(MongoDBObject("moves.fen" -> prune.fen))
          var canRemove = true
          if (parents.size == 1) {
            for (parentdb <- parents) {
              val parent = grater[Position].asObject(parentdb)
              def findMoveByFen(moves: Seq[Move], fenPos: String): Int = {
                moves.zipWithIndex.foreach {
                  case (move, idx) =>
                    if (move.fen == fenPos) {
                      return idx;
                    }
                }
                // this should never happen
                println("this should never happen: -1")
                return -1;
              }
              var idx = findMoveByFen(parent.moves, prune.fen)
              if (parent.moves(idx).score != prune.bestScore) {
                canRemove = false
                println("parent not updated yet, deepening first: " + parent.fen)
                positionsColl.save(grater[Position].asDBObject(prune.copy(parentDeepen = Some(true))))
              }
            }
          } else if (parents.size > 1) {
            for (parentdb <- parents) {
              val parent = grater[Position].asObject(parentdb)
              if (!parent.possiblePrune.getOrElse(false) && !parent.prune.getOrElse(false)) {
                canRemove = false
                println("parent blocking: " + prune.fen + "; " + parent.fen)
              }
            }
          }

          if (canRemove) {

            if (prune.moves != null) {
              for (move <- prune.moves if move.fen != null) {
                //println("setting to remove: " + move.fen)
                positionsColl.update(MongoDBObject("fen" -> move.fen), MongoDBObject("$set" -> MongoDBObject("prune" -> true)))
              }
            }
            println("removing (" + prune.id + "): " + prune.fen)
            positionsColl.remove(MongoDBObject("fen" -> prune.fen))

          } else {
            println("couldn't remove: " + prune.fen);
            positionsColl.update(MongoDBObject("fen" -> prune.fen), MongoDBObject("$unset" -> MongoDBObject("prune" -> true)))
          }

        }
      if (toPrune == None && toRemove == None) {
        Thread.sleep(1000)
      }
    }

  }
}*/
