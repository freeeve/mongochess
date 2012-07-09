package wes.mongochess

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
    val mongoConn = MongoConnection(options = MongoOptions(autoConnectRetry = true), replicaSetSeeds =
      new com.mongodb.ServerAddress("mongo1.skeweredrook.com") :: new com.mongodb.ServerAddress("mongo2.skeweredrook.com") :: Nil);
    //val mongoConn = MongoConnection();
    val fen = new FEN;
    val positionsColl = mongoConn("mongochess")("positions");
    while (true) {
      val toPrune = positionsColl.find(MongoDBObject("possiblePrune" -> true,"parentDeepen"->MongoDBObject("$exists" -> false), "minMoves" -> MongoDBObject("$gt" -> 0))).sort(MongoDBObject("minMoves" -> -1)).limit(1);
      for (prunedb <- toPrune) {
        val prune = grater[Position].asObject(prunedb)
        println("pruning: " + prune.fen)
        // clear links
        if (prune.moves != null) {
          val movesWithoutLinks: Seq[Move] = prune.moves.map { move =>
            move.copy(link = null)
          }
          if (prune.moves.size == 0) {
            positionsColl.remove(MongoDBObject("_id" -> prune.id))
          } else {
            for (move <- prune.moves if move.link != null) {
              println("setting to remove: " + move.link)
              positionsColl.update(MongoDBObject("_id" -> move.link), MongoDBObject("$set" -> MongoDBObject("prune" -> true)))
            }

            val pruneCopy = prune.copy(possiblePrune = None, moves = movesWithoutLinks)
            positionsColl.save(grater[Position].asDBObject(pruneCopy))
          }
        } else {
          val pruneCopy = prune.copy(possiblePrune = None)
          positionsColl.save(grater[Position].asDBObject(pruneCopy))
        }

      }
      val toRemove = positionsColl.find(MongoDBObject("prune" -> true, "minMoves" -> MongoDBObject("$gt" -> 0))).sort(MongoDBObject("minMoves" -> -1)).limit(10);
      for (prunedb <- toRemove) {
        val prune = grater[Position].asObject(prunedb)
        println("attempting to remove: " + prune.fen)
        val parents = positionsColl.find(MongoDBObject("moves.link" -> prune.id))
        var canRemove = true
        for(parentdb <- parents) {
          val parent = grater[Position].asObject(parentdb)
          if(parent.possiblePrune.getOrElse(true) && !parent.prune.getOrElse(false)) canRemove = false
        }
        if(canRemove) {
          
        if (prune.moves != null) {
          for (move <- prune.moves if move.link != null) {
            println("setting to remove: " + move.link)
            positionsColl.update(MongoDBObject("_id" -> move.link), MongoDBObject("$set" -> MongoDBObject("prune" -> true)))
          }
        }
        positionsColl.remove(MongoDBObject("_id" -> prune.id))
        } else {
          val pruneCopy = prune.copy(prune = None)
          positionsColl.save(grater[Position].asDBObject(pruneCopy))
        }
      }
      if (toPrune.size == 0 && toRemove.size == 0) {
        Thread.sleep(10000)
      }
    }

  }
}
