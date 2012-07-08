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

package object deepen {

  implicit val ctx = new Context {
    val name = "Never-TypeHint-Context"

    override val typeHintStrategy = NeverTypeHint

  }

  def abs(n:Double):Double = {
    if(n < 0) return n * -1.0;
    return n;
  }
  
  def main(args: Array[String]): Unit = {
    val mongoConn = MongoConnection(options = MongoOptions(autoConnectRetry = true), replicaSetSeeds =
      new com.mongodb.ServerAddress("mongo1.skeweredrook.com") :: new com.mongodb.ServerAddress("mongo2.skeweredrook.com") :: Nil);
    //val mongoConn = MongoConnection();
    val fen = new FEN;
    val positionsColl = mongoConn("mongochess")("positions");
    while (true) {
      //add a delay if none found?
      val children = positionsColl.find(MongoDBObject("parentDeepen"->true, "minMoves" -> MongoDBObject("$gt" -> 0))).sort(MongoDBObject("minMoves" -> -1)).limit(100);
      if(children.size == 0)  {
        Thread.sleep(10000)
      }
      for(childdb <- children) {
        val child = grater[Position].asObject(childdb)
        println("deepening parents of child: " + child.fen)
        // get parents
        val parents = positionsColl.find(MongoDBObject("moves.link" -> child.id))
        for(parentdb <- parents) {
          val parent = grater[Position].asObject(parentdb)
          println("deepening parent: " + parent.fen)
          //find move that matches
          def findMoveByLink(moves:Seq[Move], link:ObjectId):Int = {
            moves.zipWithIndex.foreach{ case(move, idx) => 
              if(move.link == link) {
                return idx;
              }
            }
            // this should never happen
            return -1;
          }
          var idx = findMoveByLink(parent.moves, child.id)
          val move = parent.moves(idx)
          // if parent move depth <= child depth, deepen
          if(move.depth <= child.maxDepth) {
            // copy moves/scores/bestScore from child record
            // set depth for parent move to child depth + 1
            // need to swap signs for scores from the child
            val newMoves = parent.moves.updated(idx, 
              move.copy(depth = child.maxDepth + 1,
                        score = child.bestScore * -1.0,
                        scores = parent.moves(idx).scores ++ Seq(child.moves(0).scores.last * -1.0),
                        bestMoves = Seq(child.moves(0).move) ++ child.moves(0).bestMoves))
            var minDepth = 10000;
            // find lowest depth that isn't mate (max complete search depth so far for this node)
            for(m <- newMoves) {
              if(m.depth < minDepth && abs(m.score) < 999.0) minDepth = m.depth;
            }
            var bestScore = -1000000.0;
            for(m <- newMoves) {
              if(m.score > bestScore) bestScore = m.score;
            }
            positionsColl.save(grater[Position].asDBObject(parent.copy(parentDeepen=Some(true), bestScore = bestScore, maxDepth = minDepth, moves = newMoves.sortWith(lt = (a:Move, b:Move) => {a.score > b.score} ))))
          }
        }  
        // clear child's parentDeepen flag / save child
        positionsColl.save(grater[Position].asDBObject(child.copy(parentDeepen = None)))
      }
    }

  }
}
