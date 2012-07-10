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
      val children = positionsColl.find(MongoDBObject("parentDeepen"->true, "minMoves" -> MongoDBObject("$gt" -> 0))).sort(MongoDBObject("minMoves" -> -1)).limit(100);
      if(children.size == 0)  {
        Thread.sleep(1000)
      }
      for(childdb <- children) {
        val child = grater[Position].asObject(childdb)
        println("deepening parents of child: " + child.fen + "; " + child.id)
        // get parents
        val parents = positionsColl.find(MongoDBObject("moves.link" -> child.id))
        for(parentdb <- parents) {
          val parent = grater[Position].asObject(parentdb)
          println("deepening parent: " + parent.fen + " ; " + parent.id)
          //find move that matches
          def findMoveByLink(moves:Seq[Move], link:ObjectId):Int = {
            moves.zipWithIndex.foreach{ case(move, idx) => 
              if(move.link == link) {
                return idx;
              }
            }
            // this should never happen
            println("this should never happen: -1")
            return -1;
          }
          var idx = findMoveByLink(parent.moves, child.id)
          val move = parent.moves(idx)
          // if parent move depth <= child depth, deepen
          if(move.depth <= child.maxDepth || abs(child.bestScore) > 999.0 || child.forcedDraw.getOrElse(false)) {
            // copy moves/scores/bestScore from child record
            // set depth for parent move to child depth + 1
            // need to swap signs for scores from the child
            val newMoves = if(child.forcedDraw.getOrElse(false)) {
              parent.moves.updated(idx, 
              move.copy(depth = parent.moves(idx).depth + 1,
                        score =  0,
                        forcedDraw = child.forcedDraw))
            } else {
              parent.moves.updated(idx, 
              move.copy(depth = child.maxDepth + 1,
                        score = if(abs(child.bestScore) < 999.0) child.bestScore * -1.0 else child.bestScore * -1.0 + 1.0 * child.bestScore / abs(child.bestScore),
                        scores = parent.moves(idx).scores ++ child.moves(0).scores.slice(parent.moves(idx).depth - 1, child.moves(0).scores.size).map{ s => s * -1.0},
                        bestMoves = Seq(child.moves(0).move) ++ child.moves(0).bestMoves,
                        endFen = child.moves(0).endFen,
                        forcedDraw = child.forcedDraw))
            }
            var minDepth = 10000;
            // find lowest depth that isn't mate (max complete search depth so far for this node)
            for(m <- newMoves) {
              if(m.depth < minDepth && abs(m.score) < 999.0) minDepth = m.depth;
            }
            // if we came back with a mate or draw, let's try another option
            if(newMoves(idx).score < -999.0 || newMoves(idx).forcedDraw.getOrElse(true)) {
              for(m <- parent.moves)  {
                if(m.score > -999.0) {
                  println("setting priority for : " + m.link)
                  positionsColl.update(MongoDBObject("_id" -> m.link), MongoDBObject("$set" -> MongoDBObject("priority" -> true)))
                }
              }
            }
            var bestScore = -1000000.0;
            var forcedDraw:Option[Boolean] = Some(true)
            newMoves.map ( m => if(m.forcedDraw == None && (m.score > -999.0 && m.score != 0.0)) {forcedDraw = None;})
            if(bestScore == 0.0 && forcedDraw.getOrElse(false)) forcedDraw = Some(true)
            for(m <- newMoves) {
              if(m.score > bestScore) bestScore = m.score;
            }
            val possiblePrune:Option[Boolean] = if(abs(bestScore) > 999.0 || parent.forcedDraw.getOrElse(false)) Some(true) else None
            positionsColl.save(grater[Position].asDBObject(parent.copy(forcedDraw = forcedDraw, possiblePrune=possiblePrune, parentDeepen=Some(true), bestScore = bestScore, maxDepth = minDepth, moves = newMoves.sortWith(lt = (a:Move, b:Move) => {a.score > b.score} ))))
          }
        }  
        // clear child's parentDeepen flag / save child
        positionsColl.save(grater[Position].asDBObject(child.copy(parentDeepen = None)))
      }
    }

  }
}
