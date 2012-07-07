package wes.mongochess

import com.novus.salat._
import com.novus.salat.annotations._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import scala.collection.mutable.HashMap
import java.util.Date
import ictk.boardgame.chess._
import ictk.boardgame.chess.io._
import ictk.boardgame.chess.ui.cli.TxChessBoardDisplay
import wes.mongochess.model._
import scala.collection.mutable.Map

package object never_typehint_context {

  implicit val ctx = new Context {
    val name = "Never-TypeHint-Context"

    override val typeHintStrategy = NeverTypeHint

  }

  class Exe(command: String, out: String => Unit, err: String => Unit) {

    import scala.sys.process._
    import scala.io._
    import java.io._
    import scala.concurrent._

    val inputStream = new SyncVar[OutputStream];
    val outputStream = new SyncVar[BufferedReader];

    val process = Process(command).run(
      new ProcessIO(
        stdin => inputStream.put(stdin),
        stdout => outputStream.put(new BufferedReader(new InputStreamReader(new BufferedInputStream(stdout)))),
        stderr => Source.fromInputStream(stderr).getLines.foreach(err)));

    def write(s: String): Unit = synchronized {
      inputStream.get.write((s + "\n").getBytes)
      inputStream.get.flush
    }

    def readLine(): String = {
      outputStream.get.readLine
    }

    def close(): Unit = {
      inputStream.get.close
      outputStream.get.close
    }
  }

  def main(args: Array[String]): Unit = {
    val san = new SAN

    val exe = new Exe("stockfish", out => println("o: " + out), err => println("e: " + err))
    exe.write("setoption name Hash value 1024");
    exe.write("setoption name MultiPV value 100");
    val mongoConn = MongoConnection(options = MongoOptions(autoConnectRetry = true), replicaSetSeeds =
      new com.mongodb.ServerAddress("mongo1.skeweredrook.com") :: new com.mongodb.ServerAddress("mongo2.skeweredrook.com") :: Nil);
    val fen = new FEN;
    var maxDepth = 15;
    val maxThreshhold = 999.0;
    var bestLink: ObjectId = null;
    var lastBestLink: ObjectId = null;
    val positionsColl = mongoConn("mongochess")("positions");
    while (true) {
      val unanalyzed = if (bestLink == null || bestLink == lastBestLink) {
        // this should probably start at the base position each time, until all of the base moves are explored... then
        // move to the next level and treat it like base moves. need a flag on the moves to show analyzed?
        positionsColl.find("maxDepth" $lt maxDepth).sort(MongoDBObject("minMoves" -> 1, "bestScore" -> 1)).limit(10);
        //positionsColl.find(MongoDBObject("_id" -> new ObjectId("4fd9548e03649b52043e42a3")));
      } else {
        lastBestLink = bestLink;
        //positionsColl.find(MongoDBObject("_id" -> bestLink));
        positionsColl.find("maxDepth" $lt maxDepth).sort(MongoDBObject("minMoves" -> 1, "bestScore" -> 1)).limit(10);
      }
      //val unanalyzed = positionsColl.find("maxDepth" $lt maxDepth).sort(MongoDBObject("bestScore" -> 1)).limit(10);
      if (unanalyzed.size == 0) maxDepth += 1;

      for (positionDB <- unanalyzed) {
        var position = grater[Position].asObject(positionDB);
        exe.write("position fen " + position.fen);
        exe.write("go depth " + maxDepth);
        val board = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]

        println("analyzing to depth " + maxDepth + " cur best: " + position.bestScore + ": " + position.fen);
        new TxChessBoardDisplay(board).print

        if (readUntilBestMove()) {
          println("analyzed in " + "xxx" + " seconds; best score: " + position.bestScore + ". \n");
          
          // add endfen, sort by score, and calculate scoreDepth
          position.moves.zipWithIndex.foreach { case(move, idx) =>
            val board = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]
            board.playMove(san.stringToMove(board, move.move))
            for (m <- move.bestMoves) {
              val chessMove = san.stringToMove(board, m)
              board.playMove(chessMove);
            }
            position = position.copy(
               moves = position.moves.updated(idx, 
                  move.copy(endFen = fen.boardToString(board))))
          }
          position = position.copy(moves = position.moves.sortWith(lt = (a:Move, b:Move) => {a.score > b.score} ))

          positionsColl.save(grater[Position].asDBObject(position))
        }

        def readUntilBestMove(): Boolean = {
          val createHash = new HashMap[String, (ObjectId, Double)]

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

          var lastDepth = 0;
          var bestScoreThisDepth = -500.0;
          while (true) {
            val line = exe.readLine
            if (line.startsWith("info") && line.contains("seldepth") && !line.contains("bound")) {
              val split = line.split(" ");
              val depth = split(2).toInt;
              if (depth > lastDepth) {
                lastDepth = depth; print("D" + depth + " ");
                bestScoreThisDepth = -500000.0;
              }
              if (depth >= lastDepth) {
                val score =
                  if (split(6) == "mate") {
                    1000.0 * split(7).toInt;
                  } else {
                    split(7).toInt / 100.0;
                  }
                val mate = if (split(6) == "mate") {
                  Some(split(7).toInt);
                } else {
                  None;
                }
                if (score > bestScoreThisDepth) {
                  bestScoreThisDepth = score;
                }
                val time = split(13).toLong / 1000;
                val board: ChessBoard = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]
                val chessMove = algToCM(board, split(17));
                val moveStr = san.moveToString(chessMove);
                board.playMove(chessMove);
                val fenAfterMove = fen.boardToString(board);
                val moveList = {
                  var ms = split.slice(18, split.size);
                  val b = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]
                  b.playMove(algToCM(b, split(17)));
                  for (m <- ms) yield {
                    val cm = algToCM(b, m);
                    b.playMove(cm)
                    san.moveToString(cm);
                  }
                }

                var link: ObjectId = null;
                if (depth == maxDepth && (score > -maxThreshhold && score < maxThreshhold) && position.minMoves < 200) {
                  // make sure it doesn't exist first (and check the hash)
                  val fenSplit = fenAfterMove.split(" ");
                  val shortenedFen = {
                    var res = "";
                    for (i <- 0 to 3) {
                      res += fenSplit(i) + " ";
                    }
                    res.substring(0, res.length - 1);
                  }
                  if (!createHash.contains(fenAfterMove)) {
                    println("checking whether fen exists: " + shortenedFen)
                    //shortenedFen.replaceAll("/", "\\/");
                    val temp = fen.stringToBoard(fenAfterMove).asInstanceOf[ChessBoard];
                    if (temp.getUnCapturedPieces(false).size + temp.getUnCapturedPieces(true).size > 6 && !temp.is50MoveRuleApplicible) {
                      if (positionsColl.count(MongoDBObject("pos" -> shortenedFen)) == 0) {
                        println("creating new position: ");
                        new TxChessBoardDisplay(fen.stringToBoard(fenAfterMove).asInstanceOf[ChessBoard]).print;
                        val newPos = Position(fen = fenAfterMove, pos = shortenedFen, minMoves = position.minMoves + 1, bestScore = -score);
                        positionsColl.save(grater[Position].asDBObject(newPos));
                        link = newPos.id
                        if (-newPos.bestScore == bestScoreThisDepth) {
                          bestLink = link
                        }
                      } else {
                        val p = grater[Position].asObject(positionsColl.findOne(MongoDBObject("pos" -> shortenedFen)).get);
                        link = p.id
                        if (-p.bestScore == bestScoreThisDepth) {
                          bestLink = link
                        }
                      }
                    }
                    
                    createHash += ((fenAfterMove, (link, score)));

                  } else {
                    var bestScore = 0.0;
                    link = createHash.getOrElse(fenAfterMove, (null, 0.0))._1;
                    bestScore = createHash.getOrElse(fenAfterMove, (null, 0.0))._2;
                    if(link == null) {
                      val p = grater[Position].asObject(positionsColl.findOne(MongoDBObject("pos" -> shortenedFen)).get);
                      link = p.id
                      bestScore = -p.bestScore;
                    }
                    if (bestScore == bestScoreThisDepth) {
                      bestLink = link
                    }
                  }

                }
                if (position.moves == null) {
                  position = position.copy(moves = Seq[Move]())
                }

                def findMove(moves:Seq[Move], moveStr:String):Int = {
                  moves.zipWithIndex.foreach { case (move, i) =>
                    if(move.move == moveStr) return i;
                  }
                  return -1;
                }
                val idx = findMove(position.moves, moveStr);
                if (idx == -1) {
                  position = position.copy(bestScore = bestScoreThisDepth, moves = position.moves :+
                                           Move(move = moveStr, bestMoves = moveList, score = score, link = link, depth = depth, scores = Seq(score)))
                } else {
                  position = position.copy(maxDepth = depth, 
                                bestScore = bestScoreThisDepth, 
                                moves = position.moves.updated(idx, 
                                   position.moves(idx).copy(bestMoves = moveList, 
                                      score = score, 
                                      link = link, 
                                      depth = depth, 
                                      scores = if(position.moves(idx).scores.length >= depth) { 
                                            position.moves(idx).scores.updated(depth - 1, score)} 
                                         else { 
                                           position.moves(idx).scores :+ score
                                         }
                                   )))
                }

              }

            } else if (line.startsWith("bestmove")) {
              return true;
            }
          }
          return false;
        }
      }
    }
    exe.close();

  }
}