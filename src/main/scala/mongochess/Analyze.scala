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
    val exe = new Exe("/Users/wfreeman/Documents/Stockfish/src/stockfish", out => println("o: " + out), err => println("e: " + err))
    exe.write("setoption name Hash value 1024");
    exe.write("setoption name MultiPV value 100");
    val mongoConn = MongoConnection();
    val fen = new FEN;
    var maxDepth = 15;
    val maxThreshhold = 100.0;
    val positionsColl = mongoConn("mongochess")("positions");
    while (true) {
      val unanalyzed = positionsColl.find("maxDepth" $lt maxDepth).sort(MongoDBObject("minMoves" -> -1, "bestScore" -> 1)).limit(1);
      //val unanalyzed = positionsColl.find("maxDepth" $lt maxDepth).sort(MongoDBObject("bestScore" -> 1)).limit(10);
      if (unanalyzed.size == 0) maxDepth += 1;

      for (positionDB <- unanalyzed) {
        var position = grater[Position].asObject(positionDB);
        exe.write("position fen " + position.fen);
        exe.write("go depth " + maxDepth);
        if (maxDepth > 4) {
          println("analyzing to depth " + maxDepth + " cur best: " + position.bestScore + ": " + position.fen);
          new TxChessBoardDisplay(fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]).print
        }
        if (readUntilBestMove()) {
          val bestScore: Double = {
            var best = -500.0;

            for (an <- position.engineAnalysis) {
              if (an.depth == maxDepth) {
                best = an.bestScore;
              }
            }
            best;
          }
          println("analyzed in " + position.rt + " seconds; best score: " + bestScore + ". \n");
          positionsColl.save(grater[Position].asDBObject(position.copy(ut = new Date, bestScore = bestScore)))
        }

        def readUntilBestMove(): Boolean = {
          val san = new SAN
          val createHash = new HashSet[String]
          def algToCM(b: ChessBoard, str: String): ChessMove = {
            //println(str);
            if (str.length == 4) {
              new ChessMove(b,
                san.fileToNum(str(0)),
                san.rankToNum(str(1)),
                san.fileToNum(str(2)),
                san.rankToNum(str(3)));
            } else {
              val pieceIdx = str.substring(4, 5) match {
                case "q" => Queen.INDEX;
                case "n" => Knight.INDEX;
                case "b" => Bishop.INDEX;
                case "r" => Rook.INDEX;
              }
              new ChessMove(b,
                san.fileToNum(str(0)),
                san.rankToNum(str(1)),
                san.fileToNum(str(2)),
                san.rankToNum(str(3)),
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
                bestScoreThisDepth = -500.0;
              }
              if (depth >= lastDepth) {
                def getCurDepth(eaList: Seq[EngineAnalysis]): (EngineAnalysis, Int) = {
                  if (eaList != null) {
                    for (i <- 0 until eaList.size) {
                      if (eaList(i).depth == depth) {
                        return (eaList(i), i)
                      }
                    }
                  } else {
                    position = position.copy(engineAnalysis = Seq[EngineAnalysis]())
                  }
                  val newEA = EngineAnalysis(depth = depth, analysis = Seq[Analysis]());
                  position = position.copy(engineAnalysis = position.engineAnalysis :+ newEA)
                  return (newEA, position.engineAnalysis.length - 1);
                }
                val (ea, engineAnalysisIdx) = getCurDepth(position.engineAnalysis);

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
                val move = san.moveToString(chessMove);
                board.playMove(chessMove);
                val fenAfterMove = fen.boardToString(board);
                val moves = {
                  var ms = split.slice(18, split.size);
                  val b = fen.stringToBoard(position.fen).asInstanceOf[ChessBoard]
                  b.playMove(algToCM(b, split(17)));
                  for (m <- ms) yield {
                    val cm = algToCM(b, m);
                    b.playMove(cm)
                    san.moveToString(cm);
                  }
                }
                var curAnalysis = Analysis(score = score, move = move, mate = mate, moves = moves);

                if (ea.depth == maxDepth && (curAnalysis.score > -maxThreshhold && curAnalysis.score < maxThreshhold)) {
                  // make sure it doesn't exist first (and check the hash)
                  if (!createHash.contains(fenAfterMove)) {
                    val fenSplit = fenAfterMove.split(" ");
                    val shortenedFen = {
                      var res = "";
                      for (i <- 0 to 3) {
                        res += fenSplit(i) + " ";
                      }
                      res.substring(0, res.length - 1);
                    }
                    println("checking whether fen exists: " + shortenedFen)
                    //shortenedFen.replaceAll("/", "\\/");
                    createHash.add(fenAfterMove);
                    val temp = fen.stringToBoard(fenAfterMove).asInstanceOf[ChessBoard];
                    if (temp.getUnCapturedPieces(false).size + temp.getUnCapturedPieces(true).size > 6 && !temp.is50MoveRuleApplicible) {
                      if (positionsColl.count(MongoDBObject("pos" -> shortenedFen)) == 0) {
                        println("creating new position: ");
                        new TxChessBoardDisplay(fen.stringToBoard(fenAfterMove).asInstanceOf[ChessBoard]).print;
                        val newPos = Position(fen = fenAfterMove, pos = shortenedFen, minMoves = position.minMoves + 1, bestScore = -curAnalysis.score);
                        positionsColl.save(grater[Position].asDBObject(newPos));
                        curAnalysis = curAnalysis.copy(link = newPos.id)
                      }
                    }

                  }
                }

                def containsMove(seq: Seq[Analysis]): Boolean = {
                  for (an <- seq) {
                    if (an.move == move) {
                      return true;
                    }
                  }
                  return false;
                }
                if (!containsMove(ea.analysis)) {
                  val newSeq = position.engineAnalysis.updated(
                    engineAnalysisIdx,
                    ea.copy(analysis = ea.analysis :+ curAnalysis, bestScore = bestScoreThisDepth));
                  position = position.copy(engineAnalysis = newSeq, maxDepth = depth, rt = time);
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
