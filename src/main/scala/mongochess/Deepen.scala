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
      
    }

  }
}
