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

package object never_typehint_context2 {

  implicit val ctx = new Context {
    val name = "Never-TypeHint-Context"

    override val typeHintStrategy = NeverTypeHint

  }

  def main2(args: Array[String]): Unit = {
    val mongoConn = MongoConnection();
    val fen = new FEN;
    var maxDepth = 20;
    val maxThreshhold = 1.0;
    val positionsColl = mongoConn("mongochess")("positions");
    while (true) {
    }

  }
}
