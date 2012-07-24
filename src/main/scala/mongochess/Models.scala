package wes.mongochess.model

import com.novus.salat._
import com.novus.salat.annotations._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import java.util.Date
import scala.collection.mutable.Map

case class Move(
  move: String,
  score: Double = 0.0,
  depth: Int = 0,
  mate: Option[Int] = None,
  scores: Seq[Double] = Seq[Double](),
  bestMoves: Seq[String] = null,
  endFen: String = null,
  forcedDraw: Option[Boolean] = None,
  fen:String = null,
  @Ignore stored:Boolean = false
)

case class Position( 
  @Key("_id") id: ObjectId = new ObjectId,
  fen: String,
  pos: String,
  bestScore: Double = -50000.0,
  maxDepth: Int = 0,
  minMoves: Int = 0,
  moves: Seq[Move]  = null,
  forcedDraw:Option[Boolean] = None
)