package wes.mongochess.model

import com.novus.salat._
import com.novus.salat.annotations._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import java.util.Date

case class EngineAnalysis(
  depth: Int = 0,
  engine: String = "stockfish",
  bestScore: Double = -500.0,
  analysis: Seq[Analysis] = null
)

case class Analysis(
  score: Double,
  move: String,
  link: ObjectId = null,
  mate: Option[Int] = None,
  moves: Seq[String]
)

case class Position( 
  @Key("_id") id: ObjectId = new ObjectId,
  fen: String,
  pos: String,
  bestScore: Double = -500.0,
  maxDepth: Int = 0,
  minMoves: Int = 0,
  ut: Date = new Date,
  ct: Date = new Date,
  rt: Long = 0,
  engineAnalysis: Seq[EngineAnalysis] = null
)