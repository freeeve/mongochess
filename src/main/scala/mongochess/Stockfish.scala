package wes.mongochess

object Stockfish {
  var exe: Exe = null
}
class Stockfish(ram: Int, multipv: Int, path: String = "") {
  def start() = {
    Stockfish.exe = new Exe("stockfish", out => println("o: " + out), err => println("e: " + err))
    Stockfish.exe.write("setoption name Hash value " + ram);
    Stockfish.exe.write("setoption name MultiPV value " + multipv);
  }

  def setFEN(fen: String) = {
    Stockfish.exe.write("position fen " + fen);
  }

  def stop() = {
    Stockfish.exe.close();
  }

  def goToDepth(depth: Int) = {
    Stockfish.exe.write("go depth " + depth);
  }

  def goInfinite() = {
    Stockfish.exe.write("go infinite");
  }

  def readLine() = {
    Stockfish.exe.readLine
  }

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