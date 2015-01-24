#!/usr/bin/env scala
import java.io.File
import java.util.regex.Matcher
import scala.io.StdIn
import scala.io.Source
import scala.io.AnsiColor
import scala.util.Properties
import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}

// ============================================ //

Runner.main()

// ============================================ //


object Runner {
  val alias = new Alias()
  val conf = new Conf(".scshrc")
  val env = new Env()

  // command execution
  def eval(input: String): Boolean =
    parse(input) match {
      case Nil => true // do nothing

      // shell built-in commands
      case "exit" :: xs =>
        println("Program will exit...")
        sys.exit(0)
        true

      // assignment
      case name :: "=" :: value :: _ =>
        println(s"+ $name -> $value")
        env.values += name -> value
        true

      case "env" :: Nil =>
        println(env.values.map(p => s"${p._1} -> ${p._2}").mkString("\n"))
        true


      case "cd" :: Nil => true // do nothing
      case "cd" :: x :: _ =>
        val dest = env.resolve(x)
        if (dest.isDirectory) {
          env.cd(x)
          true
        } else {
          println("cd: no such directory")
          false
        }

      case "alias" :: Nil =>
        println(alias.table.mkString("\n"))
        true

      case "alias" :: x :: _ =>
        if (!x.contains('=')) {
          if (alias.table.contains(x)) {
            println(s"$x=${alias.table(x)}")
            true
          } else false
        } else {
          val p = alias.splitByEq(x)
          println(s"alias: ${p._1} -> ${p._2}")
          alias.table += p
          true
        }

      case in =>
        import java.io.IOException
        // result (0 or else)
        val result = try env.system(alias.aliased(in))
        catch {
          case e: IOException =>
            // println(e.getMessage)
            println(s"permission denied: $input")
            1
        }
        result == 0
    }

  def main() {
    conf.eval()

    while (true) {
      val input = StdIn.readLine(env.values("prompt"))
      eval(input)
    }
  }

  def parse(cmd: String): List[String] =
    """\$\{([a-zA-Z_]+)\}""".r
    .replaceAllIn(cmd, m => Matcher.quoteReplacement(env.get(m.group(1))))
    .replace("\\ ", "\\$space")
    .split(' ')
    .map { _.trim.replace("\\$space", " ") }
    .toList


  // Config
  class Conf(file: String) {
    def eval(): Unit =
      for (l <- Source.fromFile(file).getLines) Runner.eval(l)
  }
}


object MyUtil {
  // get file by given path
  def getFile(file: File)(name: String) = new File(file.toURI.resolve(name))
}


// Environment
class Env {
  import scala.io.AnsiColor._
  private val p = RED + "$ " + RESET
  val values = mutable.Map[String, String]("prompt" -> p, "err_color" -> "x", "test" -> "hello")
  def envOrElse(key: String, default: => String) = Properties.envOrElse(key, default)
  def get(x: String) = envOrElse(x, values.getOrElse(x, ""))

  val logger = ProcessLogger (
    out => System.out.println(out),
    err => System.err.println (
      if (values("err_color") == "o") RED + err + RESET
      else err
    )
  )
  var pos = new File(".")

  def resolve(name: String) = MyUtil.getFile(pos)(name)

  def cd(name: String): Unit = {
    val dest = resolve(name)
    require(dest.isDirectory)
    pos = dest
  }

  // command execution
  def system(cmd: Seq[String]) = Process(cmd, pos) ! logger
}






class Alias {
  // aliases
  val table = mutable.Map[String, String]()
  // returns aliased list
  def aliased(cmd: List[String]): List[String] = cmd match {
    case Nil     => Nil
    case x :: xs => Runner.parse(aliased(x)) ::: xs
  }
  // returns aliased string
  def aliased(cmd: String): String = table.getOrElse(cmd, cmd)

  // split string by the first '='
  def splitByEq(str: String) =
    str.splitAt(str.indexOf('=')) match { case (l, r) => (l, r.tail) }
}

