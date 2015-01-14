import scala.io.StdIn
import scala.io.AnsiColor
import scala.sys.process._


val logger = ProcessLogger (
  out => println(out),
  err => println(err)
)

// command execution
def system(cmd: Seq[String]) = Process(cmd) ! logger
def parse(cmd: String): List[String] = cmd.split(' ').map(_.trim).toList

// read a line with prompt
def readLine(prompt: String) = {
  print(prompt)
  val ret = StdIn.readLine()
  ret
}
val prompt = AnsiColor.RED + "$ " + AnsiColor.RESET

while (true) {
  val input = readLine(prompt)
  parse(input) match {
    case Nil => // do nothing

    // shell built-in commands
    case "exit" :: xs =>
      println("Program will exit...")
      sys.exit(0)

    case "cd" :: xs =>
      println("Not implemented yet :(")

    case in =>
      // result (0 or else)
      val result = try system(in)
      catch {
        case e: java.io.IOException =>
          println(s"permission denied: $input")
          1
      }
  }
}
