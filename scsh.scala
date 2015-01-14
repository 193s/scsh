import scala.io.StdIn
import scala.io.AnsiColor
import scala.sys.process._


val logger = ProcessLogger (
  out => println(out),
  err => println(err)
)

// command execution
def system(str: String) = Process(str) ! logger

// read a line with prompt
def readLine(prompt: String) = {
  print(prompt)
  val ret = StdIn.readLine()
  ret
}

val prompt = AnsiColor.RED + "$ " + AnsiColor.RESET

while (true) {
  readLine(prompt) match {
    case "" => // do nothing
    case "exit" =>
      println("Program will exit...")
      sys.exit(0)

    case in =>
      try system(in)
      catch {
        case e: java.io.IOException =>
          println(s"permission denied: $in")
      }
  }
}
