package airlineFood

import java.io.FileNotFoundException
import scala.io.Source.fromFile
import scala.util.{Failure, Success, Try}


object Main extends App {

  val input: String = Try(fromFile("src/test.txt")) match {
    case Success(file) => file.mkString
    case Failure(e)    => throw new FileNotFoundException()
  }

  val p = new AirlineParser(input)

  p.parse match {
    case p.Success(operazioni: List[Operation], _) =>  operazioni.foreach(y => y.exec)
    case p.Failure(x, _) => println(x)
    case p.Error(x,_) => println(x)
    }
}

