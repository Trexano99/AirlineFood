package airlineFood

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import scala.util.matching._
import scala.util.Try


class AirlineParser(private val input: String) extends JavaTokenParsers{

	def parse : ParseResult[List[Operation]] = parseAll(rootParser, input)
	
	def rootParser : Parser[List[Operation]] = rep(phrase) 
	
	def phrase : Parser[Operation] = decl | declMovePoint | movePointD | movePointU | movePointToV | addVarToPoint | subVarToPoint | multVarToPoint | whileCond | askInput | askOuput
	
	
	//DECLARATIONS
	def decl : Parser[Operation] = "You" ~> "ever" ~> "notice" ~> """[a-zA-Z0-9_ ]*""".r <~"?" ^^ {case "airline food" => (CreateVar(null)) case x => (CreateVar(x))}
	def declMovePoint : Parser[Operation] = "What's" ~> "the" ~> "deal" ~> "with" ~> """[a-zA-Z0-9_ ]*""".r <~"?" ^^ {case "airline food" => (CreateVarAndPoint(null)) case x => (CreateVarAndPoint(x))}
	
	//POINTER MOVE
	def movePointD : Parser[Operation] = "Um," ^^ {case _ => (PointerDown())}
	def movePointU : Parser[Operation] = "Yeah," ^^ {case _ => (PointerUp())}
	def movePointToV : Parser[Operation] = "Let's" ~> "talk" ~> "about" ~> """[a-zA-Z0-9_ ]*""".r <~ "." ^^ {case x => (PointerToVar(x))}
	
	//OPERATIONS
	def addVarToPoint : Parser[Operation] = "It's" ~> "kinda" ~> "like" ~> """[a-zA-Z0-9_ ]*""".r <~ "." ^^ {case x => (AddValueOfVarToPoint(x))}
	def subVarToPoint : Parser[Operation] = "Not" ~> "like" ~> """[a-zA-Z0-9_ ]*""".r <~ "." ^^ {case x => (SubValueOfVarToPoint(x))}
	def multVarToPoint : Parser[Operation] = "Just" ~> "like" ~> """[a-zA-Z0-9_ ]*""".r <~ "." ^^ {case x => (MultValueOfVarToPoint(x))}
	
	//CONDITIONS
	def whileCond : Parser[Operation] = "So..." ~> rep(phrase) <~ "Moving" <~ "on..." ^^ {case listPh => (While(listPh))}
	
	//INPUT-OUTPUT
	def askInput : Parser[Operation] = "Right?" ^^ {case _ => (AskForChangePointed())}
	def askOuput : Parser[Operation] = "See?" ^^ {case _ => (PrintPointerValue())}
}

