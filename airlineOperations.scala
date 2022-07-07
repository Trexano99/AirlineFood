package airlineFood


sealed trait Operation { 
	def exec () : Unit
}



case class CreateVar (private val varName : String) extends Operation {
	override def exec() = AirlineStack.addVar(varName)
}  
case class CreateVarAndPoint (private val varName : String) extends Operation {
	override def exec() = {
	  AirlineStack.addVar(varName)
	  AirlineStack.pointerToHead
	}
}  
case class PointerDown () extends Operation {
	override def exec() = AirlineStack.pointerDown
}  
case class PointerUp () extends Operation {
	override def exec() = AirlineStack.pointerUp
} 
case class PointerToVar (private val varName : String) extends Operation {
	override def exec() = AirlineStack.pointerToVar(varName)
} 
case class AddValueOfVarToPoint (private val varName : String) extends Operation {
	override def exec() = AirlineStack.changeValOfVarPointed(AirlineStack.valueOfVar(varName) + AirlineStack.valueOfVarPointed)	
} 
case class SubValueOfVarToPoint (private val varName : String) extends Operation {
	override def exec() = AirlineStack.changeValOfVarPointed(AirlineStack.valueOfVarPointed - AirlineStack.valueOfVar(varName) )	
} 
case class MultValueOfVarToPoint (private val varName : String) extends Operation {
	override def exec() = AirlineStack.changeValOfVarPointed(AirlineStack.valueOfVar(varName) * AirlineStack.valueOfVarPointed)	
} 
case class ChangePointedValue (private val value : Int) extends Operation {
	override def exec() = AirlineStack.changeValOfVarPointed(value)	
} 

case class PrintPointerValue () extends Operation {
	override def exec() = println(AirlineStack.valueOfVarPointed)
}
case class AskForChangePointed () extends Operation {
	override def exec() = AirlineStack.changeValOfVarPointed(scala.io.StdIn.readLine().toInt)
}


case class While (private val operations : List[Operation] ) extends Operation {
	override def exec() ={
	   while (AirlineStack.valueOfVarPointed!=0) {
	      operations.foreach(y => y.exec)
	   }
	}
}
