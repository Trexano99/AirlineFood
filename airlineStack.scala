package airlineFood

import scala.collection.mutable.ListBuffer

object AirlineStack {

	private var stack : ListBuffer[(String, Int)] = ListBuffer.empty[(String, Int)]
	private var pointer : Int = 0

	def addVar (varName : String)  =
		stack += ((varName, 1))
	
	def pointerDown =
		if (pointer > 0)
		   pointer-=1
	
	def pointerUp =
		if (pointer < stack.size-1)
		  pointer+=1

	def pointerToHead = 
		pointer = stack.size-1
		
	def pointerToVar(varName : String) = {
		val index = stack.indexWhere(_._1 == varName)
		if (index != -1)
			pointer = index 
	}
			
			
	def valueOfVar(varName : String) : Int=
		(stack.find(_._1 == varName).getOrElse(null,-1))._2
		
	def valueOfVarPointed : Int =
		stack.apply(pointer)._2
		
	
		
	def changeValOfVarPointed(newValue : Int) = {
		stack.update(pointer, (stack.apply(pointer)._1 , newValue))
	}
		
	//CAN USE FOR DEBUG
	def debugOutput ={
		println("Pointer: "+ pointer)
		println("Stack: "+ stack)
	}
}

