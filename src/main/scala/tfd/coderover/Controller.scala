package tfd.coderover

import collection.mutable.Stack

class Controller(val state:State, environment:Environment = DefaultEnvironment, constraints:Constraints = DefaultConstraints) {
  private[this] var callStackSize = 0

  private[coderover] val blockMap = new scala.collection.mutable.HashMap[String, List[Instruction]]()

  private[this] val stack = new Stack[Int]()

  private[coderover] def moveForward() = state.moveForward()

  private[coderover] def turnRight() = state.turnRight()

  private[coderover] def turnLeft() = state.turnLeft()

  private[coderover] def paint() = environment.paint(state)

  private[coderover] def print(value:String) = println(value)  

  private[coderover] val memory = new Array[Int](constraints.memorySize)

  private[coderover] var stopped = false

  private[coderover] def push(value:Int) =  {
      stack.push(value)
      if (depth > constraints.maxStackSize) {
        new EvaluationResult[Unit](StackOverflow)
      } else {
        EvaluationSuccessUnit
    }
  }

  private[coderover] def pop() =
      if (!stack.isEmpty) {
	  	  stack.pop()
        EvaluationSuccessUnit
      } else {
  		  new EvaluationResult[Unit](IllegalOperationOnEmptyStack)
  		}

  private[coderover] def top =
      if (!stack.isEmpty) {
	  	  new EvaluationResult[Int](stack.top)
      } else {
  		  new EvaluationResult[Int](IllegalOperationOnEmptyStack)
  		}

  private[coderover] def depth = stack.size

  private[coderover] def resetCallStack() {
    callStackSize = 0
  }

  private[coderover] def incrementCallStack():EvaluationResult[Unit] = {
    callStackSize = callStackSize + 1
    if (callStackSize > constraints.maxCallStackSize) {
       new EvaluationResult[Unit](CallStackOverflow)
    } else {
        EvaluationSuccessUnit
    }
  }

  private[coderover] def decrementCallStack() {
    callStackSize = callStackSize - 1
  }

  private[coderover] def mem(address:Int):EvaluationResult[Int] =
     if (address > 0 && address < memory.size) {
         new EvaluationResult(memory(address))
     } else {
         new EvaluationResult(InvalidMEMAddress(address));
     }

  private[coderover] def store(address:Int, value:Int):EvaluationResult[Unit] = {
     if (address > 0 && address < memory.size) {
          new EvaluationResult(memory(address) = value)
     } else {
          new EvaluationResult(InvalidMEMAddress(address));
     }
  }

  private[coderover] val sizeX = environment.sizeX

  private[coderover] val sizeY = environment.sizeY

  private[coderover] def distanceX(entity:String):Option[Int] = environment.distanceX(entity, state)

  private[coderover] def distanceY(entity:String):Option[Int] = environment.distanceY(entity, state)

  private[coderover] def isObstructed(x:Int, y:Int) = environment.obstructed((x,y))

  private[coderover] def isPainted(x:Int, y:Int) = environment.isPainted(x, y, state)

  private[coderover] def isAdjacent(entity:String) = environment.adjacent(entity, state)

  private[coderover] def canMoveForward() = environment.canMoveForward(state)

  private[coderover] def postMoveForward() = environment.postMoveForward(state)
  
  private[coderover] def gridX = state.gridX

  private[coderover] def gridY = state.gridY

  private[coderover] def deltaX = state.deltaX

  private[coderover] def deltaY = state.deltaY

}
