package tfd.coderover

import collection.mutable.Stack

class Controller(val state:State, environment:Environment = DefaultEnvironment, constraints:Constraints = DefaultConstraints) {
  private[this] var callStackSize = 0

  private[coderover] val blockMap = new scala.collection.mutable.HashMap[String, List[Instruction]]()

  private[this] val stack = new Stack[Int]()

  private[coderover] def moveForward(distance:Int):ResultOrAbend[Unit] = {
    var absDistance = Math.abs(distance)
    var postForwardAbend:Option[Abend] = None
    while (!stopped
        && postForwardAbend.isEmpty
        && absDistance > 0
        && canMoveForward()) {
          executeMoveForward()
          postForwardAbend = environment.postMoveForward(state)
        	absDistance = absDistance - 1
    }
    if (postForwardAbend.isEmpty) {
      SuccessResultUnit
    } else {
      new ResultOrAbend(postForwardAbend.get)
    }
  }

  protected def executeMoveForward() = state.moveForward()

  private[coderover] def turnRight() = state.turnRight()

  private[coderover] def turnLeft() = state.turnLeft()

  private[coderover] def paint() = environment.paint(state)

  private[coderover] def print(value:String) = println(value)  

  private[coderover] val memory = new Array[Int](constraints.memorySize)

  private[coderover] var stopped = false

  private[coderover] def push(value:Int):ResultOrAbend[Unit] =  {
      stack.push(value)
      if (depth > constraints.maxStackSize) {
        new ResultOrAbend(StackOverflow)
      } else {
        SuccessResultUnit
    }
  }

  private[coderover] def pop():ResultOrAbend[Unit] =
      if (!stack.isEmpty) {
	  	  stack.pop()
        SuccessResultUnit
      } else {
  		  new ResultOrAbend(IllegalOperationOnEmptyStack)
  		}

  private[coderover] def top:ResultOrAbend[Int] =
      if (!stack.isEmpty) {
	  	  new ResultOrAbend(stack.top)
      } else {
  		  new ResultOrAbend(IllegalOperationOnEmptyStack)
      }


  private[coderover] def depth = stack.size

  private[coderover] def resetCallStack() {
    callStackSize = 0
  }

  private[coderover] def incrementCallStack():ResultOrAbend[Unit] = {
    callStackSize = callStackSize + 1
    if (callStackSize > constraints.maxCallStackSize) {
       new ResultOrAbend[Unit](CallStackOverflow)
    } else {
        SuccessResultUnit
    }
  }

  private[coderover] def decrementCallStack() {
    callStackSize = callStackSize - 1
  }

  private[coderover] def mem(address:Int):ResultOrAbend[Int] =
     if (address > 0 && address < memory.size) {
         new ResultOrAbend(memory(address))
     } else {
         new ResultOrAbend(InvalidMEMAddress(address));
     }

  private[coderover] def store(address:Int, value:Int):ResultOrAbend[Unit] = {
     if (address > 0 && address < memory.size) {
          new ResultOrAbend(memory(address) = value)
     } else {
          new ResultOrAbend(InvalidMEMAddress(address));
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

  private[coderover] def postMoveForward():ResultOrAbend[Unit] = {
    val result = environment.postMoveForward(state)
    if (result.isEmpty) {
      SuccessResultUnit
    } else {
      new ResultOrAbend(result.get)
    }
  }
  
  private[coderover] def gridX = state.gridX

  private[coderover] def gridY = state.gridY

  private[coderover] def deltaX = state.deltaX

  private[coderover] def deltaY = state.deltaY

}
