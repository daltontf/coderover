package tfd.coderover

import collection.mutable.Stack

class Controller(val state:State, environment:Environment = DefaultEnvironment, constraints:Constraints = DefaultConstraints) {
  var executionState:ExecutionState = _

  private[coderover] def resetState() {
    executionState = new ExecutionState(constraints)
  }

  private[coderover] def moveForward():ResultOrAbend[Unit] = {
    var postForwardAbend:Option[Abend] = None
    if (!executionState.stopped && canMoveForward()) {
          executeMoveForward()
          postForwardAbend = environment.postMoveForward(state)
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

  private[coderover] def push(value:Int):ResultOrAbend[Unit] =  {
      executionState.stack.push(value)
      if (executionState.stack.size > constraints.maxStackSize) {
        new ResultOrAbend(StackOverflow)
      } else {
        SuccessResultUnit
    }
  }

  private[coderover] def pop():ResultOrAbend[Unit] =
      if (!executionState.stack.isEmpty) {
	  	  executionState.stack.pop()
        SuccessResultUnit
      } else {
  		  new ResultOrAbend(IllegalOperationOnEmptyStack)
  		}

  private[coderover] def top:ResultOrAbend[Int] =
      if (!executionState.stack.isEmpty) {
	  	  new ResultOrAbend(executionState.stack.top)
      } else {
  		  new ResultOrAbend(IllegalOperationOnEmptyStack)
      }


  private[coderover] def depth = executionState.stack.size

  private[coderover] def incrementCallStack():ResultOrAbend[Unit] = {
    executionState.incrementCallStack()
    if (executionState.callStackSize > constraints.maxCallStackSize) {
       new ResultOrAbend[Unit](CallStackOverflow)
    } else {
        SuccessResultUnit
    }
  }
  
  private[coderover] def mem(address:Int):ResultOrAbend[Int] =
     if (address >= 0 && address < executionState.memory.size) {
         new ResultOrAbend(executionState.memory(address))
     } else {
         new ResultOrAbend(InvalidMEMAddress(address));
     }

  private[coderover] def store(address:Int, value:Int):ResultOrAbend[Unit] = {
     if (address >= 0 && address < executionState.memory.size) {
          new ResultOrAbend(executionState.memory(address) = value)
     } else {
          new ResultOrAbend(InvalidMEMAddress(address));
     }
  }

  private[coderover] val sizeX = environment.sizeX

  private[coderover] val sizeY = environment.sizeY

  private[coderover] def distanceX(entity:String):Option[Int] = environment.distanceX(entity, state)

  private[coderover] def distanceY(entity:String):Option[Int] = environment.distanceY(entity, state)

  private[coderover] def isObstructed(x:Int, y:Int) = environment.isObstructed(x,y)

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

  private[coderover] def stop = if (executionState != null) executionState.stopped = true

  private[coderover] def stopped = executionState != null && executionState.stopped
}
