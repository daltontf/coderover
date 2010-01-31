package tfd.coderover

class Controller(environment:Environment, constraints:Constraints) {
  private[this] var callStackSize = 0

  private[coderover] val memory = new Array[Int](constraints.memorySize)

  private[coderover] def moveForward(state:State) = state.moveForward()
  
  private[coderover] def turnRight(state:State) = state.turnRight()
  
  private[coderover] def turnLeft(state:State) = state.turnLeft()
  
  private[coderover] def paint(state:State) = environment.paint(state)

  private[coderover] def push(state:State, value:Int) {
    state.push(value)
    if (state.depth > constraints.maxStackSize) {
      state.fail(StackOverflow)
    }
  }

  private[coderover] def resetCallStack() {
    callStackSize = 0
  }

  private[coderover] def incrementCallStack(state:State) {
    callStackSize = callStackSize + 1
    if (callStackSize > constraints.maxCallStackSize) {
      state.fail(CallStackOverflow)
    }
  }

  private[coderover] def decrementCallStack() {
    callStackSize = callStackSize - 1
  }

  private[coderover] def mem(address:Int, state:State) =
     if (address > 0 && address < memory.size) {
         memory(address)
     } else {
         state.fail(InvalidMEMAddress(address));
         0
     }

  private[coderover] def store(address:Int, value:Int, state:State) {
     if (address > 0 && address < memory.size) {
          memory(address) = value
     } else {
          state.fail(InvalidMEMAddress(address));
     }
  }

  private[coderover] def print(value:String) = println(value)
}
