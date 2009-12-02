package tfd.coderover

class Controller(environment:Environment, constraints:Constraints) {
  private[this] var callStackSize = 0

  private[coderover] val memory = new Array[Int](constraints.memorySize)

  def moveForward(state:State) = state.moveForward()
  
  def turnRight(state:State) = state.turnRight()
  
  def turnLeft(state:State) = state.turnLeft()
  
  def paint(state:State) = environment.paint(state)

  def push(state:State, value:Int) {
    state.push(value)
    if (state.depth > constraints.maxStackSize) {
      state.fail(StackOverflow)
    }
  }

  def resetCallStack() {
    callStackSize = 0
  }

  def incrementCallStack(state:State) {
    callStackSize = callStackSize + 1
    if (callStackSize > constraints.maxCallStackSize) {
      state.fail(CallStackOverflow)
    }
  }

  def decrementCallStack() {
    callStackSize = callStackSize - 1
  }

  def mem(address:Int, state:State) =
     if (address > 0 && address < memory.size) {
         memory(address)
     } else {
         state.fail(InvalidMEMAddress(address));
         0
     }

  def store(address:Int, value:Int, state:State) {
     if (address > 0 && address < memory.size) {
          memory(address) = value
     } else {
          state.fail(InvalidMEMAddress(address));
     }
  }

  def print(value:String) = println(value)
}
