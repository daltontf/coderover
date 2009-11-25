package tfd.coderover

class Controller(environment:Environment) {
  def moveForward(state:State) = state.moveForward()
  
  def turnRight(state:State) = state.turnRight()
  
  def turnLeft(state:State) = state.turnLeft()
  
  def paint(state:State) = environment.paint(state)
  
  def print(value:String) = println(value)
}
