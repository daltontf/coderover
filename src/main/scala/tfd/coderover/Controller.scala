package tfd.coderover

class Controller(environment:Environment) {
  def moveForward(state:State) = state.moveForward()
  
  def turnRight(state:State) = state.turnRight()
  
  def turnLeft(state:State) = state.turnLeft()
  
  def paint(color:Int, state:State) = environment.paint(color, state)
  
  def print(value:String) = println(value)
}
