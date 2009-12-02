package tfd.coderover

class Environment(val sizeX:Int, val sizeY:Int)  {
  def canMoveForward(state:State) = {
      val nextX = state.gridX + state.deltaX
      val nextY = state.gridY + state.deltaY
      (nextX >= 0 && nextX < sizeX && nextY >=0 && nextY < sizeY)
    }
  
  def postMoveForward(state:State) { }
  
  def paint(state:State) { }
  
  def distanceX(entity:String, state:State):Option[Int] = None
  
  def distanceY(entity:String, state:State):Option[Int] = None
  
  def adjacent(entity:String, state:State) = false

  def isPainted(x:Int, y:Int, state:State) = false
} 

object DefaultEnvironment extends Environment(10,10) 
