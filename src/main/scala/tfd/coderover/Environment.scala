package tfd.coderover

class Environment(val sizeX:Int, val sizeY:Int, protected val obstructed:Set[(Int,Int)]) {

  def this(sizeX:Int, sizeY:Int) = this(sizeX, sizeY, Set())

  def canMoveForward(state:State) = {
      val nextX = state.gridX + state.deltaX
      val nextY = state.gridY + state.deltaY
      (nextX >= 0 && nextX < sizeX && nextY >=0 && nextY < sizeY && !obstructed.contains((nextX, nextY)))
    }
  
  def postMoveForward(state:State) { }
  
  def paint(state:State) { }
  
  def distanceX(entity:String, state:State):Option[Int] = None
  
  def distanceY(entity:String, state:State):Option[Int] = None
  
  def adjacent(entity:String, state:State) = false

  def isPainted(x:Int, y:Int, state:State) = false
} 

object DefaultEnvironment extends Environment(10,10) 
