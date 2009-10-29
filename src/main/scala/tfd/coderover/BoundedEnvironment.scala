package tfd.coderover

class BoundedEnvironment(val sizeX:Int, val sizeY:Int) extends Environment {
    override def canMoveForward(state:State) = {
      val nextX = state.gridX + state.deltaX
      val nextY = state.gridY + state.deltaY
      (nextX >= 0 && nextX < sizeX && nextY >=0 && nextY < sizeY)
    }
  
    override def postMoveForward(state:State) { }
}
