package tfd.coderover

class BoundedEnvironment(private val maxX:Int, private val maxY:Int) extends Environment {
    override def canMoveForward(state:State) = {
      val nextX = state.gridX + state.deltaX
      val nextY = state.gridY + state.deltaY
      (nextX >= 0 && nextX <= maxX && nextY >=0 && nextY <= maxY)
    }
  
    override def postMoveForward(state:State) { }
}
