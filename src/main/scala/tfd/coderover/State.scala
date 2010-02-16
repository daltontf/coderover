package tfd.coderover

case class State(
  var gridX:Int,
  var gridY:Int,
  var directionIndex:Int
) {  

  import Constants._

  def moveForward() = {
    gridX = gridX + deltaX
    gridY = gridY + deltaY
  }
  
  def turnRight() = directionIndex = (directionIndex + DIRECTIONS.length + 1) % DIRECTIONS.length
  
  def turnLeft() = directionIndex = (directionIndex + DIRECTIONS.length - 1) % DIRECTIONS.length
      
  def deltaX = DIRECTIONS(directionIndex)._1
  
  def deltaY = DIRECTIONS(directionIndex)._2

  def setEqual(other:State) {
    gridX = other.gridX
    gridY = other.gridY
    directionIndex = other.directionIndex
  }
}
