package tfd.coderover

object State {
  val NORTH = (0, -1)
  val EAST = (1, 0)
  val SOUTH = (0, 1)
  val WEST = (-1, 0)
  
  val DIRECTIONS =  Array(NORTH,
                          EAST, 
                          SOUTH,
                          WEST) 
}


case class State(
  
  var gridX:Int,
  var gridY:Int,
  var directionIndex:Int
) {  
  var stopped = false;
  
  import scala.collection.mutable.Stack
  import State._
  
  private val stack = new Stack[Int]()
  
  def moveForward(distance:Int) = {
    gridX = gridX + (DIRECTIONS(directionIndex)._1 * distance)
    gridY = gridY + (DIRECTIONS(directionIndex)._2 * distance)
  }
  
  def turnRight() = directionIndex = (directionIndex + DIRECTIONS.length + 1) % DIRECTIONS.length
  
  def turnLeft() = directionIndex = (directionIndex + DIRECTIONS.length - 1) % DIRECTIONS.length
  
  def push(value:Int) = stack.push(value)
  
  def pop() = stack.pop()
  
  def top() = stack.top
  
  def deltaX = DIRECTIONS(directionIndex)._1
  
  def deltaY = DIRECTIONS(directionIndex)._2
  
}
