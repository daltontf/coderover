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
  var abend:Option[Abend] = None
  
  import scala.collection.mutable.Stack
  import State._
  
  private val stack = new Stack[Int]()
  
  def moveForward() = {
    gridX = gridX + deltaX
    gridY = gridY + deltaY
  }
  
  def turnRight() = directionIndex = (directionIndex + DIRECTIONS.length + 1) % DIRECTIONS.length
  
  def turnLeft() = directionIndex = (directionIndex + DIRECTIONS.length - 1) % DIRECTIONS.length
  
  def push(value:Int) = stack.push(value)
  
  def pop() = if (!stack.isEmpty) {
	  			 stack.pop()
  			  } else {
  				 fail(IllegalOperationOnEmptyStack)
  				 0
  			  }
  
  def top() = if (!stack.isEmpty) {
	  			 stack.top
  			  } else {
  				 fail(IllegalOperationOnEmptyStack)
  				 0
  			  }
  
  def deltaX = DIRECTIONS(directionIndex)._1
  
  def deltaY = DIRECTIONS(directionIndex)._2
  
  def fail(abend:Abend) {
    this.abend = Some(abend)
    stopped = true
  } 
  
  def reset() {
    stopped = false
    abend = None
  }
}
