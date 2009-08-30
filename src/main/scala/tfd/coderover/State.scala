package tfd.coderover

object State {
  val directions =  Array((0,-1), // North
                          (1, 0), // East
                          (0, 1), // South
                          (-1,0)) // West
}


case class State(
  
  var droidX:Int,
  var droidY:Int,
  var droidDirectionIndex:Int
) {  
  import scala.collection.mutable.Stack
  
  val stack = new Stack[Int]()
  
  def droidMoveForward(distance:Int) = {
    droidX = droidX + (State.directions(droidDirectionIndex)._1 * distance)
    droidY = droidY + (State.directions(droidDirectionIndex)._2 * distance)
  }
  
  def droidTurnRight() = droidDirectionIndex = (droidDirectionIndex + State.directions.length + 1) % State.directions.length
  
  def droidTurnLeft() = droidDirectionIndex = (droidDirectionIndex + State.directions.length - 1) % State.directions.length
  
  def push(value:Int) = stack.push(value)
  
  def pop() = stack.pop()
  
  def top() = stack.top
  
  def deltaX = State.directions(droidDirectionIndex)._1
  
  def deltaY = State.directions(droidDirectionIndex)._2
  
}
