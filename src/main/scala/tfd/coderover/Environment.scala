package tfd.coderover

trait Environment {
  def canMoveForward(state:State) = true
  
  def postMoveForward(state:State) { }
  
  def paint(color:Int, x:Int, y:Int) { }
  
  def distanceX(entity:String, state:State):Option[Int] = None
  
  def distanceY(entity:String, state:State):Option[Int] = None
  
  def adjacent(entity:String, state:State) = false

  def isPainted(x:Int, y:Int) = false

  def paintColor(x:Int, y:Int, state:State):Option[Int] = None
} 

object DefaultEnvironment extends Environment 
