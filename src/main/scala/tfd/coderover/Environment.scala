package tfd.coderover

trait Environment {
  def canMoveForward(state:State) = true
  
  def postMoveForward(state:State) { }
  
  def paint(state:State) { }
  
  def distanceX(entity:String, state:State):Option[Int] = None
  
  def distanceY(entity:String, state:State):Option[Int] = None
  
  def adjacent(entity:String, state:State) = false

  def isPainted(x:Int, y:Int, state:State) = false

  def store(address:Int, value:Int, state:State) { }

  def mem(address:Int, state:State) = 0
} 

object DefaultEnvironment extends Environment 
