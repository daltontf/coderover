package tfd.coderover

trait Environment {
  def canMoveForward(state:State) = true
  
  def postMoveForward(state:State) { }
  
  def paint(color:Int, state:State) { }

  def isPainted(x:Int, y:Int) = false
} 

object DefaultEnvironment extends Environment 
