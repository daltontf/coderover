package tfd.coderover

abstract class Environment {
  def canMoveForward(state:State):Boolean
  
  def postMoveForward(state:State):Unit 
}

object DefaultEnvironment extends Environment {
  
  override def canMoveForward(state:State) = true
  
  override def postMoveForward(state:State) { }

}
