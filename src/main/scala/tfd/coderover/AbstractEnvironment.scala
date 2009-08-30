package tfd.coderover

abstract trait AbstractEnvironment {
  
  def createInitialState:State
  
  def isObjectiveComplete(state:State)

}
