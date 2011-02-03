package tfd.coderover

class Environment(
        val sizeX:Int,
        val sizeY:Int
  ) {
  
  def isObstructed(x:Int, y:Int) = false
  
  def paint(x:Int, y:Int) { }

  def obstruct(x:Int, y:Int) { }
  
  def distanceX(entity:String, x:Int, y:Int):Option[Int] = None
  
  def distanceY(entity:String, x:Int, y:Int):Option[Int] = None

  def count(entity:String):Option[Int] = None

  def adjacent(entity:String, x:Int, y:Int) = false

  def isPainted(x:Int, y:Int) = false
} 

object DefaultEnvironment extends Environment(10,10) 
