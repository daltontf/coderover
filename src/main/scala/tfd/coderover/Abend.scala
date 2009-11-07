package tfd.coderover

abstract class Abend(val message:String)

object IllegalOperationOnEmptyStack extends Abend("Illegal operation performed on empty stack")

case class UnknownEntity(val entity:String) extends Abend("Unknown entity :" + entity)

case class UndefinedBlock(val name:String) extends Abend("Undefined block :" + name)


