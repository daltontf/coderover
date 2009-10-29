package tfd.coderover

abstract class Abend(val message:String)

object IllegalOperationOnEmptyStack extends Abend("Illegal operation performed on empty stack")


