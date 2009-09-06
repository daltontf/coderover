package tfd.coderover

object Main {
	import LanguageParser._
	import Evaluator._
 
  def run(code:String, state:State) = {
    val result = parse(code)
    if (result.successful) {
    	evaluate(result.get, state)
    } else {
        println(result)
    }
  }
  
  def main(args:Array[String]) {
    val state = new State(8,7,0)
    run("""
PUSH (8 - GRIDX)
IF (TOP > 0) {
  WHILE (DELTAX <> 1) {
	  RIGHT
  }
} ELSE IF (TOP < 0) {
  WHILE (DELTAX <> -1) {
	  RIGHT
  }
}
FORWARD POP
PUSH (8 - GRIDY)
IF (TOP > 0) {
  WHILE (DELTAY <> 1) {
	  RIGHT
  }
} ELSE IF (TOP < 0) {
  WHILE (DELTAY <> -1) {
	  RIGHT
  }
}
FORWARD POP
    """,state)
    println(state)
  }
}
