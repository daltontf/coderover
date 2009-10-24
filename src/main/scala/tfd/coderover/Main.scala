package tfd.coderover

object Main {
	import LanguageParser._

 
  def run(code:String, state:State) = {
    val result = parse(code)
    if (result.successful) {
    	new Evaluator(DefaultEnvironment).evaluate(result.get, state)
    } else {
        println(result)
    }
  }
  
  def main(args:Array[String]) {
    val state = new State(8,7,0)
    run("""
WHILE (ISPAINTED(1,2)) { FORWARD }
    """,state)
    println(state)
  }
}
