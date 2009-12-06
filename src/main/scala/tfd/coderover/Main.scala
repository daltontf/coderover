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
    println(parseAll(booleanExpression,"NOT(1 = 2)"))
  }
}
