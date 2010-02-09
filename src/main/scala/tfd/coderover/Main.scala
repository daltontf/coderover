package tfd.coderover

object Main {
	private[this] val languageParser = new LanguageParser()
  import languageParser._
 
  def run(code:String, state:State) = {
    val result = parse(code)
    if (result.successful) {
    	new Evaluator().evaluate(result.get, new Controller(state))
    } else {
        println(result)
    }
  }
  
  def main(args:Array[String]) {
    println(parseAll(booleanExpression,"NOT(1 = 2)"))
  }
}
