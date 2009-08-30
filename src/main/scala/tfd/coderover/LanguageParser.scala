package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

object LanguageParser extends JavaTokenParsers {
  
  def expression:Parser[Expression] = "(" ~> mathematical <~ ")" | function | arityOneFunction | constant 
  
  def mathematical:Parser[Mathematical] = expression ~ ("+"|"-") ~ expression ^^ {
    case left~"+"~right => Plus(left, right)
    case left~"-"~right => Minus(left, right)
  }
  
  def arityOneFunction:Parser[Expression] = "ABS" ~ "(" ~ (mathematical | function | arityOneFunction | constant) <~ ")" ^^ {
    case "ABS"~_~parm => Abs(parm)
  }
  
  def function:Parser[Expression] = ("POP"|"TOP"|"GRIDX"|"GRIDY"|"DELTAX"|"DELTAY") ^^ {
    	case "POP" => Pop()
    	case "TOP" => Top()
    	case "GRIDX" => GridX()
        case "GRIDY" => GridY()
        case "DELTAX" => DeltaX()
        case "DELTAY" => DeltaY()
  }
   
  def constant:Parser[Constant] = wholeNumber ^^ { x => Constant(x.toInt) } 
    
  def comparison:Parser[Comparison] = expression ~ ("=" | "<=" | ">=" | "<>" | "<" | ">" ) ~ expression ^^ {
         case left~"="~right 	=> Equal(left, right)
         case left~"<="~right 	=> LessThanOrEqual(left, right)
         case left~">="~right 	=> GreaterThanOrEqual(left, right)
         case left~"<>"~right 	=> NotEqual(left, right)
         case left~"<"~right 	=> LessThan(left, right)
         case left~">"~right 	=> GreaterThan(left, right)
    }     			  
  
  def nestedBoolean:Parser[BooleanLogic] = "(" ~> (comparison | logical) <~ ")"
  
  def logical:Parser[BooleanLogic] = nestedBoolean ~ ("OR" | "AND") ~ nestedBoolean ^^ { 
    	case left~"OR"~right => Or(left, right)
    	case left~"AND"~right => And(left, right) 
  	}  
  
  def elseBlock:Parser[List[Instruction]] = "ELSE" ~ "{" ~> rep(instruction) <~ "}"
  
  def elseIfBlock:Parser[List[Instruction]] = "ELSE" ~> ifStatement ^^ {
	  x => List(x)
  }
  
  def ifStatement:Parser[If] = "IF" ~> nestedBoolean ~ "{" ~ 
	  		rep(instruction) ~ "}" ~  opt(elseBlock | elseIfBlock) ^^
    {
    	case ifExpression~_~thenInstructions~_~Some(elseInstructions) => If(ifExpression, thenInstructions, elseInstructions)
    	case ifExpression~_~thenInstructions~_~None => If(ifExpression, thenInstructions, Nil)
  	}
  
  def whileStatement:Parser[While] = "WHILE" ~> nestedBoolean ~ "{" ~ rep(instruction) <~ "}" ^^ {
    	case whileExpression~_~blockInstructions => While(whileExpression, blockInstructions) 
  	} 
  
  def forward:Parser[Forward] = "FORWARD"~>opt(expression) ^^ { 
    	case Some(expression)=> Forward(expression) 
    	case None => Forward(Constant(1))  			
  	} 
  
  def push:Parser[Push] = "PUSH"~> expression ^^ { x:Expression => Push(x)}
  
  def right:Parser[TurnRight] = "RIGHT" ^^ { _ => TurnRight() }
  
  def left:Parser[TurnLeft] = "LEFT" ^^ { _ => TurnLeft() }
  
  def instruction:Parser[Instruction] = forward | right | left | ifStatement | whileStatement | push
  
  def program = rep(instruction)
  
  def parse(text:String) = parseAll(program, text)

}
