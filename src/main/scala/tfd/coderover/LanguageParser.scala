package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

object LanguageParser extends JavaTokenParsers {
  
  def expression:Parser[Expression] = "(" ~> mathematical <~ ")" | function | arityTwoFunction | arityOneFunction | constant 
  
  def mathematical:Parser[Mathematical] = expression ~ ("+"|"-"|"*"|"/"|"%") ~ expression ^^ {
    case left~"+"~right => Add(left, right)
    case left~"-"~right => Subtract(left, right)
    case left~"*"~right => Multiply(left, right)
    case left~"/"~right => Divide(left, right) 
    case left~"%"~right => Modulus(left, right)
  }
  
  def functionParameter = (mathematical | function | arityTwoFunction | arityOneFunction | constant)
  
  def arityOneFunction:Parser[Expression] = "ABS" ~ "(" ~ functionParameter <~ ")" ^^ {
    case "ABS"~_~parm => Abs(parm)
  }
  
  def arityTwoFunction:Parser[Expression] = ("MAX"|"MIN") ~ "(" ~ functionParameter ~ "," ~ functionParameter <~ ")" ^^ {
    case "MAX"~_~parm1~_~parm2 => Max(parm1, parm2)
    case "MIN"~_~parm1~_~parm2 => Min(parm1, parm2)
  }

  def arityTwoBoolean:Parser[BooleanExpression] = ("ISPAINTED") ~ "(" ~ functionParameter ~ "," ~ functionParameter <~ ")" ^^ {
    case "ISPAINTED"~_~parm1~_~parm2 => IsPainted(parm1, parm2)
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
  
  def nestedBoolean:Parser[BooleanExpression] = "(" ~> (comparison | logical | not | arityTwoBoolean ) <~ ")"
  
  def not:Parser[BooleanExpression] = "NOT" ~> nestedBoolean ^^ { expression => Not(expression) } 
  
  def logical:Parser[BooleanExpression] = logicalOr | logicalAnd
  
  def logicalOr = (nestedBoolean ~ "OR" ~ rep1sep(nestedBoolean, "OR")) ^^ {
    case head~"OR"~tail => Or(head :: tail)
  } 
  
   def logicalAnd = (nestedBoolean ~ "AND" ~ rep1sep(nestedBoolean, "AND")) ^^ {
    case head~"AND"~tail => And(head :: tail)
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
  
  def paint:Parser[Paint] = "PAINT"~> opt(expression) ^^ {
    case Some(expression) => Paint(expression)
    case None => Paint(Constant(0))
  }
  
  def instruction:Parser[Instruction] = forward | right | left | paint | ifStatement | whileStatement | push
  
  def program = rep(instruction)
  
  def parse(text:String) = parseAll(program, text)

}
