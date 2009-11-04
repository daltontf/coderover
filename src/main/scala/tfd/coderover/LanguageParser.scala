package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

object LanguageParser extends JavaTokenParsers {
  
  def expression:Parser[Expression] = "(" ~> mathematical <~ ")" | arityNoneFunction | arityOneFunction | arityTwoFunction | constant 
  
  def mathematical:Parser[Mathematical] = mathematical("+", (head:Expression, tail:List[Expression]) => Add(head :: tail)) |
    								 	  mathematical("-", (head:Expression, tail:List[Expression]) => Subtract(head :: tail)) |
    								 	  mathematical("*", (head:Expression, tail:List[Expression]) => Multiply(head :: tail)) |
    								 	  mathematical("/", (head:Expression, tail:List[Expression]) => Divide(head :: tail)) |
    								 	  mathematical("%", (head:Expression, tail:List[Expression]) => Modulus(head :: tail))
  
  def mathematical(sign:String, f:(Expression, List[Expression]) => Mathematical) =
    (expression ~ sign ~ rep1sep(expression, sign)) ^^ {
      case head~sign~tail => f(head, tail)
    }
  
  def functionParameter = (mathematical | arityNoneFunction | arityOneFunction |arityTwoFunction | constant)
  
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
  
  def arityNoneFunction:Parser[Expression] = ("TOP"|"GRIDX"|"GRIDY"|"DELTAX"|"DELTAY"|"DEPTH") ^^ {
    	case "TOP" => Top()
    	case "GRIDX" => GridX()
        case "GRIDY" => GridY()
        case "DELTAX" => DeltaX()
        case "DELTAY" => DeltaY()
        case "DEPTH" => Depth()
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
  
  def pop:Parser[Pop] = "POP" ^^ { _ => Pop() }
  
  def right:Parser[TurnRight] = "RIGHT" ^^ { _ => TurnRight() }
  
  def left:Parser[TurnLeft] = "LEFT" ^^ { _ => TurnLeft() }
  
  def paint:Parser[Paint] = "PAINT"~> opt(expression) ^^ {
    case Some(expression) => Paint(expression)
    case None => Paint(Constant(0))
  }
  
  def replace:Parser[Replace] = "REPLACE"~>expression ^^ { x:Expression => Replace(x) }
    
  def command:Parser[Instruction] = forward | right | left | paint | push | pop | replace
  
  def controlFlow:Parser[Instruction] = ifStatement | whileStatement
  
  def instruction:Parser[Instruction] = command  | controlFlow
  
  def program = rep(instruction)
  
  def parse(text:String) = parseAll(program, text)

}
