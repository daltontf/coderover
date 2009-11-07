package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

object LanguageParser extends JavaTokenParsers {
  
  def expression:Parser[Expression] = negatableExpression | negatedExpression | constant
  
  def negatableExpression:Parser[Expression] = "(" ~> mathematical <~ ")" | arityNoneFunction | arityOneFunction | arityTwoFunction | arityOneIdentFunction
  
  def negatedExpression:Parser[Expression] = "-"~>negatableExpression ^^ { expression => Negate(expression) } 
  
  def mathematical:Parser[Mathematical] = mathematical("+", (head:Expression, tail:List[Expression]) => Add(head :: tail)) |
    								 	  mathematical("-", (head:Expression, tail:List[Expression]) => Subtract(head :: tail)) |
    								 	  mathematical("*", (head:Expression, tail:List[Expression]) => Multiply(head :: tail)) |
    								 	  mathematical("/", (head:Expression, tail:List[Expression]) => Divide(head :: tail)) |
    								 	  mathematical("%", (head:Expression, tail:List[Expression]) => Modulus(head :: tail))
  
  def mathematical(sign:String, f:(Expression, List[Expression]) => Mathematical) =
    (expression ~ sign ~ rep1sep(expression, sign)) ^^ {
      case head~sign~tail => f(head, tail)
    }
  
  def expressionParameter:Parser[Expression] = mathematical | expression 
  
  def arityOneFunction:Parser[Expression] = "ABS" ~ "(" ~ expressionParameter <~ ")" ^^ {
    case "ABS"~_~parm => Abs(parm)
  }
  
  def arityOneIdentFunction:Parser[Expression] = ("DISTANCEX" | "DISTANCEY") ~ "(" ~ ident <~ ")" ^^ {
    case "DISTANCEX"~_~parm => DistanceX(parm)
    case "DISTANCEY"~_~parm => DistanceY(parm)
  }
  
  def arityTwoFunction:Parser[Expression] = ("MAX"|"MIN") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case "MAX"~_~parm1~_~parm2 => Max(parm1, parm2)
    case "MIN"~_~parm1~_~parm2 => Min(parm1, parm2)
  }
  
  def adjacent:Parser[BooleanExpression] = "ADJACENT" ~ "(" ~> ident <~ ")" ^^ {
    	x => Adjacent(x)
  }

  def arityTwoBoolean:Parser[BooleanExpression] = ("PAINTED") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case "PAINTED"~_~parm1~_~parm2 => Painted(parm1, parm2)
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
  
  def nestedBoolean:Parser[BooleanExpression] = "(" ~> (comparison | logical | not | adjacent | arityTwoBoolean ) <~ ")"
  
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
