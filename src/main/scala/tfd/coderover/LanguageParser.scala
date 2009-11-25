package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

object LanguageParser extends JavaTokenParsers {
  
  def intExpression:Parser[IntExpression] = negatableExpression | negatedExpression | constant
  
  def negatableExpression:Parser[IntExpression] = "(" ~> mathematical <~ ")" | arityNoneFunction | arityOneFunction | arityTwoFunction | arityOneIdentFunction
  
  def negatedExpression:Parser[IntExpression] = "-"~>negatableExpression ^^ { expression => Negate(expression) } 
  
  def mathematical:Parser[Mathematical] = mathematical("+", (head:IntExpression, tail:List[IntExpression]) => Add(head :: tail)) |
    								 	  mathematical("-", (head:IntExpression, tail:List[IntExpression]) => Subtract(head :: tail)) |
    								 	  mathematical("*", (head:IntExpression, tail:List[IntExpression]) => Multiply(head :: tail)) |
    								 	  mathematical("/", (head:IntExpression, tail:List[IntExpression]) => Divide(head :: tail)) |
    								 	  mathematical("%", (head:IntExpression, tail:List[IntExpression]) => Modulus(head :: tail))
  
  def mathematical(sign:String, f:(IntExpression, List[IntExpression]) => Mathematical) =
    (intExpression ~ sign ~ rep1sep(intExpression, sign)) ^^ {
      case head~sign~tail => f(head, tail)
    }
  
  def expressionParameter:Parser[IntExpression] = mathematical | intExpression 
  
  def arityOneFunction:Parser[IntExpression] = "ABS" ~ "(" ~ expressionParameter <~ ")" ^^ {
    case "ABS"~_~parm => Abs(parm)
  }
  
  def arityOneIdentFunction:Parser[IntExpression] = ("DISTANCEX" | "DISTANCEY") ~ "(" ~ ident <~ ")" ^^ {
    case "DISTANCEX"~_~parm => DistanceX(parm)
    case "DISTANCEY"~_~parm => DistanceY(parm)
  }
  
  def arityTwoFunction:Parser[IntExpression] = ("MAX"|"MIN"|"PAINTCOLOR") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case "MAX"~_~parm1~_~parm2 => Max(parm1, parm2)
    case "MIN"~_~parm1~_~parm2 => Min(parm1, parm2)
  }
  
  
  def arityNoneFunction:Parser[IntExpression] = ("TOP"|"GRIDX"|"GRIDY"|"DELTAX"|"DELTAY"|"DEPTH") ^^ {
    	case "TOP" => Top()
    	case "GRIDX" => GridX()
      case "GRIDY" => GridY()
      case "DELTAX" => DeltaX()
      case "DELTAY" => DeltaY()
      case "DEPTH" => Depth()
  }

  def adjacent:Parser[BooleanExpression] = "ADJACENT" ~ "(" ~> ident <~ ")" ^^ {
        	x => Adjacent(x)
  }
        
  def arityTwoBoolean:Parser[BooleanExpression] = ("PAINTED") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
        case "PAINTED"~_~parm1~_~parm2 => Painted(parm1, parm2)
  }
   
  def constant:Parser[Constant] = wholeNumber ^^ { x => Constant(x.toInt) } 
    
  def comparison:Parser[Comparison] = intExpression ~ ("=" | "<=" | ">=" | "<>" | "<" | ">" ) ~ intExpression ^^ {
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
   
  def printString = "PRINT" ~> rep1sep((intExpression | nestedBoolean | stringConstant), "+") ^^ { Print(_) }
  
  def stringConstant = stringLiteral ^^ { x=> StringConstant(x.substring(1, x.length-1)) }
  
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
  
  def forward:Parser[Forward] = "FORWARD"~>opt(intExpression) ^^ { 
    	case Some(expression)=> Forward(expression) 
    	case None => Forward(Constant(1))  			
  	} 
  
  def push:Parser[Push] = "PUSH"~> intExpression ^^ { x:IntExpression => Push(x)}
  
  def pop:Parser[Pop] = "POP" ^^ { _ => Pop() }
  
  def right:Parser[TurnRight] = "RIGHT" ^^ { _ => TurnRight() }
  
  def left:Parser[TurnLeft] = "LEFT" ^^ { _ => TurnLeft() }
  
  def paint:Parser[Paint] = "PAINT" ^^ { _ => Paint() }
  
  def replace:Parser[Replace] = "REPLACE"~>intExpression ^^ { x:IntExpression => Replace(x) }
    
  def command:Parser[Instruction] = forward | right | left | paint | push | pop | replace | call | printString
  
  def controlFlow:Parser[Instruction] = ifStatement | whileStatement 
  
  def definition:Parser[Def] = "DEF" ~> ident ~ "{" ~ rep(instruction) <~ "}" ^^ {
    case name~_~instructions => Def(name, instructions)
  }
  
  def call:Parser[Call] = "CALL" ~> ident ^^ { x:String => Call(x) }
  
  def instruction:Parser[Instruction] = command  | controlFlow 
  
  def topLevelInstruction:Parser[Instruction] = instruction | definition
  
  def program = rep(topLevelInstruction)
  
  def parse(text:String) = parseAll(program, text)

}
