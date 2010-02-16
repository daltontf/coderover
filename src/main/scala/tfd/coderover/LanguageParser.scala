package tfd.coderover

import scala.util.parsing.combinator.JavaTokenParsers

class LanguageParser extends JavaTokenParsers {

  override protected val whiteSpace = """(\s+|//[^\n]*\n|/\*(.|[\r\n])*?\*/|//[^\n]*$)+""".r
  
  lazy val intExpression:Parser[IntExpression] = negatableExpression | negatedExpression | param | constant
  
  lazy val negatableExpression:Parser[IntExpression] = "(" ~> mathematical <~ ")" | arityNoneFunction | arityOneFunction | arityTwoFunction | arityOneIdentFunction
  
  lazy val negatedExpression:Parser[IntExpression] = "-"~>negatableExpression ^^ { expression => Negate(expression) }
  
  lazy val mathematical:Parser[Mathematical] = mathematical("+", (head:IntExpression, tail:List[IntExpression]) => Add(head :: tail)) |
    								 	  mathematical("-", (head:IntExpression, tail:List[IntExpression]) => Subtract(head :: tail)) |
    								 	  mathematical("*", (head:IntExpression, tail:List[IntExpression]) => Multiply(head :: tail)) |
    								 	  mathematical("/", (head:IntExpression, tail:List[IntExpression]) => Divide(head :: tail)) |
    								 	  mathematical("%", (head:IntExpression, tail:List[IntExpression]) => Modulus(head :: tail))
  
  def mathematical(sign:String, f:(IntExpression, List[IntExpression]) => Mathematical) =
    (intExpression ~ sign ~ rep1sep(intExpression, sign)) ^^ {
      case head~sign~tail => f(head, tail)
    }
  
  lazy val expressionParameter:Parser[IntExpression] = mathematical | intExpression
  
  lazy val arityOneFunction:Parser[IntExpression] = ("ABS"|"MEM") ~ "(" ~ expressionParameter <~ ")" ^^ {
    case "ABS"~_~parm => Abs(parm)
    case "MEM"~_~parm => Mem(parm)
  }
  
  lazy val arityOneIdentFunction:Parser[IntExpression] = ("DISTANCEX" | "DISTANCEY") ~ "(" ~ ident <~ ")" ^^ {
    case "DISTANCEX"~_~parm => DistanceX(parm)
    case "DISTANCEY"~_~parm => DistanceY(parm)
  }
  
  lazy val arityTwoFunction:Parser[IntExpression] = ("MAX"|"MIN") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
    case "MAX"~_~parm1~_~parm2 => Max(parm1, parm2)
    case "MIN"~_~parm1~_~parm2 => Min(parm1, parm2)
  }
  
  lazy val arityNoneFunction:Parser[IntExpression] = ("TOP"|"X"|"Y"|"DX"|"DY"|"DEPTH") ^^ {
    	case "TOP" => Top()
    	case "X" => GridX()
      case "Y" => GridY()
      case "DX" => DeltaX()
      case "DY" => DeltaY()
      case "DEPTH" => Depth()
  }

  lazy val param:Parser[Param] = """:\d+""".r ^^ { x => Param(x.substring(1).toInt) }

  lazy val constant:Parser[Constant] = wholeNumber ^^ { x => Constant(x.toInt) }

  lazy val adjacent:Parser[BooleanExpression] = "ADJACENT" ~ "(" ~> ident <~ ")" ^^ {
        	x => Adjacent(x)
  }

  lazy val arityTwoBoolean:Parser[BooleanExpression] = ("PAINTED"|"OBSTRUCTED") ~ "(" ~ expressionParameter ~ "," ~ expressionParameter <~ ")" ^^ {
        case "PAINTED"~_~x~_~y => Painted(x, y)
        case "OBSTRUCTED"~_~x~_~y => Obstructed(x, y)
  }

  lazy val comparison:Parser[Comparison] = intExpression ~ ("=" | "<=" | ">=" | "<>" | "<" | ">" ) ~ intExpression ^^ {
         case left~"="~right 	=> Equal(left, right)
         case left~"<="~right 	=> LessThanOrEqual(left, right)
         case left~">="~right 	=> GreaterThanOrEqual(left, right)
         case left~"<>"~right 	=> NotEqual(left, right)
         case left~"<"~right 	=> LessThan(left, right)
         case left~">"~right 	=> GreaterThan(left, right)
    }     			  

  lazy val parenBoolean:Parser[BooleanExpression] = "(" ~> (nestedBoolean | booleanExpression) <~ ")"

  lazy val nestedBoolean:Parser[BooleanExpression] = comparison | logical | booleanExpression

  lazy val booleanExpression:Parser[BooleanExpression] =  ("(" ~> nestedBoolean  <~ ")") | not | adjacent | arityTwoBoolean 

  lazy val not:Parser[BooleanExpression] = "NOT" ~> parenBoolean ^^ { expression => Not(expression) }
  
  lazy val logical:Parser[BooleanExpression] = logicalOr | logicalAnd
  
  lazy val logicalOr = (booleanExpression ~ "OR" ~ rep1sep(booleanExpression, "OR")) ^^ {
    case head~"OR"~tail => Or(head :: tail)
  } 
  
   lazy val logicalAnd = (booleanExpression ~ "AND" ~ rep1sep(booleanExpression, "AND")) ^^ {
    case head~"AND"~tail => And(head :: tail)
  }
   
  lazy val printString = "PRINT" ~> rep1sep((intExpression | booleanExpression | stringConstant), "+") ^^ { Print(_) }
  
  lazy val stringConstant = stringLiteral ^^ { x=> StringConstant(x.substring(1, x.length-1)) }
  
  lazy val elseBlock:Parser[List[Instruction]] = "ELSE" ~ "{" ~> rep(instruction) <~ "}"
  
  lazy val elseIfBlock:Parser[List[Instruction]] = "ELSE" ~> ifStatement ^^ {
	  x => List(x)
  }
  
  lazy val ifStatement:Parser[If] = "IF" ~> booleanExpression ~ "{" ~
	  		rep(instruction) ~ "}" ~  opt(elseBlock | elseIfBlock) ^^
    {
    	case ifExpression~_~thenInstructions~_~Some(elseInstructions) => If(ifExpression, thenInstructions, elseInstructions)
    	case ifExpression~_~thenInstructions~_~None => If(ifExpression, thenInstructions, Nil)
  	}
  
  lazy val whileStatement:Parser[While] = "WHILE" ~> booleanExpression ~ "{" ~ rep(instruction) <~ "}" ^^ {
    	case whileExpression~_~blockInstructions => While(whileExpression, blockInstructions) 
  	} 
  
  lazy val forward:Parser[Forward] = "FORWARD"~>opt(intExpression) ^^ {
    	case Some(expression)=> Forward(expression) 
    	case None => Forward(Constant(1))  			
  	} 
  
  lazy val push:Parser[Push] = "PUSH"~> intExpression ^^ { x:IntExpression => Push(x)}
  
  lazy val pop:Parser[Pop] = "POP" ^^ { _ => Pop() }
  
  lazy val right:Parser[TurnRight] = "RIGHT" ^^ { _ => TurnRight() }
  
  lazy val left:Parser[TurnLeft] = "LEFT" ^^ { _ => TurnLeft() }
  
  lazy val paint:Parser[Paint] = "PAINT" ^^ { _ => Paint() }
  
  lazy val replace:Parser[Replace] = "REPLACE"~>intExpression ^^ { x:IntExpression => Replace(x) }

  lazy val store:Parser[Store] = "STORE" ~ "(" ~> intExpression ~ "," ~ intExpression <~ ")" ^^ {
    case address~_~value => Store(address, value)
  }
  
  lazy val command:Parser[Instruction] = forward | right | left | paint | push | pop | replace | call | store | printString
  
  lazy val controlFlow:Parser[Instruction] = ifStatement | whileStatement
  
  lazy val definition:Parser[Def] = "DEF" ~> ident ~ "{" ~ rep(instruction) <~ "}" ^^ {
    case name~_~instructions => Def(name, instructions)
  }
  
  lazy val call:Parser[Call] = "CALL" ~> ident ~ opt(callParams)  ^^ {
    case defName~Some(params) => Call(defName, params)
    case defName~None => Call(defName, Nil) 
  }

  lazy val callParam:Parser[IntExpression] = mathematical | intExpression 

  lazy val callParams:Parser[List[IntExpression]] = "(" ~> repsep(callParam, ",") <~ ")"
  
  lazy val instruction:Parser[Instruction] = command  | controlFlow
  
  lazy val topLevelInstruction:Parser[Instruction] = instruction | definition
  
  lazy val program = rep(topLevelInstruction)
  
  def parse(text:String) = parseAll(program, text)

}
