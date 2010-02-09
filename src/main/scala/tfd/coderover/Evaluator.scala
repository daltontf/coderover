package tfd.coderover

import collection.mutable.Stack

case class EvaluationResult[A](val value:Option[A], val abend:Option[Abend]) {
  def this(rawValue:A) = this(Some(rawValue), None)
  def this(rawAbend:Abend) = this(None, Some(rawAbend))

  def success = !value.isEmpty

  def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B] =
    if (success) f(value.get) else this.asInstanceOf[EvaluationResult[B]]

  def map[B](f: A => B): EvaluationResult[B] = 
     if (success) new EvaluationResult(f(value.get)) else this.asInstanceOf[EvaluationResult[B]]
}

object EvaluationSuccessUnit extends EvaluationResult[Unit](Some(()), None)

class Evaluator() {
  
  final def evaluate(instructions:List[Instruction], controller:Controller):EvaluationResult[Unit] = {
    controller.stopped = false
    evaluate(instructions, Array.empty[Int], controller)
  }  

  private[this] final def evaluate(instructions:List[Instruction], args:Array[Int], controller:Controller):EvaluationResult[Unit] =
    if (instructions != Nil) {
      instructions.tail.foldLeft(evaluateInstruction(instructions.head, args, controller)) {
         (previousResult, instruction) =>
            for (xp <- previousResult; yp <- evaluateInstruction(instruction, args, controller))
              yield (yp)
      }
    } else {
      EvaluationSuccessUnit
    }

  
  private[this] final def processDistance(distance:Option[Int], entity:String):EvaluationResult[Int] =
    if (distance == None) { 
    	new EvaluationResult(UnknownEntity(entity))
    } else {
      new EvaluationResult(distance.get)
    }

  private[this] def evaluateDivideByZero(expression:IntExpression, args:Array[Int], controller:Controller):EvaluationResult[Int] = {
    val result = evaluateInt(expression, args, controller)
    if (!result.value.isEmpty && result.value.get == 0) {
      new EvaluationResult(DivideByZero)
    } else {
      result
    }
  }

  private[coderover] val add = { (x:EvaluationResult[Int], y:EvaluationResult[Int]) =>
    for (xp <- x; yp <- x) yield (xp + yp)
  }

  private[coderover] final def evaluateInt(expression:IntExpression, args:Array[Int], controller:Controller):EvaluationResult[Int] = {
    expression match {
      case Constant(x)               => new EvaluationResult(x)
      case Add(expressionList) 	 	   => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
                                              yield (xp + yp)
                                        }
      case Subtract(expressionList)  => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
                                              yield (xp - yp) 
                                        }
      case Multiply(expressionList)  => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateInt(intExpression, args, controller))
                                              yield (xp * yp)
                                        }
      case Divide(expressionList)    => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, controller))
                                              yield (xp / yp)
                                        }
      case Modulus(expressionList)   => expressionList.tail.foldLeft(evaluateInt(expressionList.head, args, controller)) {
                                          (previousResult, intExpression) =>
                                            for (xp <- previousResult; yp <- evaluateDivideByZero(intExpression, args, controller))
                                              yield (xp % yp)
                                        }
      case Top() 				             => controller.top
      case GridX() 				           => new EvaluationResult(controller.gridX)
      case GridY() 				           => new EvaluationResult(controller.gridY)
      case DeltaX() 			           => new EvaluationResult(controller.deltaX)
      case DeltaY() 			           => new EvaluationResult(controller.deltaY)
      case Depth()				           => new EvaluationResult(controller.depth)
      case Abs(expr) 			           => for (x <- evaluateInt(expr, args, controller)) yield (Math.abs(x))
      case Max(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, controller);
                                             y <- evaluateInt(expr2, args, controller)) yield (Math.max(x,y))
      case Min(expr1, expr2) 	       => for (x <- evaluateInt(expr1, args, controller);
                                             y <- evaluateInt(expr2, args, controller)) yield (Math.min(x,y))
      case Negate(expr)			         => for (x <- evaluateInt(expr, args, controller)) yield (-x)
      case DistanceX(entity)  	     => processDistance(controller.distanceX(entity), entity)
      case DistanceY(entity)  	     => processDistance(controller.distanceY(entity), entity)
      case Mem(address)              => for (x <- evaluateInt(address, args, controller);
                                             y <- controller.mem(x)) yield (y)
      case Param(position)           => if (position > 0 && position <= args.length) {
                                          new EvaluationResult(args(position-1))
                                        } else {
                                          new EvaluationResult(UnboundParameter(position))
                                        }
    }
  }
  
   private[coderover] final def evaluateBoolean(booleanExpression:BooleanExpression, args:Array[Int], controller:Controller):EvaluationResult[Boolean] = {
	  booleanExpression match {
        case Obstructed(xExpression, yExpression) => for (x <- evaluateInt(xExpression, args, controller);
                                                          y <- evaluateInt(yExpression, args, controller))
                                                          yield (
                                                              (x < 0) ||
                                                              (y < 0) ||
                                                              (x >= controller.sizeX) ||
                                                              (y >= controller.sizeY) ||
                                                              controller.isObstructed(x,y) )
        case Painted(xExpression, yExpression)    => for (x <- evaluateInt(xExpression, args, controller);
                                                          y <- evaluateInt(yExpression, args, controller))
                                                          yield (controller.isPainted(x, y))
        case Not(booleanExpression)		            => for (x <- evaluateBoolean(booleanExpression, args, controller))
                                                          yield ( !x )
	      case And(booleanExpressionList)		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, controller)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateBoolean(booleanExpression, args, controller))
                                                            yield (xp && yp)
                                                     }
        case Or(booleanExpressionList) 		        => booleanExpressionList.tail.foldLeft(evaluateBoolean(booleanExpressionList.head, args, controller)) {
                                                        (previousResult, booleanExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateBoolean(booleanExpression, args, controller))
                                                            yield (xp || yp)
                                                     }
        case Equal(left, right) 			            => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x == y)
        case LessThan(left, right) 			          => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x < y)
        case GreaterThan(left, right) 	          => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x > y)
        case LessThanOrEqual(left, right) 	      => for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x <= y)
        case GreaterThanOrEqual(left, right)      =>  for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x >= y)
        case NotEqual(left, right) 			          =>  for (x <- evaluateInt(left, args, controller);
                                                           y <- evaluateInt(right, args, controller))
                                                             yield (x != y)
        case Adjacent(entity)				              =>  new EvaluationResult(controller.isAdjacent(entity))
	  }
  }
   
  private[coderover] final def evaluateString(expression:Expression, args:Array[Int], controller:Controller):EvaluationResult[String] = {
	   expression match {
        case StringConstant(value)					      => new EvaluationResult(value)
        case intExpression:IntExpression          => for (x <- evaluateInt(intExpression, args, controller))
                                                        yield(x.toString)
        case booleanExpression:BooleanExpression  => for (x <- evaluateBoolean(booleanExpression, args, controller))
                                                        yield(x.toString)
	   }  
  }

  private[coderover] final def isSuccessAndTrue(booleanExpression:BooleanExpression, args:Array[Int], controller:Controller)=
    evaluateBoolean(booleanExpression, args, controller).value == Some(true)

  private[coderover] final def evaluateInstruction(instruction:Instruction, args:Array[Int], controller:Controller):EvaluationResult[Unit] = {
      if (!controller.stopped) {
        instruction match {
            case Def(name, instructions) => {
                                                controller.blockMap += name -> instructions
                                                EvaluationSuccessUnit
                                            }
            case Call(name, callArgs)    => if (controller.blockMap.contains(name)) {
                                                controller.incrementCallStack()
                                                val evalArgs = callArgs.map{ evaluateInt(_, args, controller) }
                                                val failedArgEval = evalArgs.find{ !_.success }
                                                val result:EvaluationResult[Unit] = if (failedArgEval.isEmpty) {
                                                  evaluate(
                                                      controller.blockMap(name),
                                                      evalArgs.map {_.value.get }.toArray,
                                                      controller)
                                                } else {
                                                  new EvaluationResult(failedArgEval.get.abend.get)
                                                }
                                                controller.decrementCallStack()
                                                result
            				                        } else {
            					   	                    new EvaluationResult(UndefinedBlock(name))
            				                        }
        	  case Forward(expression)     => for (distance <- evaluateInt(expression, args, controller))
                                              yield {
        	                                      var absDistance = Math.abs(distance)
                                                while (!controller.stopped && absDistance > 0 && controller.canMoveForward()) {
        				                                  controller.moveForward()
        				                                  controller.postMoveForward()
        				                                  absDistance = absDistance - 1
        		                                    }
                                            }
        	  case TurnRight()             => {
                                              controller.turnRight()
                                              EvaluationSuccessUnit
                                            }
        	  case TurnLeft()	             => {
                                              controller.turnLeft()
                                              EvaluationSuccessUnit
                                            }
        	  case Push(expression)        => for (x <- evaluateInt(expression, args, controller);
                                                 y <- controller.push(x))
                                              yield (y)
            case Pop() 			             => controller.pop()
            case Replace(expression)     => for (x <- evaluateInt(expression, args, controller))
            	                                yield {
                                                controller.pop()
            	                                  controller.push(x)
                                              }
        	case If(booleanExpression, thenstatements, elsestatements) => 
        		for (x <- evaluateBoolean(booleanExpression, args, controller))
              yield {
                if (x) {
                  evaluate(thenstatements, args, controller)
        		    } else if (!elsestatements.isEmpty) {
        			    evaluate(elsestatements, args, controller)
        		    }
              }
        	case While(booleanExpression, blockstatements) => {
            var result:EvaluationResult[Unit] = EvaluationSuccessUnit
        		while (isSuccessAndTrue(booleanExpression, args, controller) && (result.success)) {
            		result = evaluate(blockstatements, args, controller)
        		}
            result
          }
          case Paint() => {
              controller.paint()
              EvaluationSuccessUnit
          }
          case Print(expressionList) => for (evaluationResult <- expressionList.tail.foldLeft(evaluateString(expressionList.head, args, controller)) {
                                                        (previousResult, stringExpression) =>
                                                          for (xp <- previousResult; yp <- evaluateString(stringExpression, args, controller))
                                                            yield (xp + yp)
                                           }) yield { controller.print(evaluationResult) }
          case Store(address, value) => for (x <- evaluateInt(address, args, controller);
                                             y <- evaluateInt(value, args, controller))
                                                yield {  controller.store(x, y)  }
        }
      } else {
          new EvaluationResult(None, None)
      }
  }
}
