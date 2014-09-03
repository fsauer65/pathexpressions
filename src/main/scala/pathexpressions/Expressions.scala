package pathexpressions

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.Map

object Expressions {

  private case class Match(target: Variable, key:String, groups:List[String])

  private case class Matcher(vars: Variable*) {

    /**
     * Match a single string against all my variables
     * @param path string to match
     * @return list of matches with the full match as well as a List with all wildcard values
     */
    def apply(path:String):Iterable[Match] = vars.flatMap { case v =>
      v.path.findAllIn(path).matchData.flatMap (m => Range(0,m.groupCount+1) map (m.group(_))).toList match {
        case series :: groups => Some(Match(v,series,groups))
        case _ => None
      }
    }

    /**
     * Find the matches for the list of strings for which the wildcard values match for all inputs
     * @param paths list of paths that have to any of my variables as long as the wildcards have identical values
     * @return a list with the matches if there are any. Empty list of there are none with matching wildcards
     */
    def apply(paths:Iterable[String]):Iterable[Match] = paths.flatMap(apply).groupBy(_.groups).filter {
      case (_,v) => v.size == vars.size // only keep the groups for which ALL variables have a match
    }.flatMap (_._2)
  }

  /**
   * Base trait for both Expressions and Predicates. T is the result type of evaluating a node
   * @tparam T Double for Expressions and Boolean for Predicates
   */
  sealed trait Node[T] {
    protected val left : Expression
    protected val right: Expression

    /**
     * collect a list of all Variable nodes used in the subtree starting at this node
     * @return
     */
    protected lazy val collectVars: List[Variable] = left.collectVars ++ right.collectVars

    /**
     * resolve all variables by looking them up in the given symbol table.
     * All wildcards must resolve to the same value
     * @param symbolTable symbol table to find the variable values
     * @return list of matches or empty if not all variables could be resolved (with the same wildcards)
     */
    protected def resolve(implicit symbolTable: Map[String,Double]):Map[Variable,Double] =
      Matcher(collectVars:_*)(symbolTable.keys).foldLeft(Map[Variable,Double]()) {
        case (result,Match(v,series, _)) => result.updated(v,symbolTable.get(series).get)
      }

    /**
     * Only public entry point in Node:
     * Evaluate the value of this node. If any variable is undefined, return None
     * @return Option[Boolean] for predicates or Option[Double] for expressions
     */
    def value(implicit symbolTable: Map[String,Double]): Option[T] = eval(resolve)

    /**
     * Default implementation of Recursive tree traverser for all internal nodes.
     * Only the leaf nodes Constant and Variable override in order to terminate the recursion.
     * @param resolvedVars Map of all resolved variables
     * @return Some(T) or None if left or right subtree return None
     */
    protected def eval(implicit resolvedVars: Map[Variable,Double]): Option[T] = for {l <- left.eval; r <- right.eval} yield eval(l,r)

    /**
     * Abstract actual evaluation function. Expressions return Double, Predicates return Boolean
     * @param l left value
     * @param r right value
     * @return either a Boolean for Predicates or a Double for expressions
     */
    protected def eval(l:Double,r:Double):T
  }

  sealed trait Expression extends Node[Double]

  case class Constant(v:Double) extends Expression {
    override val left  = this // a smell from not having a separate leaf node class
    override val right = this // a smell from not having a separate leaf node class
    override lazy val collectVars: List[Variable] = List.empty
    override def eval(implicit symbolTable: Map[Variable,Double]): Option[Double] = Some(v)
    override def eval(l:Double,r:Double) = 0.0 // a smell from not having a separate leaf node class
  }

  case class Variable(path:Regex) extends Expression {
    override val left  = this // a smell from not having a separate leaf node class
    override val right = this  // a smell from not having a separate leaf node class
    override lazy val collectVars: List[Variable] = List(this)
    override def eval(l:Double,r:Double) = 0.0 // a smell from not having a separate leaf node class
    override def eval(implicit resolvedVars: Map[Variable,Double]): Option[Double] = resolvedVars.get(this)
  }

  case class Minus(left:Expression, right:Expression) extends Expression {
    override def eval(l:Double, r:Double):Double = l - r
  }
  case class Plus(left:Expression, right:Expression) extends  Expression {
    override def eval(l:Double, r:Double):Double = l + r
  }
  case class Multiply(left:Expression, right:Expression) extends Expression {
    override def eval(l:Double, r:Double):Double = l * r
  }
  case class Divide(left:Expression, right:Expression) extends Expression {
    override def eval(l:Double, r:Double):Double = l / r
  }

  sealed trait Predicate extends Node[Boolean]

  case class LT(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double):Boolean = l < r
  }
  case class LTE(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double):Boolean = l <= r
  }
  case class EQ(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double):Boolean = l == r
  }
  case class GTE(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double):Boolean = l >= r
  }
  case class GT(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double):Boolean = l > r
  }

  class Parser extends JavaTokenParsers {

    /**
     * converts a glob-style expression into a proper regex with escaped periods and groups around all wildcards
     * @param s glob-style path with optional wildcards
     * @return a fully anchored regex with surrounding "" removed,  periods escaped and all * surrounded with ()
     */
    private def globToRegex(s: String): Regex = s"""^${s.replace("\"", "").replace(".", "\\.").replace("*", "(.*)")}$$""".r

    def glob: Parser[Regex] = stringLiteral ^^ (globToRegex(_))

    def expr: Parser[Expression] = term ~ (("+" | "-") ~ term).* ^^ {
      case head ~ tail => tail.foldLeft(head) {
        case (left,"+" ~ right) => Plus(left, right)
        case (left,"-" ~ right) => Minus(left, right)
      }
    }

    def term: Parser[Expression] = factor ~ (("*" | "/") ~ factor).* ^^ {
      case head ~ tail => tail.foldLeft(head) {
        case (left,"*" ~ right) => Multiply(left, right)
        case (left,"/" ~ right) => Divide(left, right)
      }
    }

    def factor: Parser[Expression] = variable | ("(" ~> expr <~ ")") | constant

    def constant: Parser[Expression] = floatingPointNumber ~ unit.? ^^ {
      case num ~ multiplier => Constant(num.toDouble * multiplier.getOrElse(1.0))
    }

    def unit: Parser[Double] = (
        "[kK][bB]?".r ^^^ 1000.0
      | "[mM][bB]?".r ^^^ 1000 * 1000.0
      | "[gG][bB]?".r ^^^ 1000 * 1000 * 1000.0
      | "%" ^^^ 0.01
    )

    def predicate: Parser[Predicate] = expr ~ ("<=" | "<" | "==" | ">=" | ">") ~ expr ^^ {
      case left ~ "<=" ~ right => LTE(left, right)
      case left ~ "<"  ~ right => LT(left, right)
      case left ~ "==" ~ right => EQ(left, right)
      case left ~ ">=" ~ right => GTE(left, right)
      case left ~ ">"  ~ right => GT(left, right)
    }

    def variable: Parser[Expression] = glob ^^ (Variable(_))

    def parseExpression(text: String): ParseResult[Expression] = parseAll(expr, text)

    def parsePredicate(text: String): ParseResult[Predicate] = parseAll(predicate, text)
  }

}
