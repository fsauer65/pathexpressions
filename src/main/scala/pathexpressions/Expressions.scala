package pathexpressions

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.Map

object Expressions {

  case class Match(key:String, groups:List[String])

  case class Matcher(vars: Variable*) {

    /**
     * Match a single string against all my variables
     * @param path string to match
     * @return list of matches with the full match as well as a List with all wildcard values
     */
    def apply(path:String):Iterable[Match] = vars.map(_.path).flatMap { case rx =>
      rx.findAllIn(path).matchData.flatMap (m => Range(0,m.groupCount+1) map (m.group(_))).toList match {
        case series :: groups => Some(Match(series,groups))
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

  sealed trait Node[T] {
    val left: Expression
    val right: Expression

    /**
     * collect a list of all Variable nodes used in the subtree starting at this node
     * @return
     */
    lazy val collectVars: List[Variable] = left.collectVars ++ right.collectVars

    /**
     * resolve all variables by looking them up in the given symbol table.
     * All wildcards must resolve to the same value
     * @param universe symbol table to find the variable values
     * @return list of matches or empty if not all variables could be resolved (with the same wildcards)
     */
    def resolve(implicit universe: Map[String,Double]):Map[String,Double] =
      Matcher(collectVars:_*)(universe.keys).foldLeft(Map[String,Double]()) {
        case (result,Match(series, _)) => result.updated(series,universe.get(series).get)
      }

    /**
     * Evaluate the value of this node. If any variable is undefined, return None
     * @return Option[Boolean] for predicates or Option[Double] for expressions
     */
    def eval(implicit universe: Map[String,Double]): Option[T] = for {l <- left.eval; r <- right.eval} yield eval(l,r)(resolve)

    /**
     * Abstract actual evaluation function. Expressions return Double, Predicates return Boolean
     * @param l left value
     * @param r right value
     * @return either a Boolean for Predicates or a Double for expressions
     */
    def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):T
  }

  sealed trait Expression extends Node[Double]

  case class Constant(value:Double) extends Expression {
    override val left = this
    override val right = this
    override lazy val collectVars: List[Variable] = List.empty
    override def eval(implicit universe: Map[String,Double]): Option[Double] = Some(value)
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]) = value // won't get called
  }

  case class Variable(path:Regex) extends Expression {
    override val left = this
    override val right = this
    override lazy val collectVars: List[Variable] = List(this)
    override def eval(implicit universe: Map[String,Double]): Option[Double] = {
      //println(s"looking up $path in $universe")
      universe.find{case (k,v)=> path.findFirstIn(k).isDefined}.map(_._2)
    }
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):Double = 0.0 // won't get called
  }

  case class Minus(left:Expression, right:Expression) extends Expression {
    override def eval(l:Double, r:Double)(implicit universe: Map[String,Double]):Double = l - r
  }
  case class Plus(left:Expression, right:Expression) extends  Expression {
    override def eval(l:Double, r:Double)(implicit universe: Map[String,Double]):Double = l + r
  }
  case class Multiply(left:Expression, right:Expression) extends Expression {
    override def eval(l:Double, r:Double)(implicit universe: Map[String,Double]):Double = l * r
  }
  case class Divide(left:Expression, right:Expression) extends Expression {
    override def eval(l:Double, r:Double)(implicit universe: Map[String,Double]):Double = l / r
  }

  sealed trait Predicate extends Node[Boolean]

  case class LT(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):Boolean = l < r
  }
  case class LTE(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):Boolean = l <= r
  }
  case class EQ(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):Boolean = l == r
  }
  case class GTE(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):Boolean = l >= r
  }
  case class GT(left:Expression, right: Expression) extends Predicate {
    override def eval(l:Double,r:Double)(implicit universe: Map[String,Double]):Boolean = l > r
  }


  /**
   * Parse expressions and predicates containing glob-style patterns as variables (these need to be quoted).
   * Example:
   *    2 * "A.*.B" + "X.Y.*" / 4 >= 80 % * "K.*.M" is a valid predicate, which translated to the following tree:
   *
   * GTE(
   *    Plus(
   *       Multiply(
   *          Constant(2.0),
   *          Variable(&#94;A\.(.*)\.B$)
   *       ),
   *       Divide(
   *          Variable(&#94;X\.Y\.(.*)$),
   *          Constant(4.0)
   *       )
   *    ),
   *    Multiply(
   *       Constant(0.8),
   *       Variable(&#94;K\.(.*)\.M$)
   *    )
   * )
   *
   */
  class Parser extends JavaTokenParsers {

    /**
     * converts a glob-style expression into a propoer regex with escaped periods and groups around all wildcards
     * @param s glob-style path with optional wildcards
     * @return a fully anchored regex with surrounding "" removed,  periods escaped and all * surrounded with ()
     */
    private def globToRegex(s: String): Regex = s"""^${s.replace("\"", "").replace(".", "\\.").replace("*", "(.*)")}$$""".r

    def glob: Parser[Regex] = stringLiteral ^^ {globToRegex(_)}

    def expr: Parser[Expression] = term ~ (("+" | "-") ~ term).? ^^ {
      case term ~ None => term
      case left ~ Some(("+" ~ right)) => Plus(left, right)
      case left ~ Some(("-" ~ right)) => Minus(left, right)
    }

    def term: Parser[Expression] = factor ~ (("*" | "/") ~ factor).? ^^ {
      case factor ~ None => factor
      case left ~ Some(("*" ~ right)) => Multiply(left, right)
      case left ~ Some(("/" ~ right)) => Divide(left, right)
    }

    def factor: Parser[Expression] = variable | ("(" ~> expr <~ ")") | constant

    def constant: Parser[Expression] = floatingPointNumber ~ unit.? ^^ {
      case num ~ multiplier => Constant(num.toDouble * multiplier.getOrElse(1.0))
    }

    def unit: Parser[Double] = (
        "[kK][bB]?".r ^^^ {1000.0}
      | "[mM][bB]?".r ^^^ {1000 * 1000.0}
      | "[gG][bB]?".r ^^^ {1000 * 1000 * 1000.0}
      | "%" ^^^ {0.01}
    )

    def predicate: Parser[Predicate] = expr ~ ("<" | "<=" | "==" | ">=" | ">") ~ expr ^^ {
      case left ~ "<"  ~ right => LT(left, right)
      case left ~ "<=" ~ right => LTE(left, right)
      case left ~ "==" ~ right => EQ(left, right)
      case left ~ ">=" ~ right => GTE(left, right)
      case left ~ ">"  ~ right => GT(left, right)
    }

    /**
     * Does a deferred lookup in the current metrics values for the first one that matches the regex and returns its value
     * @return
     */
    def variable: Parser[Expression] = glob ^^ {
      case x => Variable(x)
    }

    def parseExpression(rules: String): ParseResult[Expression] = parseAll(expr, rules)

    def parsePredicate(rules: String): ParseResult[Predicate] = parseAll(predicate, rules)
  }

}
