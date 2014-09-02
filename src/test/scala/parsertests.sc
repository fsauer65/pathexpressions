import pathexpressions.Expressions

//
// testing the expression parser
//
// this universe is a simulated symbol table
// universe MUST have values for all defined variables in the above predicate AND
// the keys must all have the same wildcard value (foo)
implicit val universe = Map[String,Double] (
  "A.foo.B" -> 2.0,
  "X.Y.bar" -> 16.0,
  "X.Y.foo" -> 12.0,
  "K.foo.M" -> 7.0
)
// expressions
val parser = new Expressions.Parser
val expression ="""
      | 2 * "A.*.B" + ("X.Y.*" / 4)
  """.stripMargin
val x = parser.parseExpression(expression)
x.get.eval(x.get.resolve)
// predicates
val pred ="""
      | 2 * "A.*.B" + "X.Y.*" / 4 == "K.*.M"
  """.stripMargin
val predicate = parser.parsePredicate(pred)

//predicate.get.resolve
predicate.get.eval(predicate.get.resolve)


