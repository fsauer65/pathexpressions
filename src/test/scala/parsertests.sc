import pathexpressions.Expressions

//
// testing the expression parser
//
// this is a simulated symbol table
// universe MUST have values for all defined variables in the below expressions and predicate
// AND the keys must all have the same wildcard value (foo)
val universe = Map[String,Double] (
  "A.foo.B" -> 2.0,
  "X.Y.bar" -> 16.0,
  "X.Y.foo" -> 12.0,
  "K.foo.M" -> 7.0
)

implicit class XEvaluator(x:Expressions.Expression) {
  def value = x.eval(x.resolve(universe))
}
implicit class PEvaluator(x:Expressions.Predicate) {
  def value = x.eval(x.resolve(universe))
}

// test expressions
val parser = new Expressions.Parser
val constant = "10 %"
val c = parser.parseExpression(constant).get
val cval = c.value
val variable = """"A.*.B""""
val vp = parser.parseExpression(variable).get
val resolvedVariable = vp.value
val expression = """
                  |
                  | 2 * "A.*.B" / 1 + ("X.Y.*" / 4) - "K.*.M"
                  |
                 """.stripMargin
val x = parser.parseExpression(expression).get
val evaluatedExpression = x.value
// test predicates
val pred = """
            |
            | 2 * "A.*.B" + "X.Y.*" / 4 == "K.*.M"
            |
           """.stripMargin
val predicate = parser.parsePredicate(pred).get
val evaluatedPredicate = predicate.value

