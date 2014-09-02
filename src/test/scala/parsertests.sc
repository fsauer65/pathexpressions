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
// test expressions
val parser = new Expressions.Parser
val expression = """
                  |
                  | 2 * "A.*.B" + ("X.Y.*" / 4)
                  |
                 """.stripMargin
val x = parser.parseExpression(expression).get
x.eval(x.resolve(universe))
// test predicates
val pred = """
            |
            | 2 * "A.*.B" + "X.Y.*" / 4 == "K.*.M"
            |
           """.stripMargin
val predicate = parser.parsePredicate(pred).get

predicate.eval(predicate.resolve(universe))


