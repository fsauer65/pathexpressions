import pathexpressions.Expressions._

//
// testing the expression parser
//
// this is a simulated symbol table
// universe MUST have values for all defined variables in the below expressions and predicate
// AND the keys must all have the same wildcard value (foo)
implicit val universe = Map[String,Double] (
  "A.foo.B" -> 2.0,
  "X.Y.bar" -> 16.0,
  "X.Y.foo" -> 12.0,
  "K.foo.M" -> 7.0
)

val missingVar = Map[String,Double] (
  "A.foo.B" -> 2.0,
  "X.Y.bar" -> 16.0,
  "K.foo.M" -> 7.0
)


// test expressions
val constant = "10 %"
val c = Parser.parseExpression(constant).get
val cval = c.value
val variable = """"A.*.B""""
val vp = Parser.parseExpression(variable).get
val resolvedVariable = vp.value
val expression = """
                  |
                  | 2 * "A.*.B" / 1 + ("X.Y.*" / 4) - "K.*.M"
                  |
                 """.stripMargin
val x = Parser.parseExpression(expression).get
val evaluatedExpression = x.value
val xWithoutXY = x.value(missingVar)
// test predicates
val pred = """
            |
            | 2 * "A.*.B" + "X.Y.*" / 4 == "K.*.M"
            |
           """.stripMargin
val predicate = Parser.parsePredicate(pred).get
val evaluatedPredicate = predicate.value
val pWithoutXY = predicate.value(missingVar)
