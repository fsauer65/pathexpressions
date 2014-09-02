import pathexpressions.{Expressions, Matcher}
//
// testing the variable matcher
//
val matcher = Matcher("messaging.queues.*.spooled.*", "messaging.queues.*.quota.*")
matcher("messaging.queues.q1.spooled.boo")
matcher("system.cpu.usr")
matcher("messaging.queues.q2.spooled.bar")
matcher("messaging.queues.q2.quota.bar")
// this one returns empty because one of the paths does not match any pattern
matcher(List("messaging.queues.q1.spooled.bar", "messaging.queues.q2.spooled.bar", "messaging.queues.q1.quota.bar"))
// this one returns two matches becuase ALL paths find a match and they all have the same wildcards
matcher(List("messaging.queues.q1.spooled.bar", "messaging.queues.q1.quota.bar"))
// this one returns empty because ALL paths find a match but they have the different wildcards
matcher(List("messaging.queues.q1.spooled.bar", "messaging.queues.q1.quota.foo"))
// without wildcards they match as long as all patterns don't have wildcards
val m2 = Matcher("test.foo.bar", "test.bar.foo")
m2("test.foo.bar")
m2(List("test.foo.bar", "test.bar.foo"))
val m3 = Matcher("test.foo.bar", "test.*.bar")
m3("test.foo.bar")
m3(List("test.foo.bar", "test.bar.foo"))
val m4 = Matcher("test.*.bar")
m4("test.foo.bar")
m4(List("test.foo.bar"))
//
// testing the expression parser
//

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

// this universe is a simulated metrics cache with the current value of each metric

//predicate.get.resolve
predicate.get.eval(predicate.get.resolve)


