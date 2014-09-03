Path-Expression parser and evaluator
====================================

Path expressions are regular algebraic expressions and predicates in which the variables are a form of regular expressions themselves.
They are glob-style dot-separated paths with wildcards like used in graphite metrics. The expressions parsed and evaluated by this code
only yield a result when:

1. all variables (paths) resolve to a value
2. all wildcards used in the matching paths match to the same value.

Example:

Given the following variables:

    implicit val symbolTable = Map[String,Double] (
       "A.foo.B" -> 2.0,
       "X.Y.bar" -> 16.0,
       "X.Y.foo" -> 12.0,
       "K.foo.M" -> 7.0
    }

The following expression evaluates to `Some(0.0)`:

    2 * "A.*.B" / 1 + ("X.Y.*" / 4) - "K.*.M"

Because all paths exists in the symbol Table and all wildcards evaluate to the same value (foo)

In predicate form:

    2 * "A.*.B" + "X.Y.*" / 4 >= "K.*.M"

it evaluates to `Some(true)`

If however we remove X.Y.foo from the symbol table, both the expression and predicate evaluate to `None`,
because even though there still is an `X.Y.bar` which matches `"X.Y.*"` but the other paths don't exist with
bar as a value for their wildcards.


The grammar is a standard expression grammer based on the one in the Programming in Scala book's chapter on parser combinators, but with added
predicates and obviously the paths variables. It also has some units defined to write things like `10 MB` or `50 %` as constant values.

The above predicate translates to this AST:

    GTE(
       Plus(
          Multiply(
             Constant(2.0),
             Variable(&#94;A\.(.*)\.B$)
          ),
          Divide(
             Variable(&#94;X\.Y\.(.*)$),
             Constant(4.0)
          )
       ),
       Multiply(
          Constant(0.8),
          Variable(&#94;K\.(.*)\.M$)
       )
    )

To parse an expression of predicate:

    val parser = new Expressions.Parser

This returns a standard ParseResult. Ignoring the error handling for a second, to evaluate the result:

    val predicate = parser.parsePredicate("""2 * "A.*.B" + "X.Y.*" / 4 >= "K.*.M"""").get
    predicate.value // value method expects an implicit Map[String,Double) as symbolTable


Same for expressions:

    val expression = parser.parseExpression("""2 * "A.*.B" / 1 + ("X.Y.*" / 4) - "K.*.M""""").get
    expression.value // value method expects an implicit Map[String,Double) as symbolTable


The parsertests.sc file in the test directory is an IntelliJ Scala IDE worksheet. This code is in need of formal automated tests, pull requests welcome!