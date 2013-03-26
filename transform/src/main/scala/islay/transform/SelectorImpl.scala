package islay.transform

import scala.reflect.macros.Context

import org.parboiled.errors.{DefaultInvalidInputErrorFormatter, InvalidInputError}
import org.parboiled.scala.parserunners.{BasicParseRunner, ReportingParseRunner}

import islay.transform.parser.SelectorParser


object SelectorImpl {

  def cImpl(c: Context)(): c.Expr[Selector] = {
    import c.universe._
    import c.mirror.staticPackage

    val Apply(_, List(
      Apply(_, List(
        Literal(Constant(expression: String))
      ))
    )) = c.prefix.tree

    val result = ReportingParseRunner(SelectorParser.selector).run(expression)
    result.result match {

      case None =>
        result.parseErrors foreach {
          case error: InvalidInputError =>
            val formatter = new DefaultInvalidInputErrorFormatter
            val message = formatter.format(error).replaceFirst("Invalid input", "Invalid selector syntax at")
            c.error(c.enclosingPosition, message)
          case _ =>
            c.error(c.enclosingPosition, "whatever")
        }
        c.Expr[Selector](Literal(Constant(null)))

      case Some(r) =>

        reify {
          BasicParseRunner(SelectorParser.selector).run(c.Expr[String](Literal(Constant(expression))).splice).result.get
        }
        /*
        c.Expr(
          Apply(
            Select(
              Select(
                Select(
                  build.Ident(staticPackage("islay")),
                  newTermName("transform")
                ),
                newTermName("Selector")
              ),
              newTermName("apply")
            ),
            List(
              Select(
                build.This(
                  staticPackage("scala.collection.immutable").asModule.moduleClass
                ),
                newTermName("Nil")
              )
            )
          )
        )
        */
    }
  }
}