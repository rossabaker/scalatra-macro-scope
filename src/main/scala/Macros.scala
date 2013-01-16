import reflect.macros.Context

/**
 * @author ross
 */
object Macros {
  // TODO move back into trait
  var route: (Request, Response) => Any = _

  def requestImpl(c: Context): c.Expr[Request] = {
    import c.universe._
    c.warning(c.macroApplication.pos, "Invalid request access")
    reify { Macros.dummyRequest }
  }

  val dummyRequest: Request = null

  def getImpl(c: Context)(path: c.Expr[String])(action: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._

    val RequestTerm = stringToTermName("dummyRequest")
    val ResponseTerm = stringToTermName("_dummyResponse")

    val transformer = new Transformer {
      override def transform(tree: c.Tree) = tree match {
        // TODO make sure first part of this comes from "this"
        case s @ Select(_, RequestTerm) => Ident(newTermName("request"))
        case t => super.transform(t)
      }
    }
    def rewrite(tree: Tree) = c.Expr[Any](c.resetAllAttrs(transformer.transform(tree)))

    val newExpr = reify { route = {
      (request: Request, response: Response) =>
        implicit val implicitRequest = request
        implicit val implicitResponse = response
        rewrite(action.tree).splice
    }}
//    println(showRaw(newExpr.tree))
    newExpr
  }
}
