package macrospike

import reflect.macros.Context

/**
 * @author ross
 */
object Macros {
  // TODO move back into trait
  var route: (Request, Response) => Any = _

  def requestImpl(c: Context): c.Expr[Request] = {
    import c.universe._
    new Traverser {
      var inRoute = false

      val Get = stringToTermName("get")
      val Post = stringToTermName("post")

      override def traverse(tree: c.Tree) {
        tree match {
          case Apply(Apply(Ident(Get | Post), _), trees) =>
            inRoute = true
            super.traverseTrees(trees)
            inRoute = false

          case t if t.pos == c.macroApplication.pos =>
            if (inRoute == false)
              c.error(t.pos, "Invalid request access")

          case t => super.traverse(t)
        }
      }
    }.traverse(c.enclosingClass)
    reify { Request.verifiedAccess }
  }

  val dummyRequest: Request = null

  def addRouteImpl(c: Context)(path: c.Expr[String])(action: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._

    val verifiedRequest = (reify { Request.verifiedAccess }).tree

    val transformer = new Transformer {
      val RequestTerm = newTermName("Request")
      val VerifiedAccess = newTermName("verifiedAccess")

      override def transform(tree: c.Tree) = tree match {
        // TODO make sure first part of this comes from "this"
        case Select(Ident(RequestTerm), VerifiedAccess) => Ident(newTermName("request"))
        case t => super.transform(t)
      }
    }
    def rewrite(tree: Tree) = c.Expr[Any](c.resetAllAttrs(transformer.transform(tree)))

    val newExpr = reify { route = {
      (request: Request, response: Response) => rewrite(action.tree).splice
    }}
//    println(showRaw(newExpr.tree))
    newExpr
  }
}

case class Request(contentType: String)

object Request {
  def verifiedAccess: Request = ???
}

class Response

