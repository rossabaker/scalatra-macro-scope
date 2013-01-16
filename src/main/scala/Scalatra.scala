import reflect.macros.Context
import scala.language.experimental.macros

import concurrent.{ExecutionContext, Future}
import scala.util.DynamicVariable

case class Request(contentType: String)
class Response

trait Scalatra {
  implicit protected val executor = ExecutionContext.Implicits.global
  def handle(request: Request, response: Response): Unit
}

trait OldScalatra extends Scalatra {
  private val _request: DynamicVariable[Request] = new DynamicVariable[Request](null)
  private val _response: DynamicVariable[Response] = new DynamicVariable[Response](null)

  protected def request = _request.value
  protected def response = _response.value

  private var route: () => Any = _

  def get(path: String)(action: Any) = route = () => action

  def contentType = request

  def handle(request: Request, response: Response) {
    _request.withValue(request) {
      _response.withValue(response) {
        route()
      }
    }
  }
}

trait NewScalatra extends Scalatra {
  protected implicit def request:  Request  = ???
  protected implicit def response: Response = ???

  def get(path: String)(action: Any) = macro NewScalatra.getImpl

  def contentTypeUnsafe = request

  def contentTypeSafe(implicit request: Request) = request.contentType

  def handle(req: Request, res: Response) {
    NewScalatra.route(req, res)
  }
}

object NewScalatra {
  // TODO move back into trait
  var route: (Request, Response) => Any = _

  def getImpl(c: Context)(path: c.Expr[String])(action: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._

    val RequestTerm = stringToTermName("request")
    val ResponseTerm = stringToTermName("response")

    val transformer = new Transformer {
      override def transform(tree: c.Tree) = tree match {
        // TODO make sure first part of this comes from "this"
        case s @ Select(_, RequestTerm) => Ident(newTermName("request"))
        case s @ Select(_, ResponseTerm) => Ident(newTermName("response"))
        case t => super.transform(t)
      }
    }
    def rewrite(tree: Tree) = c.Expr[Any](c.resetLocalAttrs(transformer.transform(tree)))

    val newExpr = reify { route = {
      (request: Request, response: Response) => rewrite(action.tree).splice
    }}
    println("NEW EXPR = "+showRaw(newExpr.tree))
    newExpr
  }
}