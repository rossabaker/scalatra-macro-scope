package macrospike

import annotation.implicitNotFound
import reflect.macros.Context
import scala.language.experimental.macros

import concurrent.{ExecutionContext, Future}
import scala.util.DynamicVariable




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
  protected implicit def request: Request = macro Macros.requestImpl

  def get(path: String)(action: Any) = macro Macros.addRouteImpl

  def contentType(implicit request: Request) = request.contentType

  def handle(req: Request, res: Response) {
    Macros.route(req, res)
  }
}