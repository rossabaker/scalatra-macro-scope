package macrospike

import concurrent.Future

object Demo extends App {
  object OldScalatraApp extends OldScalatra {
    get("/dummy") {
      Future {
        println(s"old request  = ${request}")
      }
    }
  }

  object NewScalatraApp extends NewScalatra {
    def ct = request.contentType

    get("/dummy") {
      def foo = request.hashCode()

      Future {
        println(s"new request  = ${request}")
        println(s"content type = ${contentType}")
      }
    }
  }

  println("Old")
  OldScalatraApp.handle(new Request("html"), new Response)
  println("New")
  NewScalatraApp.handle(new Request("html"), new Response)
}




