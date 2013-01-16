import concurrent.Future

object Demo extends App {
  object OldScalatraApp extends OldScalatra {
    get("/dummy") {
//      Future {
        println(s"old request  = ${request}")
//      }
    }
  }

  object NewScalatraApp extends NewScalatra {
    get("/dummy") {
//      Future {
        println(s"new request  = ${request}")
//      }
    }
  }

  println("Old")
  OldScalatraApp.handle(new Request("html"), new Response)
  println("New")
  NewScalatraApp.handle(new Request("html"), new Response)

  Thread.sleep(1000)
}




