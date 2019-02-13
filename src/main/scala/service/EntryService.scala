package service

import cats.effect.IO
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl

class EntryService() extends Http4sDsl[IO] {

  val service = HttpService[IO] {
    case POST -> Root / "entry" =>
      for {

      }
  }

}
