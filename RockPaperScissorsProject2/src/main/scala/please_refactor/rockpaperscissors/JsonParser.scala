package please_refactor.rockpaperscissors

  import fpinscala.parsing.JSON.{JArray, JNumber, JObject, JString}
  import fpinscala.parsing.ReferenceTypes.Parser
  import fpinscala.parsing.{JSON, Location, ParseError}
  import scala.io.Source

  object JsonParser {

    def go(jsonFileName: String) = {

      val filename = jsonFileName
      val jsonTxt = Source.fromFile(filename).getLines.mkString

      Source.fromFile(filename).close()

      val P = fpinscala.parsing.Reference

      import fpinscala.parsing.ReferenceTypes.Parser

      val json: Parser[JSON] = JSON.jsonParser(P)
      val resultOfParsing = P.run(json)(jsonTxt) // this parses JSON input into a JSON object
      resultOfParsing.flatMap(j => betterUnpackUsingForComprehension2(j)) match{
        case Right(x)=>x
      }
    }

    case class SampleDTO(
                          tournaments: Double,
                          roundsPerMatch: Double,
                          players: List[String],
                          types: List[String],
                          weights: List[Double]
                        )

    def betterUnpackUsingForComprehension2(json: JSON): Either[ParseError, SampleDTO] = {
      val res = {
        json match {
          case jObject: JObject =>
            for {
              tournaments <- unpackNumber(jObject, "tournaments")
              roundsPerMatch <- unpackNumber(jObject, "roundsPerMatch")
              players <- unpackArray(jObject, "players")
              types <- unpackArray(jObject, "types")
              weights <- unpackArrayNumbers(jObject, "weights")

            } yield SampleDTO(tournaments, roundsPerMatch, players, types, weights)

          case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))

        }
      }
      res
    }

    def unpackArray(jObject: JObject, key: String): Either[ParseError, List[String]] = {
      for {
        relatedPacked <- jObject.get(key) match {
          case jArray: JArray => Right(jArray.get)
          case _ => Left(ParseError(List((Location("Could not unpack related"), "related"))))
        }
        related <- unpackList(relatedPacked.toList, Right(List.empty))
      } yield related
    }

    def unpackArrayNumbers(jObject: JObject, key: String): Either[ParseError, List[Double]] = {
      for {
        relatedPacked <- jObject.get(key) match {
          case jArray: JArray => Right(jArray.get)
          case _ => Left(ParseError(List((Location("Could not unpack related"), "related"))))
        }
        related <- unpackListNumber(relatedPacked.toList, Right(List.empty))
      } yield related
    }

    def unpackList(c: List[JSON], r: Either[ParseError, List[String]]): Either[ParseError, List[String]] =
      c match {
        case ::(head, next) => head match {
          case JString(v) => unpackList(next, r.flatMap(list => Right(v :: list)))
          case p: ParseError => Left(p)
        }
        case Nil => r
      }

    def unpackListNumber(c: List[JSON], r: Either[ParseError, List[Double]]): Either[ParseError, List[Double]] =
      c match {
        case ::(head, next) => head match {
          case JNumber(v) => unpackListNumber(next, r.flatMap(list => Right(v :: list)))
          case p: ParseError => Left(p)
        }
        case Nil => r
      }

    def unpackNumber(jObject: JObject, key: String): Either[ParseError, Double] = jObject.get(key) match {
      case jNumber: JNumber => Right(jNumber.get)
      case _ => Left(ParseError(List((Location("Could not unpack ticker"), "tournaments"))))
    }

}
