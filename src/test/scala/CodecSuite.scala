package com.alecdorrington.annotatedtextformat

import io.circe.{Decoder, Encoder, Json}
import io.circe.parser.{decode, parse}
import io.circe.syntax.*
import munit.FunSuite

class CodecSuite extends FunSuite:

  /** A value as an application might attach: a correction, or praise. */
  private enum Remark:

    case Fix(correction: String)
    case Praise

  private given Encoder[Remark] = Encoder.instance:
    case Remark.Fix(correction) =>
      Json.obj("fix" -> Json.fromString(correction))
    case Remark.Praise => Json.fromString("praise")

  private given Decoder[Remark] = Decoder[String]
    .emap(name => Either.cond(name == "praise", Remark.Praise, name))
    .or(Decoder.forProduct1("fix")(Remark.Fix.apply))

  private val annotated = AnnotatedText
    .of(
      "She always is late.",
      List(
        Annotation(
          List(Span(4, 10), Span.point(14)),
          Remark.Fix("She is always late."),
        ),
        Annotation(Span(0, 3), Remark.Praise),
      ),
    )
    .toOption
    .get

  private val json = parse("""{
      |  "text": "She always is late.",
      |  "annotations": [
      |    {"spans": [{"start": 4, "end": 10}, {"start": 14, "end": 14}], "value": {"fix": "She is always late."}},
      |    {"spans": [{"start": 0, "end": 3}], "value": "praise"}
      |  ]
      |}""".stripMargin).toOption.get

  test(
    "An annotated text is written as its text and its annotations' spans and values",
  ):
    assertEquals(annotated.asJson, json)

  test("An annotated text is read back as it was written"):
    assertEquals(
      json.as[AnnotatedText[Remark]],
      Right(annotated),
    )

  test("An annotated text whose annotations do not fit its text is refused"):
    val read = decode[AnnotatedText[String]](
      """{"text": "Short.", "annotations": [{"spans": [{"start": 2, "end": 40}], "value": "x"}]}""",
    )
    assertEquals(
      read.left.map(_.getMessage),
      Left(
        "DecodingFailure at : Annotation 0 has a span, from 2 to 40, outside the text.",
      ),
    )
