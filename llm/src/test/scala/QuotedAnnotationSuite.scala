package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.JsonSchema
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

class QuotedAnnotationSuite extends FunSuite:

  test("A quote is split into parts where an ellipsis stands between spaces"):
    assertEquals(
      QuotedAnnotation.parts("stehe … auf"),
      List("stehe", "auf"),
    )
    assertEquals(
      QuotedAnnotation.parts("stehe ... auf"),
      List("stehe", "auf"),
    )
    assertEquals(
      QuotedAnnotation.parts("Wait... what"),
      List("Wait... what"),
    )
    assertEquals(QuotedAnnotation.parts("  "), Nil)

  test("A quoted annotation is written as its quote and its value"):
    val quoted = QuotedAnnotation("stehe … auf", "get up")
    assertEquals(
      quoted.asJson,
      Json.obj(
        "quote" -> Json.fromString("stehe … auf"),
        "value" -> Json.fromString("get up"),
      ),
    )
    assertEquals(
      decode[QuotedAnnotation[String]](quoted.asJson.noSpaces),
      Right(quoted),
    )

  test("A list's schema lifts the value's definitions to its root"):
    val value = Json.obj(
      "$ref"  -> Json.fromString("#/$defs/Gloss"),
      "$defs" ->
        Json.obj("Gloss" -> Json.obj("type" -> Json.fromString("string"))),
    )
    val schema = QuotedAnnotation.listSchema(value).hcursor
    assertEquals(
      schema.downField("$schema").as[String],
      Right(JsonSchema.dialect),
    )
    assertEquals(
      schema
        .downField("items")
        .downField("properties")
        .downField("value")
        .focus,
      Some(Json.obj("$ref" -> Json.fromString("#/$defs/Gloss"))),
    )
    assert(schema.downField("$defs").downField("Gloss").succeeded)

  test("The instructions show how parts and points are quoted"):
    assert(QuotedAnnotation.instructions.contains("stehe … auf"))
    assert(QuotedAnnotation.instructions.contains("is ‸late"))
