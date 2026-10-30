package com.alecdorrington.annotatedtextformat

import io.circe.Json
import munit.FunSuite

class JsonSchemaSuite extends FunSuite:

  /** The schema of a value that refers to a definition of its own. */
  private val value = Json.obj(
    "$schema" -> Json.fromString(JsonSchema.dialect),
    "$ref"    -> Json.fromString("#/$defs/Remark"),
    "$defs"   ->
      Json.obj("Remark" -> Json.obj("type" -> Json.fromString("string"))),
  )

  private val schema = AnnotatedText.schema(value)

  private def at(json: Json, path: String*): Option[Json] = path.foldLeft(
    Option(json),
  )((json, key) => json.flatMap(_.hcursor.downField(key).focus))

  test(
    "An annotated text's schema holds the value's at each annotation's value, its definitions lifted to the root, which alone declares the dialect",
  ):
    assertEquals(
      at(
        schema,
        "properties",
        "annotations",
        "items",
        "properties",
        "value",
      ),
      Some(Json.obj("$ref" -> Json.fromString("#/$defs/Remark"))),
    )
    assertEquals(
      at(schema, "$defs", "Remark"),
      Some(Json.obj("type" -> Json.fromString("string"))),
    )
    assertEquals(
      at(schema, "$schema"),
      Some(Json.fromString(JsonSchema.dialect)),
    )
    assertEquals(
      at(
        schema,
        "properties",
        "annotations",
        "items",
        "properties",
        "value",
        "$schema",
      ),
      None,
    )

  test(
    "A value with an identity of its own keeps its definitions, and the empty schema allows any value",
  ):
    val identified =
      value.deepMerge(Json.obj("$id" -> Json.fromString("urn:remark")))
    val embedded = at(
      AnnotatedText.schema(identified),
      "properties",
      "annotations",
      "items",
      "properties",
      "value",
    )
    assertEquals(
      embedded.flatMap(at(_, "$defs", "Remark")),
      at(value, "$defs", "Remark"),
    )
    assertEquals(
      at(
        AnnotatedText.schema(Json.obj()),
        "properties",
        "annotations",
        "items",
        "properties",
        "value",
      ),
      Some(Json.obj()),
    )
    assertEquals(
      at(
        AnnotatedText.schema(Json.obj()),
        "$defs",
      ),
      None,
    )

  test("Schemas are described by the Scaladoc of what they describe"):
    val span = TextSpan.schema.hcursor
    assert(
      span.get[String]("description").exists(_.startsWith("A part of a text")),
    )
    assert(
      span
        .downField("properties")
        .downField("start")
        .get[String]("description")
        .exists(_.contains("UTF-16")),
    )
    val text = AnnotatedText.schema(Json.obj()).hcursor
    assert(
      text
        .get[String]("description")
        .exists(_.startsWith("A text with annotations")),
    )
    assert(text.get[String]("description").forall(!_.contains("`")))
