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
    "An annotated text's schema holds the value's at each annotation's value",
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

  test(
    "The value's definitions are lifted to the root, where its references resolve",
  ):
    assertEquals(
      at(schema, "$defs", "Remark"),
      Some(Json.obj("type" -> Json.fromString("string"))),
    )

  test("Only the root declares the dialect"):
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

  test("A value with an identity of its own keeps its definitions"):
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

  test("Any value is allowed by the empty schema"):
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
