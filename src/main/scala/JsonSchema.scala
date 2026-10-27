package com.alecdorrington.annotatedtextformat

import io.circe.{Json, JsonObject}

/**
  * How the format's JSON Schemas are put together around the schema of an
  * annotation's value, which JSON Schema, having no type parameters, cannot
  * leave open.
  */
object JsonSchema:

  /** The dialect of JSON Schema in which the schemas are written. */
  val dialect: String = "https://json-schema.org/draft/2020-12/schema"

  /**
    * A whole schema, declaring its [[dialect]], made by `wrap` around the
    * schema `value`. The definitions of `value`, under `$defs`, are lifted to
    * the root, so that its references to them, as `#/$defs/name`, still
    * resolve; unless `value` has an `$id`, against which they resolve already.
    */
  def around(value: Json)(wrap: Json => Json): Json =
    val (definitions, rest) = lifted(value)
    Json
      .obj("$schema" -> Json.fromString(dialect))
      .deepMerge(wrap(rest))
      .deepMerge(definitions.fold(Json.obj())(defs =>
        Json.obj("$defs" -> defs),
      ))

  /** The definitions of `value`, if they may be lifted, and the rest of it. */
  private def lifted(value: Json): (Option[Json], Json) = value.asObject match
    case Some(schema) if !schema.contains("$id") =>
      (schema("$defs"), Json.fromJsonObject(bare(schema).remove("$defs")))
    case Some(schema) => (None, Json.fromJsonObject(bare(schema)))
    case None         => (None, value)

  /** `schema`, without the dialect it declares, which only a root may. */
  private def bare(schema: JsonObject): JsonObject = schema.remove("$schema")
