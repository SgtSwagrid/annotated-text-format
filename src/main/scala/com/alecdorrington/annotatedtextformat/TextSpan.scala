package com.alecdorrington.annotatedtextformat

import com.alecdorrington.cadmus.Scaladoc
import io.circe.{Decoder, Encoder, Json}

/**
  * A part of a text, from its start up to but excluding its end, counted in
  * UTF-16 code units, as JavaScript's strings count them. Where they are equal,
  * it is a point between two characters, as where text is to be inserted.
  *
  * @param start
  *   Where it begins: how many UTF-16 code units of the text come before it.
  * @param end
  *   Where it ends: how many UTF-16 code units of the text come before its end.
  *
  * @note
  *   The JVM's strings and the DOM's ranges count alike.
  */
final case class TextSpan(start: Int, end: Int):

  /** The number of characters within it. */
  def length: Int = end - start

  /** Whether it is a point, holding no characters. */
  def isPoint: Boolean = start == end

  /** Its part of `text`. */
  def in(text: String): String = text.substring(start, end)

  /** Whether the character `at` lies within it. */
  def contains(at: Int): Boolean = start <= at && at < end

  /** Whether `other` lies within it, as a point at either of its ends does. */
  def covers(other: TextSpan): Boolean = start <= other.start &&
    other.end <= end

  /** Whether it shares a character with `other`, which a point never does. */
  def overlaps(other: TextSpan): Boolean = !isPoint && !other.isPoint &&
    start < other.end && other.start < end

  /** How many characters lie between it and `other`. */
  def distance(other: TextSpan): Int = (other.start - end)
    .max(start - other.end)
    .max(0)

  /** It, moved `by` characters on. */
  def shift(by: Int): TextSpan = TextSpan(start + by, end + by)

  /** Whether it lies within a text of `length` characters, the right way round. */
  def fits(length: Int): Boolean = 0 <= start && start <= end && end <= length

object TextSpan:

  /** The point at `at`. */
  def point(at: Int): TextSpan = TextSpan(at, at)

  /** Spans in the order in which they begin, the shorter first. */
  given Ordering[TextSpan] = Ordering.by(span => (span.start, span.end))

  /** A span as `{"start": 4, "end": 9}`. */
  given Encoder[TextSpan] =
    Encoder.forProduct2("start", "end")(span => (span.start, span.end))

  given Decoder[TextSpan] = Decoder.forProduct2("start", "end")(TextSpan.apply)

  /** Its Scaladoc, which describes its schema. */
  private val doc = Scaladoc.of[TextSpan].map(_.plain)

  /** The JSON Schema of a span, described by its Scaladoc. */
  val schema: Json = JsonSchema.described(
    Json.obj(
      "type"       -> Json.fromString("object"),
      "properties" -> Json.obj(
        "start" -> offset(doc.flatMap(_.params.get("start"))),
        "end"   -> offset(doc.flatMap(_.params.get("end"))),
      ),
      "required" -> Json.arr(
        Json.fromString("start"),
        Json.fromString("end"),
      ),
      "additionalProperties" -> Json.False,
    ),
    doc.flatMap(_.description),
  )

  private def offset(description: Option[String]): Json = JsonSchema.described(
    Json.obj(
      "type"    -> Json.fromString("integer"),
      "minimum" -> Json.fromInt(0),
    ),
    description,
  )
