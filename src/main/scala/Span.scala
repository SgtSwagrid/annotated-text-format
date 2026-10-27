package com.alecdorrington.annotatedtextformat

import io.circe.{Decoder, Encoder, Json}

/**
  * A part of a text, from `start` up to but excluding `end`, counted in UTF-16
  * code units, as both the JVM's and JavaScript's strings count them, and the
  * DOM's ranges too. Where they are equal, it is a point between two
  * characters, as where text is to be inserted.
  */
final case class Span(start: Int, end: Int):

  /** The number of characters within it. */
  def length: Int = end - start

  /** Whether it is a point, holding no characters. */
  def isPoint: Boolean = start == end

  /** Its part of `text`. */
  def in(text: String): String = text.substring(start, end)

  /** Whether the character `at` lies within it. */
  def contains(at: Int): Boolean = start <= at && at < end

  /** Whether `other` lies within it, as a point at either of its ends does. */
  def covers(other: Span): Boolean = start <= other.start && other.end <= end

  /** Whether it shares a character with `other`, which a point never does. */
  def overlaps(other: Span): Boolean = !isPoint && !other.isPoint &&
    start < other.end && other.start < end

  /** How many characters lie between it and `other`. */
  def distance(other: Span): Int = (other.start - end)
    .max(start - other.end)
    .max(0)

  /** It, moved `by` characters on. */
  def shift(by: Int): Span = Span(start + by, end + by)

  /** Whether it lies within a text of `length` characters, the right way round. */
  def fits(length: Int): Boolean = 0 <= start && start <= end && end <= length

object Span:

  /** The point at `at`. */
  def point(at: Int): Span = Span(at, at)

  /** Spans in the order in which they begin, the shorter first. */
  given Ordering[Span] = Ordering.by(span => (span.start, span.end))

  /** A span as `{"start": 4, "end": 9}`. */
  given Encoder[Span] =
    Encoder.forProduct2("start", "end")(span => (span.start, span.end))

  given Decoder[Span] = Decoder.forProduct2("start", "end")(Span.apply)

  /** The JSON Schema of a span. */
  val schema: Json = Json.obj(
    "type"        -> Json.fromString("object"),
    "description" -> Json.fromString(
      "A part of the text, from start up to but excluding end, counted in UTF-16 code units; a point between two characters where they are equal.",
    ),
    "properties" -> Json.obj(
      "start" -> offset("Where the span begins."),
      "end"   -> offset("Where the span ends."),
    ),
    "required" -> Json.arr(
      Json.fromString("start"),
      Json.fromString("end"),
    ),
    "additionalProperties" -> Json.False,
  )

  private def offset(description: String): Json = Json.obj(
    "type"        -> Json.fromString("integer"),
    "minimum"     -> Json.fromInt(0),
    "description" -> Json.fromString(description),
  )
