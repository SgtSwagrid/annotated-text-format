package com.alecdorrington.annotatedtextformat

import io.circe.{Decoder, Encoder, Json}

/**
  * A `value` attached to the `spans` of a text: one, as a word, or several, as
  * the parts of a separable verb, or a span of words and the point to which
  * they move. In an [[AnnotatedText]], it has at least one span, its spans are
  * in order, and none overlaps another.
  *
  * @tparam A
  *   The type of the value, specific to the application, such as a correction
  *   or a translation.
  */
final case class Annotation[+A](spans: List[Span], value: A):

  /** It, with `transform` applied to its value. */
  def map[B](transform: A => B): Annotation[B] = copy(value = transform(value))

  /** The span from where its first span begins to where its last ends. */
  def extent: Span = Span(spans.head.start, spans.last.end)

  /** Its parts of `text`, one for each of its spans. */
  def in(text: String): List[String] = spans.map(_.in(text))

  /** Whether any of its spans shares a character with any of `other`'s. */
  def overlaps(other: Annotation[?]): Boolean =
    spans.exists(span => other.spans.exists(span.overlaps))

  /** Its spans that are not points. */
  def stretches: List[Span] = spans.filterNot(_.isPoint)

  /** Its spans that are points. */
  def points: List[Span] = spans.filter(_.isPoint)

object Annotation:

  /** An annotation of the one span `span`. */
  def apply[A](span: Span, value: A): Annotation[A] =
    Annotation(List(span), value)

  /**
    * The JSON Schema of an annotation whose value conforms to the schema
    * `value`, within a whole schema, to which the definitions of `value`
    * belong.
    */
  def schema(value: Json): Json = Json.obj(
    "type"       -> Json.fromString("object"),
    "properties" -> Json.obj(
      "spans" -> Json.obj(
        "type"        -> Json.fromString("array"),
        "description" -> Json.fromString(
          "The spans to which the value is attached, in order, none overlapping another.",
        ),
        "minItems" -> Json.fromInt(1),
        "items"    -> Span.schema,
      ),
      "value" -> value,
    ),
    "required" -> Json.arr(
      Json.fromString("spans"),
      Json.fromString("value"),
    ),
    "additionalProperties" -> Json.False,
  )

  /** An annotation as `{"spans": [...], "value": ...}`. */
  given [A : Encoder] => Encoder[Annotation[A]] =
    Encoder.forProduct2("spans", "value")(annotation =>
      (annotation.spans, annotation.value),
    )

  given [A : Decoder] => Decoder[Annotation[A]] =
    Decoder.forProduct2("spans", "value")((spans: List[Span], value: A) =>
      Annotation(spans, value),
    )
