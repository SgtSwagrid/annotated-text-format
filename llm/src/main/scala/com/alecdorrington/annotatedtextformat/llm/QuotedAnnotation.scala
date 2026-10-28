package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{AnnotatedText, JsonSchema}
import com.alecdorrington.cadmus.Scaladoc
import io.circe.{Decoder, Encoder, Json}

/**
  * An annotation of a text as a large language model writes one: by quoting the
  * passage it annotates, rather than by counting its characters, which models
  * do badly. The annotations of a text are given in the order in which their
  * passages begin.
  *
  * @param quote
  *   The passage annotated, quoted exactly as the text writes it. Where other
  *   words split the passage, as they may a separable verb, its parts are
  *   quoted in order, joined by " … ", a space either side, as in "stehe …
  *   auf". A point between two characters, as where words belong, is quoted as
  *   a word or two around it with "‸" at the point, as in "is ‸late"; those
  *   words are not themselves annotated. Where the passage could be taken for
  *   the same words elsewhere, a word or two around it are quoted too, and the
  *   passage itself is marked with "⟦" and "⟧", as in "the ⟦cat⟧ sat". A part
  *   may hold a point or a marked passage, but not both.
  *
  * @note
  *   Its value has no description here, so that a schema for it keeps its own
  *   type's. The marks in the quote's description are
  *   [[QuotedAnnotation.separator]], [[QuotedAnnotation.caret]],
  *   [[QuotedAnnotation.open]] and [[QuotedAnnotation.close]].
  */
final case class QuotedAnnotation[+A](quote: String, value: A):

  /** It, with `transform` applied to its value. */
  def map[B](transform: A => B): QuotedAnnotation[B] =
    copy(value = transform(value))

  /** The parts of its quote, in order. */
  def parts: List[String] = QuotedAnnotation.parts(quote)

object QuotedAnnotation:

  /**
    * `quoted`, anchored to `text`, as given in the order in which their
    * passages begin. Each is placed by the first of these ways that finds it:
    *   - where it first occurs from where the one before it began on, but not
    *     just where that one is, as a passage nested in it may begin where it
    *     does, but a word quoted again is a later occurrence;
    *   - else just where the one before it is, as two may annotate the same
    *     words;
    *   - else wherever it occurs only once, as a model may give one out of
    *     order, though where the search goes on from never moves back.
    *
    * Within each way, the quote is sought as written before as loosely as
    * `matching` allows, so that a correction of the case or spacing of a word
    * lands on the word as written. Each part of a quote is found after the part
    * before it, and a quote holding an ellipsis is first sought whole, as the
    * text may hold one.
    */
  def anchor[A]
    (
      text: String,
      quoted: List[QuotedAnnotation[A]],
      matching: Matching = Matching(),
    )
    : Anchored[A] = Anchoring.anchor(text, quoted, matching)

  /**
    * The annotations of `annotated`, in the order in which they begin, quoted
    * as a model would write them, each with as few words around it as anchor it
    * back to where it is, as [[anchor]] finds it: none, unless the same words
    * stand earlier where they would be found instead, and then a word or more
    * either side, the passage marked between them. Each point is quoted with
    * the word on either side of it, or more.
    *
    * An annotation that no quote within reach places back, as in a text
    * repeating the same words further than eight words either side, or one
    * holding [[caret]], [[open]] or [[close]] itself, is quoted with as many
    * words around it as may be, and may not anchor back; [[anchor]] tells which
    * do not, by where it places them.
    */
  def quote[A]
    (
      annotated: AnnotatedText[A],
      matching: Matching = Matching(),
    )
    : List[QuotedAnnotation[A]] = Anchoring.quote(annotated, matching)

  /** What joins the parts of a quote. */
  val separator: String = " … "

  /** What marks a point in a quote. */
  val caret: Char = '‸'

  /** What marks where the passage quoted among words around it begins. */
  val open: Char = '⟦'

  /** What marks where the passage quoted among words around it ends. */
  val close: Char = '⟧'

  /**
    * The parts of `quote`, split where an ellipsis, as `…` or `...`, stands
    * between spaces.
    */
  def parts(quote: String): List[String] = quote
    .split("""\s+(?:…|\.\.\.)\s+""")
    .toList
    .map(_.trim)
    .filter(_.nonEmpty)

  /**
    * The JSON Schema of a quoted annotation whose value conforms to the schema
    * `value`, within a whole schema, to which the definitions of `value`
    * belong, described by its Scaladoc.
    */
  def schema(value: Json): Json = JsonSchema.described(
    Json.obj(
      "type"       -> Json.fromString("object"),
      "properties" -> Json.obj(
        "quote" -> JsonSchema.described(
          Json.obj("type" -> Json.fromString("string")),
          doc.flatMap(_.params.get("quote")),
        ),
        "value" -> value,
      ),
      "required" -> Json.arr(
        Json.fromString("quote"),
        Json.fromString("value"),
      ),
      "additionalProperties" -> Json.False,
    ),
    doc.flatMap(_.description),
  )

  /**
    * Its Scaladoc, which describes its schema, and so tells a model how to
    * quote; but for its value, which the value's own schema describes.
    */
  private def doc = Scaladoc.of[QuotedAnnotation[Any]].map(_.plain)

  /**
    * The JSON Schema of a list of quoted annotations whose values conform to
    * the schema `value`, as a model replies with, whose definitions are lifted
    * to its root, as [[JsonSchema.around]] describes.
    */
  def listSchema(value: Json): Json = JsonSchema.around(value)(value =>
    Json.obj(
      "type"  -> Json.fromString("array"),
      "items" -> schema(value),
    ),
  )

  /** A quoted annotation as `{"quote": ..., "value": ...}`. */
  given [A : Encoder] => Encoder[QuotedAnnotation[A]] =
    Encoder.forProduct2("quote", "value")(quoted =>
      (quoted.quote, quoted.value),
    )

  given [A : Decoder] => Decoder[QuotedAnnotation[A]] =
    Decoder.forProduct2("quote", "value")(QuotedAnnotation.apply[A])
