package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{AnnotatedText, JsonSchema}
import io.circe.{Decoder, Encoder, Json}

/**
  * An annotation of a text as a large language model writes one: by quoting the
  * passage it annotates, rather than by counting its characters, which models
  * do badly.
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
  * @param value
  *   What the passage is annotated with.
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
    * What to tell a model about quoting, so that its quotes can be anchored to
    * the text.
    */
  val instructions: String =
    s"""Quote each passage that you annotate exactly as the text writes it, never by counting its characters, and give the annotations in the order in which their passages begin.
       |Where other words split a passage, as they may a separable verb, quote its parts in order, joined by "${ separator
        .trim }" with a space either side, as in "stehe${ separator }auf".
       |To mark a point between two characters, as where words belong, quote a word or two around it with "$caret" at the point, as in "is ${ caret }late"; those words are not themselves annotated.
       |Where a passage could be taken for the same words elsewhere, quote a word or two around it too, and mark the passage itself with "$open" and "$close", as in "the ${ open }cat$close sat". A part may hold a point or a marked passage, but not both."""
      .stripMargin

  /**
    * The JSON Schema of a quoted annotation whose value conforms to the schema
    * `value`, within a whole schema, to which the definitions of `value`
    * belong.
    */
  def schema(value: Json): Json = Json.obj(
    "type"       -> Json.fromString("object"),
    "properties" -> Json.obj(
      "quote" -> Json.obj(
        "type"        -> Json.fromString("string"),
        "description" -> Json.fromString(
          s"The passage annotated, quoted exactly as the text writes it, its parts joined by \"${ separator
              .trim }\", a point marked by \"$caret\", and a passage quoted among words around it marked by \"$open\" and \"$close\".",
        ),
      ),
      "value" -> value,
    ),
    "required" -> Json.arr(
      Json.fromString("quote"),
      Json.fromString("value"),
    ),
    "additionalProperties" -> Json.False,
  )

  /**
    * The JSON Schema of a list of quoted annotations whose values conform to
    * the schema `value`, as a model replies with, whose definitions are lifted
    * to its root, as [[JsonSchema.around]] describes.
    */
  def listSchema(value: Json): Json = JsonSchema.around(value)(value =>
    Json.obj(
      "type"        -> Json.fromString("array"),
      "description" -> Json.fromString(
        "Annotations of the text, in the order in which their passages begin.",
      ),
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
