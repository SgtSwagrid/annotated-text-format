package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.JsonSchema
import io.circe.{Decoder, Encoder, Json}

/**
  * A `value` attached to a text where `quote` is, as a large language model
  * writes an annotation: by quoting the text, rather than by counting its
  * characters, which models do badly. [[Anchoring]] finds where it is.
  *
  * The parts of a passage split by other words, as a separable verb is, are
  * quoted in order, joined by [[Quoted.separator]], as `"stehe … auf"`. A point
  * between two characters, as where words belong, is quoted as a few words
  * around it, with [[Quoted.caret]] at the point, as `"is ‸late"`; those words
  * are not themselves annotated. So too a passage that might be taken for the
  * same words elsewhere may be quoted with a few words around it, the passage
  * itself between [[Quoted.open]] and [[Quoted.close]], as `"the ⟦cat⟧ sat"`.
  */
final case class Quoted[+A](quote: String, value: A):

  /** It, with `transform` applied to its value. */
  def map[B](transform: A => B): Quoted[B] = copy(value = transform(value))

  /** The parts of its quote, in order. */
  def parts: List[String] = Quoted.parts(quote)

object Quoted:

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
  given [A : Encoder] => Encoder[Quoted[A]] =
    Encoder.forProduct2("quote", "value")(quoted =>
      (quoted.quote, quoted.value),
    )

  given [A : Decoder] => Decoder[Quoted[A]] =
    Decoder.forProduct2("quote", "value")(Quoted.apply[A])
