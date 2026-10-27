package com.alecdorrington.annotatedtextformat

import io.circe.{Decoder, Encoder, Json}

/**
  * A `text` with `annotations`, each attaching a value to some of its spans.
  * Annotations may overlap and nest one another, but each has at least one
  * span, and its own spans lie within the text, in order, none overlapping
  * another. [[AnnotatedText.of]] makes one, checking that this holds.
  *
  * @tparam A
  *   The type of the annotations' values, specific to the application, such as
  *   a correction or a translation.
  */
final case class AnnotatedText[+A] private[annotatedtextformat] (
  text: String,
  annotations: List[Annotation[A]],
):

  /** It, with `transform` applied to each annotation's value. */
  def map[B](transform: A => B): AnnotatedText[B] =
    copy(annotations = annotations.map(_.map(transform)))

  /** It, with only the annotations whose values `keep` holds for. */
  def filter(keep: A => Boolean): AnnotatedText[A] =
    copy(annotations = annotations.filter(annotation => keep(annotation.value)))

  /** It, with only the annotations whose values `pick` takes, as it takes them. */
  def collect[B](pick: PartialFunction[A, B]): AnnotatedText[B] =
    copy(annotations = annotations.collect {
      case annotation if pick.isDefinedAt(annotation.value) =>
        annotation.map(pick)
    })

  /** It, with `more` annotations after its own, if they fit its text. */
  def annotate[B >: A]
    (more: List[Annotation[B]])
    : Either[List[Problem], AnnotatedText[B]] =
    AnnotatedText.of(text, annotations ++ more)

  /** It, with the annotations of `other` after its own, if it is of its text. */
  def ++ [B >: A]
    (other: AnnotatedText[B])
    : Either[List[Problem], AnnotatedText[B]] = Either.cond(
    other.text == text,
    copy(annotations = annotations ++ other.annotations),
    List(Problem.OtherText),
  )

object AnnotatedText:

  /** `text`, with `annotations`, if they fit it, or else what is wrong. */
  def of[A]
    (
      text: String,
      annotations: List[Annotation[A]],
    )
    : Either[List[Problem], AnnotatedText[A]] =
    annotations.zipWithIndex.flatMap(Problem.of(text)) match
      case Nil      => Right(AnnotatedText(text, annotations))
      case problems => Left(problems)

  /** `text`, with no annotations. */
  def plain(text: String): AnnotatedText[Nothing] = AnnotatedText(text, Nil)

  /**
    * `text`, with those of `annotations` that fit it, the rest left out. It
    * only checks them, and moves none: annotations of a text since edited must
    * first be carried to where they stand in it, after which some may no longer
    * fit, as those whose words were edited away.
    */
  def fitting[A]
    (
      text: String,
      annotations: List[Annotation[A]],
    )
    : AnnotatedText[A] = AnnotatedText(
    text,
    annotations
      .zipWithIndex
      .collect {
        case (annotation, index)
          if Problem.of(text)(annotation, index).isEmpty => annotation
      },
  )

  /**
    * The JSON Schema of an annotated text whose values conform to the schema
    * `value`, whose definitions are lifted to its root, as
    * [[JsonSchema.around]] describes.
    */
  def schema(value: Json): Json = JsonSchema.around(value)(value =>
    Json.obj(
      "type"        -> Json.fromString("object"),
      "description" -> Json.fromString(
        "A text with annotations, each attaching a value to some of its spans.",
      ),
      "properties" -> Json.obj(
        "text" -> Json.obj(
          "type"        -> Json.fromString("string"),
          "description" -> Json.fromString("The text."),
        ),
        "annotations" -> Json.obj(
          "type"  -> Json.fromString("array"),
          "items" -> Annotation.schema(value),
        ),
      ),
      "required"             -> Json.arr(texts("text", "annotations")*),
      "additionalProperties" -> Json.False,
    ),
  )

  private def texts(names: String*): Seq[Json] = names.map(Json.fromString)

  /** An annotated text as `{"text": ..., "annotations": [...]}`. */
  given [A : Encoder] => Encoder[AnnotatedText[A]] =
    Encoder.forProduct2("text", "annotations")(annotated =>
      (annotated.text, annotated.annotations),
    )

  /** An annotated text, refused unless its annotations fit its text. */
  given [A : Decoder] => Decoder[AnnotatedText[A]] = Decoder
    .forProduct2("text", "annotations")(
      (
        text: String,
        annotations: List[Annotation[A]],
      ) => (text, annotations),
    )
    .emap((text, annotations) =>
      of(text, annotations).left.map(_.map(_.message).mkString(" ")),
    )

/** What keeps annotations from fitting a text. */
enum Problem:

  /** The annotation at `index` has no spans. */
  case Unspanned(index: Int)

  /**
    * The span `span` of the annotation at `index` does not lie within the text,
    * or ends before it begins.
    */
  case Outside(index: Int, span: Span)

  /** The spans of the annotation at `index` are out of order, or overlap. */
  case Unordered(index: Int)

  /**
    * The span `span` of the annotation at `index` begins or ends within a
    * character that takes two code units, as an emoji does, splitting it.
    */
  case Splits(index: Int, span: Span)

  /** The annotations are of another text. */
  case OtherText

  /** It, described in a sentence. */
  def message: String = this match
    case Unspanned(index)     => s"Annotation $index has no spans."
    case Outside(index, span) => s"Annotation $index has a span, from ${ span
          .start } to ${ span.end }, outside the text."
    case Unordered(index) =>
      s"Annotation $index has spans out of order, or overlapping."
    case Splits(index, span) => s"Annotation $index has a span, from ${ span
          .start } to ${ span.end }, that splits a character."
    case OtherText => "The annotations are of another text."

object Problem:

  /** What keeps `annotation`, at `index`, from fitting `text`. */
  def of(text: String)(annotation: Annotation[?], index: Int): List[Problem] =
    if annotation.spans.isEmpty then List(Unspanned(index))
    else
      val (inside, outside) = annotation.spans.partition(_.fits(text.length))
      outside.map(Outside(index, _)) ++
        inside.filter(splits(text, _)).map(Splits(index, _)) ++
        Option.when(!ordered(annotation.spans))(Unordered(index))

  private def splits(text: String, span: Span): Boolean =
    Script.splits(text, span.start) || Script.splits(text, span.end)

  /** Whether each of `spans` ends before the next begins, or where it does. */
  private def ordered(spans: List[Span]): Boolean = spans
    .zip(spans.drop(1))
    .forall((one, two) => one.end <= two.start && one != two)
