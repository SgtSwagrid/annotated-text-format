package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{Script, Span}
import java.util.regex.{Matcher, Pattern}

/**
  * How loosely a part of a quote may match the text, as a model may quote it a
  * little otherwise than it is written. [[Anchoring]] tries the text as it is
  * quoted first, and only then as loosely as this allows.
  *
  * Letters are matched in either case by folding each character to lower case,
  * alike on the JVM and in JavaScript; so `"kiz"` never matches `"kız"`, and
  * neither does `"σ"` match `"ς"`, the final sigma. Text is not normalised, so
  * a letter written with a combining accent does not match one written as a
  * single character. Since a quote as written is sought before a looser match,
  * an exact match further on is preferred to a nearer loose one: a straight
  * `don't` quoted for a curly `don’t` is placed on a later straight one, if
  * there is one.
  *
  * @param anyCase
  *   Whether letters match in either case.
  * @param anySpacing
  *   Whether words match with any whitespace between them, and punctuation with
  *   or without whitespace beside it, as a model may space a full stop.
  * @param anyQuotes
  *   Whether each apostrophe matches any, and each double quotation mark any.
  * @param wholeWords
  *   Whether a part beginning or ending with a word matches only a whole word
  *   there, not part of one, as one joined to it by an apostrophe or hyphen, in
  *   scripts that end their words where their letters do.
  */
final case class Matching
  (
    anyCase: Boolean = true,
    anySpacing: Boolean = true,
    anyQuotes: Boolean = true,
    wholeWords: Boolean = true,
  ):

  /** It, matching only text written exactly as quoted, but as to whole words. */
  def exact: Matching = Matching(false, false, false, wholeWords)

  /**
    * Where `part`, a part of a quote, is in `text`, in order, overlapping
    * matches included; nowhere if there is nothing in it to match but markers.
    */
  def find(part: String, text: String): List[Found] =
    val tokens = Token.of(part)
    if !tokens.exists(_.content) then Nil
    else
      val built = Built.of(tokens, this)
      val found = Pattern.compile(built.source).matcher(folded(text))
      scan(found, built, text).filter(whole(built, text, _))

  /**
    * Each match of `matcher` in `text`, in order, overlapping ones included,
    * each sought from a character after the last began, a whole code point on,
    * as JavaScript would otherwise find again a match that begins at the start
    * of a character taking two code units.
    */
  private def scan
    (
      matcher: Matcher,
      built: Built,
      text: String,
    )
    : List[Found] = List
    .unfold(0)(from =>
      Option.when(from <= text.length && matcher.find(from)):
        val start = matcher.start
        (
          Option.when(start >= from)(built.found(matcher)),
          (
            start + Script.after(text, start).fold(1)(Character.charCount)
          ).max(from + 1),
        ),
    )
    .flatten

  /** `text`, each of its characters in lower case if case does not matter. */
  private[llm] def folded(text: String): String =
    if anyCase then text.map(Character.toLowerCase) else text

  /**
    * Whether `found` begins and ends with whole words, where its part does and
    * whole words are asked for.
    */
  private def whole(built: Built, text: String, found: Found): Boolean =
    (!built.boundedStart || Words.begins(text, found.first)) &&
    (!built.boundedEnd || Words.ends(text, found.last))

  /** The pattern of `char`, a character of punctuation or any other. */
  private[llm] def other(char: String): String =
    if anyQuotes && apostrophes.contains(char) then s"[$apostrophes]"
    else if anyQuotes && doubles.contains(char) then s"[$doubles]"
    else Pattern.quote(folded(char))

  private val apostrophes = "'’‘"
  private val doubles     = "\"“”„"

object Matching:

  /** Whether `part`, a part of a quote, has anything in it to match. */
  private[llm] def matchable(part: String): Boolean = Token
    .of(part)
    .exists(_.content)

/**
  * Where a part of a quote was found, from `start` up to `end`; its first word
  * beginning at `first` and its last ending at `last`; and the span that it
  * marks, if it marks one, or else the whole of it.
  */
final case class Found
  (
    start: Int,
    end: Int,
    first: Int,
    last: Int,
    marked: Option[Span],
  ):

  /** The span that it stands for: the part it marks, or else the whole of it. */
  def span: Span = marked.getOrElse(Span(start, end))

/**
  * The pattern of a part of a quote, `source`, with the numbers of its groups
  * around its first and last words, if it begins or ends with one, and around
  * the passage it marks, if it marks one.
  */
private final case class Built
  (
    source: String,
    first: Option[(Int, String)],
    last: Option[(Int, String)],
    mark: Option[Int],
    wholeWords: Boolean,
  ):

  /** Where the part was found by `matcher`. */
  def found(matcher: Matcher): Found = Found(
    matcher.start,
    matcher.end,
    first.fold(matcher.start)((group, _) => matcher.start(group)),
    last.fold(matcher.end)((group, _) => matcher.end(group)),
    mark.map(group =>
      Span(
        matcher.start(group),
        matcher.end(group),
      ),
    ),
  )

  /** Whether the part begins with a word that must be whole there. */
  def boundedStart: Boolean = wholeWords &&
    first.exists((_, word) => Script.bounded(word.codePointAt(0)))

  /** Whether the part ends with a word that must be whole there. */
  def boundedEnd: Boolean = wholeWords &&
    last.exists((_, word) => Script.bounded(word.codePointBefore(word.length)))

private object Built:

  /**
    * The pattern of `tokens`, built as `matching` allows: each token in turn,
    * with the whitespace between them, and groups around the first and last
    * words and the passage marked.
    */
  def of(tokens: List[Token], matching: Matching): Built =
    val spaced  = Token.spaced(tokens, matching.anySpacing)
    val content = spaced.indices.filter(at => spaced(at)._1.content)
    val words   = (content.headOption ++ content.lastOption)
      .toSet
      .filter(at => word(spaced(at)._1).isDefined)
    spaced
      .indices
      .foldLeft(Building())((building, at) =>
        building.add(
          spaced(at)._1,
          piece(spaced(at)._1, matching),
          Joint(spaced, at).between(matching),
          words.contains(at) && content.headOption.contains(at),
          words.contains(at) && content.lastOption.contains(at),
        ),
      )
      .built(matching.wholeWords)

  /** The text of `token`, if it is a word. */
  def word(token: Token): Option[String] = token match
    case Token.Word(text) => Some(text)
    case _                => None

  /** The pattern of `token` alone. */
  private def piece(token: Token, matching: Matching): String = token match
    case Token.Word(text)  => Pattern.quote(matching.folded(text))
    case Token.Space(text) => Pattern.quote(text)
    case Token.Other(char) => matching.other(char)
    case Token.Caret       => "()"
    case Token.Open        => "("
    case Token.Close       => ")"

/** A pattern being built: its source so far, and its groups so far. */
private final case class Building
  (
    source: String = "",
    count: Int = 0,
    first: Option[(Int, String)] = None,
    last: Option[(Int, String)] = None,
    mark: Option[Int] = None,
  ):

  /**
    * It, with `token` added as `piece` and then `joint`, in a group of its own
    * if it is the first or last word.
    */
  def add
    (
      token: Token,
      piece: String,
      joint: String,
      isFirst: Boolean,
      isLast: Boolean,
    )
    : Building = token match
    case Token.Caret | Token.Open => copy(
        source = source + piece + joint,
        count = count + 1,
        mark = mark.orElse(Some(count + 1)),
      )
    case _ if isFirst || isLast =>
      val group = (count + 1) -> Built.word(token).getOrElse("")
      copy(
        source = source + s"($piece)" + joint,
        count = count + 1,
        first = if isFirst then Some(group) else first,
        last = if isLast then Some(group) else last,
      )
    case _ => copy(source = source + piece + joint)

  def built(wholeWords: Boolean): Built =
    Built(source, first, last, mark, wholeWords)

/**
  * Where the token `at` of `tokens` meets the next, each marked as spaced off
  * the one before it in the quote or not.
  */
private final case class Joint(tokens: Vector[(Token, Boolean)], at: Int):

  /**
    * What lies between the token and the next, as `matching` allows: nothing
    * more than the spaces that are tokens themselves, if spacing must be as
    * quoted; or else whitespace between two words of a script that spaces them,
    * and any or none elsewhere; but about a marker, the whitespace as the quote
    * spaces it on either side, so that the marker keeps its place beside it.
    */
  def between(matching: Matching): String =
    if !matching.anySpacing then ""
    else
      (token(at), token(at + 1)) match
        case (_, None)                    => ""
        case (Some(one), _) if one.marker => around(at)._2
        case (_, Some(two)) if two.marker => around(at + 1)._1
        case (one, two) => if apart(one, two) then gap else any

  /**
    * The whitespace before and after the marker `at`, as the quote spaces it,
    * between the content nearest it on either side: whitespace where the quote
    * has it between two words, any else, and none where it has none.
    */
  private def around(at: Int): (String, String) =
    val (before, after) = (spaced(at), spaced(at + 1))
    if apart(content(at, -1), content(at, 1)) then
      (before, after) match
        case (true, false)  => (gap, "")
        case (true, true)   => (gap, any)
        case (false, true)  => ("", gap)
        case (false, false) => (lazily, any)
    else (if before then any else lazily, if after then any else "")

  /** The content token nearest `at`, going `by` one way or the other. */
  private def content(at: Int, by: Int): Option[Token] = Iterator
    .iterate(at + by)(_ + by)
    .takeWhile(tokens.indices.contains)
    .map(tokens(_)._1)
    .find(_.content)

  /** Whether `one` and `two` are words that must be spaced apart. */
  private def apart(one: Option[Token], two: Option[Token]): Boolean =
    (one, two) match
      case (Some(Token.Word(first)), Some(Token.Word(second))) =>
        Script.spaced(first.codePointBefore(first.length)) &&
        Script.spaced(second.codePointAt(0))
      case _ => false

  private def token(index: Int): Option[Token] = tokens.lift(index).map(_._1)

  private def spaced(index: Int): Boolean = tokens.lift(index).exists(_._2)

  private val gap    = Token.whitespace + "+"
  private val any    = Token.whitespace + "*"
  private val lazily = Token.whitespace + "*?"

/** A piece of a part of a quote. */
private enum Token:

  /** A run of letters, digits and marks. */
  case Word(text: String)

  /** A run of whitespace. */
  case Space(text: String)

  /** A character of punctuation, or any other, as one code point. */
  case Other(char: String)

  /** The point that a part marks. */
  case Caret

  /** Where the passage that a part marks begins. */
  case Open

  /** Where the passage that a part marks ends. */
  case Close

  /** Whether it is a marker, which matches nothing of the text. */
  def marker: Boolean = this match
    case Caret | Open | Close => true
    case _                    => false

  /** Whether it matches something of the text. */
  def content: Boolean = this match
    case Space(_) => false
    case _        => !marker

private object Token:

  /**
    * The tokens of `part`, without the whitespace at its ends. Its markers are
    * taken as such only if it has one caret, or else one passage marked, opened
    * before it is closed; or else as the characters they are.
    */
  def of(part: String): List[Token] = balanced(runs(units(part.trim)))

  private def balanced(tokens: List[Token]): List[Token] =
    tokens.filter(_.marker) match
      case Nil | List(Caret) | List(Open, Close) => tokens
      case _                                     => tokens.map(literal)

  private def literal(token: Token): Token = token match
    case Caret => Other(Quoted.caret.toString)
    case Open  => Other(Quoted.open.toString)
    case Close => Other(Quoted.close.toString)
    case _     => token

  /**
    * `tokens`, each marked as spaced off the one before it; without their
    * spaces, if `loosely`, which the joints between them then stand for.
    */
  def spaced(tokens: List[Token], loosely: Boolean): Vector[(Token, Boolean)] =
    if !loosely then tokens.toVector.map(_ -> false)
    else
      tokens
        .foldLeft((Vector.empty[(Token, Boolean)], false)):
          case ((kept, _), Space(_)) => (kept, true)
          case ((kept, gap), token)  => (kept :+ (token -> gap), false)
        ._1

  /** The characters of `text`, each a code point, as a string of one or two. */
  private def units(text: String): List[String] = List.unfold(0)(at =>
    Option.when(at < text.length):
      val size = Character.charCount(text.codePointAt(at))
      (text.substring(at, at + size), at + size),
  )

  /**
    * `units` as tokens: runs of word characters and of whitespace, and others
    * alone.
    */
  private def runs(units: List[String]): List[Token] = units match
    case Nil                                        => Nil
    case unit :: _ if unit == Quoted.caret.toString => Caret :: runs(units.tail)
    case unit :: _ if unit == Quoted.open.toString  => Open :: runs(units.tail)
    case unit :: _ if unit == Quoted.close.toString => Close :: runs(units.tail)
    case unit :: _ if space(unit)  => run(units, space, Space(_))
    case unit :: _ if wordly(unit) => run(units, wordly, Word(_))
    case unit :: rest              => Other(unit) :: runs(rest)

  private def run
    (
      units: List[String],
      within: String => Boolean,
      token: String => Token,
    )
    : List[Token] =
    val (taken, rest) = units.span(within)
    token(taken.mkString) :: runs(rest)

  private def space(unit: String): Boolean = unit.forall(_.isWhitespace)

  /**
    * Whether the code point `unit` belongs in a word: a letter, a digit or a
    * mark.
    */
  def wordly(unit: String): Boolean = wordly(unit.codePointAt(0))

  /**
    * Whether the code point `char` belongs in a word: a letter, a digit or a
    * mark.
    */
  def wordly(char: Int): Boolean = Character.isLetterOrDigit(char) ||
    marks.contains(Character.getType(char))

  /**
    * Whitespace, as a class of a regular expression: any of Java's, and the
    * spaces that do not break lines, which web pages hold, but Java's `\s` does
    * not match.
    */
  val whitespace: String = """[\s\u00a0\u2007\u202f]"""

  private val marks: Set[Int] = Set(
    Character.NON_SPACING_MARK,
    Character.ENCLOSING_MARK,
    Character.COMBINING_SPACING_MARK,
  ).map(_.toInt)

/** Where words begin and end. */
private object Words:

  /**
    * Whether a word may begin at `at` in `text`: not just after a letter, a
    * digit or a mark, nor after an apostrophe or hyphen that follows one.
    */
  def begins(text: String, at: Int): Boolean =
    !Script.before(text, at).exists(Token.wordly) &&
    !(Script.before(text, at).exists(joiner) &&
    Script.before(text, at - 1).exists(Character.isLetterOrDigit))

  /**
    * Whether a word may end at `at` in `text`: not just before a letter, a
    * digit or a mark, nor before an apostrophe or hyphen that precedes one.
    */
  def ends(text: String, at: Int): Boolean =
    !Script.after(text, at).exists(Token.wordly) &&
    !(Script.after(text, at).exists(joiner) &&
    Script.after(text, at + 1).exists(Character.isLetterOrDigit))

  /** Whether `char` may join two words into one, as in "don't" or "well-known". */
  private def joiner(char: Int): Boolean = "'’-".exists(_.toInt == char)
