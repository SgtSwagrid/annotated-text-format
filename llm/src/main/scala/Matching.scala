package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.Script
import java.util.regex.Pattern
import scala.util.matching.Regex

/**
  * How loosely a part of a quote may match the text, as a model may quote it a
  * little otherwise than it is written.
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

  /**
    * Where `part`, a part of a quote, is in `text`, in order; nowhere if there
    * is nothing in it to match but a caret.
    */
  def find(part: String, text: String): List[Found] =
    val tokens = Token.of(part)
    if tokens.forall(_ == Token.Caret) then Nil
    else
      pattern(tokens)
        .findAllMatchIn(text)
        .map(Found.of)
        .filter(found => whole(tokens, text, found))
        .toList

  /**
    * The pattern of `tokens`, which captures where a caret stands, if there is
    * one, as its first group. It uses only what JavaScript's regular
    * expressions had before ECMAScript 2018, as Scala.js may link for those.
    */
  private def pattern(tokens: List[Token]): Regex = (flags + body(tokens)).r

  private def flags: String = if anyCase then "(?iu)" else "(?u)"

  /**
    * Whether `found` begins and ends with whole words, where the quote does and
    * whole words are asked for.
    */
  private def whole
    (
      tokens: List[Token],
      text: String,
      found: Found,
    )
    : Boolean =
    (!tokens.headOption.exists(bounded(_.head)) ||
      Words.begins(text, found.start)) &&
    (!tokens.lastOption.exists(bounded(_.last)) || Words.ends(text, found.end))

  private def body(tokens: List[Token]): String =
    if anySpacing then spaced(Token.spaced(tokens))
    else tokens.map(source).mkString

  /** `tokens`, each but the first marked as spaced off the one before or not. */
  private def spaced(tokens: Vector[(Token, Boolean)]): String = tokens
    .indices
    .map(at => source(tokens(at)._1) + Joint(tokens, at).between)
    .mkString

  private def source(token: Token): String = token match
    case Token.Word(text)  => Pattern.quote(text)
    case Token.Space(text) => Pattern.quote(text)
    case Token.Other(char) => other(char)
    case Token.Caret       => "()"

  private def other(char: Char): String =
    if anyQuotes && apostrophes.contains(char) then s"[$apostrophes]"
    else if anyQuotes && doubles.contains(char) then s"[$doubles]"
    else Pattern.quote(char.toString)

  /** Whether `token` is a word whose `end` should be the end of a whole one. */
  private def bounded(end: String => Char)(token: Token): Boolean = token match
    case Token.Word(text) => wholeWords && Script.bounded(end(text))
    case _                => false

  private val apostrophes = "'’‘"
  private val doubles     = "\"“”„"

/**
  * Where a part of a quote was found, from `start` up to `end`, and the `point`
  * that it marks, if it marks one.
  */
final case class Found(start: Int, end: Int, point: Option[Int])

private object Found:

  def of(found: Regex.Match): Found = Found(
    found.start,
    found.end,
    Option.when(found.groupCount >= 1 && found.start(1) >= 0)(found.start(1)),
  )

/** Where words begin and end. */
private object Words:

  /**
    * Whether a word may begin at `at` in `text`: not just after a letter, a
    * digit or a mark, nor after an apostrophe or hyphen that follows one.
    */
  def begins(text: String, at: Int): Boolean =
    !text.lift(at - 1).exists(Token.wordly) &&
    !(text.lift(at - 1).exists(joiners.contains) &&
    text.lift(at - 2).exists(_.isLetterOrDigit))

  /**
    * Whether a word may end at `at` in `text`: not just before a letter, a
    * digit or a mark, nor before an apostrophe or hyphen that precedes one.
    */
  def ends(text: String, at: Int): Boolean =
    !text.lift(at).exists(Token.wordly) &&
    !(text.lift(at).exists(joiners.contains) &&
    text.lift(at + 1).exists(_.isLetterOrDigit))

  /** What may join two words into one, as in "don't" or "well-known". */
  private val joiners = "'’-"

/**
  * Where the token `at` of `tokens` meets the next, each marked as spaced off
  * the one before it in the quote or not.
  */
private final case class Joint(tokens: Vector[(Token, Boolean)], at: Int):

  /**
    * What lies between the token and the next: whitespace between two words of
    * a script that spaces them, or else any or none; and about a caret, the
    * whitespace between the words around it, before or after it, as the quote
    * spaces it.
    */
  def between: String = (token(at), token(at + 1)) match
    case (_, None)              => ""
    case (Some(Token.Caret), _) => Joint(tokens, at - 1).caret._2
    case (_, Some(Token.Caret)) => caret._1
    case (one, two)             => if apart(one, two) then gap else any

  /** The whitespace before and after the caret just after the token. */
  private def caret: (String, String) =
    if !apart(token(at), token(at + 2)) then (any, any)
    else
      (spaced(at + 1), spaced(at + 2)) match
        case (true, false)  => (gap, "")
        case (true, true)   => (gap, any)
        case (false, true)  => ("", gap)
        case (false, false) => (any, any)

  /** Whether `one` and `two` are words that must be spaced apart. */
  private def apart(one: Option[Token], two: Option[Token]): Boolean =
    (one, two) match
      case (Some(Token.Word(first)), Some(Token.Word(second))) =>
        Script.spaced(first.last) && Script.spaced(second.head)
      case _ => false

  private def token(index: Int): Option[Token] = tokens.lift(index).map(_._1)

  private def spaced(index: Int): Boolean = tokens.lift(index).exists(_._2)

  private val gap = Token.whitespace + "+"
  private val any = Token.whitespace + "*"

/** A piece of a part of a quote. */
private enum Token:

  /** A run of letters, digits and marks. */
  case Word(text: String)

  /** A run of whitespace. */
  case Space(text: String)

  /** A character of punctuation, or any other. */
  case Other(char: Char)

  /** The point that a part marks. */
  case Caret

private object Token:

  /** The tokens of `part`, without the whitespace at its ends. */
  def of(part: String): List[Token] =
    List.unfold(part.trim)(rest => Option.when(rest.nonEmpty)(next(rest)))

  /** `tokens` but their spaces, each marked as spaced off the one before. */
  def spaced(tokens: List[Token]): Vector[(Token, Boolean)] = tokens
    .foldLeft((Vector.empty[(Token, Boolean)], false)):
      case ((kept, _), Space(_)) => (kept, true)
      case ((kept, gap), token)  => (kept :+ (token -> gap), false)
    ._1

  /** The token at the start of `text`, and the rest of `text`. */
  private def next(text: String): (Token, String) = text.head match
    case Quoted.caret              => (Caret, text.tail)
    case char if char.isWhitespace => run(text, _.isWhitespace, Space(_))
    case char if wordly(char)      => run(text, wordly, Word(_))
    case char                      => (Other(char), text.tail)

  private def run
    (
      text: String,
      within: Char => Boolean,
      token: String => Token,
    )
    : (Token, String) =
    val (taken, rest) = text.span(within)
    (token(taken), rest)

  /**
    * Whitespace, as a class of a regular expression: any of Java's, and the
    * spaces that do not break lines, which web pages hold, but Java's `\s` does
    * not match.
    */
  val whitespace: String = """[\s\u00a0\u2007\u202f]"""

  /** Whether `char` belongs in a word: a letter, a digit or a mark. */
  def wordly(char: Char): Boolean = char.isLetterOrDigit ||
    marks.contains(Character.getType(char))

  private val marks: Set[Int] = Set(
    Character.NON_SPACING_MARK,
    Character.ENCLOSING_MARK,
    Character.COMBINING_SPACING_MARK,
  ).map(_.toInt)
