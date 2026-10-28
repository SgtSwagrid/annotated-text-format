package com.alecdorrington.annotatedtextformat

/** How the scripts in which texts are written divide their words. */
private[annotatedtextformat] object Script:

  /**
    * Whether a word in the script of the code point `char` ends where its
    * letters do, as in Latin; not so in Chinese, Japanese or Thai, which do not
    * space their words, nor in Korean, which joins particles to them.
    */
  def bounded(char: Int): Boolean = !joined(char) && !within(hangul, char)

  /**
    * Whether the script of the code point `char` spaces its words, and its
    * punctuation off them, as Latin and Korean do; not so Chinese, Japanese or
    * Thai, nor their full-width punctuation.
    */
  def spaced(char: Int): Boolean = !joined(char)

  /** The code point of `text` that ends at `at`, if any. */
  def before(text: String, at: Int): Option[Int] =
    Option.when(0 < at && at <= text.length)(text.codePointBefore(at))

  /** The code point of `text` that begins at `at`, if any. */
  def after(text: String, at: Int): Option[Int] =
    Option.when(0 <= at && at < text.length)(text.codePointAt(at))

  /**
    * Whether `at` falls within a character of `text` that takes two code units,
    * splitting it, rather than between two characters.
    */
  def splits(text: String, at: Int): Boolean = 0 < at && at < text.length &&
    text(at - 1).isHighSurrogate && text(at).isLowSurrogate

  private def joined(char: Int): Boolean = within(unspaced, char)

  private def within(ranges: List[Range], char: Int): Boolean =
    ranges.exists(_.contains(char))

  /** The scripts that do not space their words, and their punctuation. */
  private val unspaced: List[Range] = List(
    0x0E00 to 0x0EFF,  // Thai and Lao.
    0x1000 to 0x109F,  // Myanmar.
    0x1780 to 0x17FF,  // Khmer.
    0x3000 to 0x303F,  // CJK symbols and punctuation, as the full stop.
    0x3040 to 0x30FF,  // Hiragana and katakana.
    0x3400 to 0x4DBF,  // Han, extension A.
    0x4E00 to 0x9FFF,  // Han.
    0xF900 to 0xFAFF,  // Han, compatibility ideographs.
    0xFF00 to 0xFF65,  // Full-width forms, as the full-width comma.
    0xFF66 to 0xFF9F,  // Katakana, half width.
    0x20000 to 0x3FFFF, // Han beyond the basic plane.
  )

  /** Korean's syllables and letters. */
  private val hangul: List[Range] = List(
    0x1100 to 0x11FF,
    0x3130 to 0x318F,
    0xAC00 to 0xD7AF,
  )
