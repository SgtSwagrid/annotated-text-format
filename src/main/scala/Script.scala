package com.alecdorrington.annotatedtextformat

import scala.collection.immutable.NumericRange

/** How the scripts in which texts are written divide their words. */
private[annotatedtextformat] object Script:

  /**
    * Whether a word in the script of `char` ends where its letters do, as in
    * Latin; not so in Chinese, Japanese or Thai, which do not space their
    * words, nor in Korean, which joins particles to them.
    */
  def bounded(char: Char): Boolean = !joined(char) && !within(hangul, char)

  /**
    * Whether the script of `char` spaces its words, as Latin and Korean do; not
    * so Chinese, Japanese or Thai.
    */
  def spaced(char: Char): Boolean = !joined(char)

  private def joined(char: Char): Boolean = within(unspaced, char)

  private def within
    (
      ranges: List[NumericRange[Char]],
      char: Char,
    )
    : Boolean = ranges.exists(_.contains(char))

  /** The scripts that do not space their words. */
  private val unspaced: List[NumericRange[Char]] = List(
    '\u0e00' to '\u0eff', // Thai and Lao.
    '\u1000' to '\u109f', // Myanmar.
    '\u1780' to '\u17ff', // Khmer.
    '\u3040' to '\u30ff', // Hiragana and katakana.
    '\u3400' to '\u4dbf', // Han, extension A.
    '\u4e00' to '\u9fff', // Han.
    '\uf900' to '\ufaff', // Han, compatibility ideographs.
    '\uff66' to '\uff9f', // Katakana, half width.
    '\ud840' to '\ud87f', // Han beyond the basic plane, as surrogates.
  )

  /** Korean's syllables and letters. */
  private val hangul: List[NumericRange[Char]] = List(
    '\u1100' to '\u11ff',
    '\u3130' to '\u318f',
    '\uac00' to '\ud7af',
  )
