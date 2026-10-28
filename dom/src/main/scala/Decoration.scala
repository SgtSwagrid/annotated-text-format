package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}
import org.scalajs.dom
import scala.annotation.tailrec
import scala.collection.mutable
import scala.scalajs.js
import scala.util.control.NonFatal

/**
  * Puts annotations into the page itself, in the text of a [[Passage]], as a
  * page that is not one's own must be: splitting its text nodes and wrapping
  * their parts, never building the passage anew, and leaving each text node of
  * the page where it was, so that [[Decoration.restore]] gives the page back
  * exactly as it was, its own text nodes whole.
  */
object Decorator:

  /**
    * `annotated`, put into the page in `passage`, whose text it must be of, as
    * `look` shows it. Each element it puts there has the class `prefix`, or one
    * beginning with it, the attribute `data-<prefix>`, the indices of the
    * annotations it belongs to, and the attribute [[Passage.decorated]]. It
    * owns the `background` of each element it highlights, drawing beneath the
    * highlight whatever layers `look` adds, and, where the system forces its
    * own colours, which takes backgrounds away, it underlines the text. It is
    * refused if the annotations are of another text, or if `prefix` could not
    * begin a class's name as it is, unescaped in a selector.
    *
    * If `whole`, an annotation is put in only if all of it can be, as it cannot
    * be over what is not prose, or another decoration, nor over line breaks
    * alone, nor at a point with no text beside it or within what another
    * annotation shows in place of its text, nor show its text in place of what
    * another already does; the rest are its `unplaced`.
    *
    * Once the page is decorated, it tells `split` of each text node of the page
    * that it split, with the node split off it, if that is still in the page,
    * not wrapped or replaced by an element of its own, as an application
    * marking the page's nodes may copy its mark; a `split` that throws is
    * passed over. As the decoration is restored, each node split off is joined
    * back into the node it came from, and taken out of the page.
    *
    * Decorations of one page may be restored in any order.
    */
  def decorate[A]
    (
      passage: Passage,
      annotated: AnnotatedText[A],
      look: Look[A],
      prefix: String = "atf",
      whole: Boolean = false,
      split: (dom.Text, dom.Text) => Unit = (_, _) => (),
    )
    : Either[Refusal, Decoration[A]] =
    if !named(prefix) then Left(Refusal.Unnamed(prefix))
    else if annotated.text != passage.text then Left(Refusal.OtherText)
    else
      Right(Decoration.make(
        passage,
        annotated,
        look,
        prefix,
        whole,
        split,
      ))

  /** Whether `name` may begin a class's name as it is, unescaped in a selector. */
  private[dom] def named(name: String): Boolean = """[a-zA-Z_][a-zA-Z0-9_-]*"""
    .r
    .matches(name)

/** Why annotations could not be put into a page. */
enum Refusal:

  /** The annotations are of another text than the passage's. */
  case OtherText

  /** `prefix` could not begin a class's name, unescaped in a selector. */
  case Unnamed(prefix: String)

  /** It, described in a sentence. */
  def message: String = this match
    case OtherText => "The annotations are of another text than the passage's."
    case Unnamed(prefix) => s"\"$prefix\" cannot begin the name of a class."

/**
  * The annotations of `annotated`, put in the page in a passage, which may be
  * hidden and shown again, each read as the page's own text while hidden, and
  * taken out of it. Each annotation is known by its index in `annotated`.
  */
final class Decoration[A] private (
  val annotated: AnnotatedText[A],
  val unplaced: Set[Int],
  passage: Passage,
  placed: Vector[Placed[A]],
  origins: Origins,
  look: Look[A],
  prefix: String,
  sheet: dom.Element,
):

  private var hidden = Set.empty[Int]

  /** The annotations lit, as the pointer is over one of their elements. */
  private var lit = Set.empty[Int]

  /** The classes from `look` that each highlighted element has been given. */
  private val classed = mutable.HashMap.empty[dom.Element, Set[String]]

  /** Whether the annotation at `annotation` is shown. */
  def isShown(annotation: Int): Boolean = !hidden.contains(annotation)

  /** Hides the annotation at `annotation`, so that it reads as the page's text. */
  def hide(annotation: Int): Unit = change(hidden + annotation, annotation)

  /** Shows the annotation at `annotation` again. */
  def show(annotation: Int): Unit = change(hidden - annotation, annotation)

  private def change(now: Set[Int], annotation: Int): Unit =
    hidden = now
    lit = lit.filter(isShown)
    concerning(Set(annotation)).foreach(refresh)
    glow()

  /**
    * Draws every annotation again, as after what `look` gives for them has
    * changed, as the layers drawn beneath them may follow an application's own
    * state.
    */
  def redraw(): Unit = placed.foreach(refresh)

  /** The elements in the page of the annotation at `annotation`. */
  def elements(annotation: Int): List[dom.Element] = concerning(Set(annotation))
    .map(_.styled)
    .toList

  /** The annotations shown in the element of the page that `node` is within. */
  def at(node: dom.Node): List[Int] = placedAt(node)
    .toList
    .flatMap(_.piece.annotations.filter(isShown).toList.sorted)

  /**
    * The highlighted element of the page that `node` is within, if any
    * annotation is shown there, as a tooltip is anchored to.
    */
  def elementAt(node: dom.Node): Option[dom.Element] = placedAt(node)
    .filter(_.piece.annotations.exists(isShown))
    .map(_.styled)

  private def placedAt(node: dom.Node): Option[Placed[A]] =
    placed.find(_.element.contains(node))

  /** The outermost element in the page of each piece that it placed there. */
  private[dom] def outermost: Vector[dom.Element] = placed.map(_.element)

  /**
    * Takes every annotation out of the page, putting back the text that each
    * took the place of, and joining each text node of the page back together.
    */
  def restore(): Unit = if !taken then
    taken = true
    placed.foreach(unplace)
    Origins.join(around(passage.root), origins)
    Forcing.release(sheet)

  /** Whether it has been taken out of the page, after which it does nothing. */
  private var taken = false

  /** Whether it has been taken out of the page. */
  private[dom] def isRestored: Boolean = taken

  /**
    * Where the text nodes that `root` was split into lie: beneath it, or, if it
    * is a text node itself, beside it, in its parent.
    */
  private def around(root: dom.Node): dom.Node =
    if root.nodeType == Passage.textNode then
      Option(root.parentNode).getOrElse(root)
    else root

  private def unplace(each: Placed[A]): Unit = Option(each.element.parentNode)
    .foreach(parent =>
      each.piece match
        case _: Piece.Insert[?] => parent.removeChild(each.element)
        case _: Piece.Wrap[?]   =>
          parent.replaceChild(restored(each), each.element),
    )

  /** The text that `each` took the place of, from the same text node. */
  private def restored(each: Placed[A]): dom.Text =
    val text = passage.document.createTextNode(each.original)
    origins.of(each.element).foreach(origins.derive(text, _))
    text

  private def concerning(annotations: Set[Int]): Vector[Placed[A]] = placed
    .filter(_.piece.annotations.exists(annotations))

  /** Shows `each` as its annotations' being shown or hidden has it. */
  private def refresh(each: Placed[A]): Unit = each.piece match
    case Piece.Wrap(_, marks, replacement) =>
      val text = replacement
        .filter(r => isShown(r.annotation))
        .fold(each.original)(_.text)
      if each.element.textContent != text then each.element.textContent = text
      draw(
        each,
        marks.filter(mark => isShown(mark.annotation)),
      )
      each
        .element
        .classList
        .toggle(
          s"$prefix-hidden",
          !each.piece.annotations.exists(isShown),
        )
    case Piece.Insert(_, mark, _, _, _) =>
      each
        .element
        .classList
        .toggle(
          s"$prefix-hidden",
          !isShown(mark.annotation),
        )
      if isShown(mark.annotation) then each.unstow() else each.stow()
      draw(
        each,
        List(mark).filter(mark => isShown(mark.annotation)),
      )

  /**
    * Highlights `each` under `marks`, or takes its highlight away if none,
    * giving it the classes that `look` gives their annotations.
    */
  private def draw(each: Placed[A], marks: List[Mark[A]]): Unit =
    val classes = marks
      .flatMap(mark => look.classes(mark.value))
      .filter(name =>
        Decorator.named(name) && name != prefix && !name.startsWith(s"$prefix-"),
      )
      .toSet
    val before = classed.getOrElse(each.styled, Set.empty)
    (before -- classes).foreach(each.styled.classList.remove)
    (classes -- before).foreach(each.styled.classList.add)
    classed.update(each.styled, classes)
    val style = each.styled.asInstanceOf[dom.HTMLElement].style
    Style.properties.foreach(style.removeProperty)
    Style
      .declarations(
        Style.layers(marks.map(mark => look.colour(mark.value) -> mark.lane)) ++
          (if marks.isEmpty then Nil else look.beneath(marks.map(_.value))),
      )
      .foreach((property, value) =>
        style.setProperty(property, value, "important"),
      )

  /** Lights the annotations of `each`, or puts them out. */
  private def light(each: Placed[A], on: Boolean): Unit =
    val annotations = each.piece.annotations.filter(isShown)
    lit = if on then lit ++ annotations else lit -- annotations
    glow()

  /** Marks as lit every element of an annotation lit, and only those. */
  private def glow(): Unit = placed.foreach(each =>
    each
      .styled
      .classList
      .toggle(
        s"$prefix-lit",
        each.piece.annotations.exists(lit),
      ),
  )

  private def listen(each: Placed[A]): Unit =
    each
      .styled
      .addEventListener(
        "mouseenter",
        (_: dom.Event) => light(each, true),
      )
    each
      .styled
      .addEventListener(
        "mouseleave",
        (_: dom.Event) => light(each, false),
      )

private object Decoration:

  /**
    * `annotated`, put into the page in `passage`, as `look` shows it, each
    * annotation whole or not at all if `whole`, telling `split` of each text
    * node split.
    */
  def make[A]
    (
      passage: Passage,
      annotated: AnnotatedText[A],
      look: Look[A],
      prefix: String,
      whole: Boolean,
      split: (dom.Text, dom.Text) => Unit,
    )
    : Decoration[A] =
    val origins            = Origins.shared
    val texts              = passage.texts.map(_._2).toList
    val (pieces, unplaced) =
      if whole then
        settle(
          annotated,
          look,
          passage,
          broken(annotated, passage),
        )
      else (Plan.of(annotated, look, texts), Set.empty[Int])
    val placed     = Edits(passage, origins, prefix, split).make(pieces)
    val decoration = new Decoration(
      annotated,
      unplaced,
      passage,
      placed,
      origins,
      look,
      prefix,
      Forcing.use(passage.root, prefix),
    )
    placed.foreach(decoration.refresh)
    placed.foreach(decoration.listen)
    decoration

  /**
    * The annotations of `annotated` that could not be put in `passage` whole:
    * those with a span over what is not prose, or another decoration, or with a
    * point with no text beside it.
    */
  private def broken[A]
    (
      annotated: AnnotatedText[A],
      passage: Passage,
    )
    : Set[Int] = annotated
    .annotations
    .zipWithIndex
    .collect {
      case (annotation, index) if !annotation.spans.forall(fits(passage)) =>
        index
    }
    .toSet

  private def fits(passage: Passage)(span: Span): Boolean =
    if span.isPoint then
      passage
        .texts
        .exists((_, text) => text.start <= span.start && span.start <= text.end)
    else
      !passage
        .atoms
        .exists((_, atom) => atom.start < span.end && span.start < atom.end) &&
      passage
        .texts
        .exists((_, text) => text.start < span.end && span.start < text.end)

  /**
    * The pieces of `annotated`, leaving out the annotations `broken`, and then,
    * one at a time, the last that they do not draw whole, until all left are;
    * and then putting back each left out that would be drawn whole, as one may
    * have been only for another since left out; and those left out.
    */
  private def settle[A]
    (
      annotated: AnnotatedText[A],
      look: Look[A],
      passage: Passage,
      broken: Set[Int],
    )
    : (List[Piece[A]], Set[Int]) =
    def plan(skipped: Set[Int]): List[Piece[A]] = Plan.of(
      annotated,
      look,
      passage.texts.map(_._2).toList,
      skipped,
    )
    def partial(skipped: Set[Int]): List[Int] =
      val pieces = plan(skipped)
      annotated
        .annotations
        .zipWithIndex
        .collect {
          case (annotation, index)
            if !skipped(index) &&
            !drawn(pieces, annotation, index, look, passage) => index
        }
    @tailrec
    def narrow(skipped: Set[Int]): Set[Int] = partial(skipped).lastOption match
      case Some(index) => narrow(skipped + index)
      case None        => skipped
    @tailrec
    def widen(skipped: Set[Int]): Set[Int] = (skipped -- broken)
      .toList
      .sorted
      .find(index => partial(skipped - index).isEmpty) match
        case Some(index) => widen(skipped - index)
        case None        => skipped
    val skipped = widen(narrow(broken))
    (plan(skipped), skipped)

  /**
    * Whether `pieces` draw `annotation`, at `index`, whole: each point put in
    * where there is text beside it, and each span that shows other text, as
    * `look` has it, showing it.
    */
  private def drawn[A]
    (
      pieces: List[Piece[A]],
      annotation: Annotation[A],
      index: Int,
      look: Look[A],
      passage: Passage,
    )
    : Boolean = annotation
    .spans
    .zipWithIndex
    .forall((span, part) =>
      if span.isPoint then
        pieces.exists:
          case Piece.Insert(at, mark, _, _, _) => mark.annotation == index &&
            mark.part == part &&
            (passage.at(at, after = true).isDefined ||
            passage.at(at, after = false).isDefined)
          case _ => false
      else
        look.shown(annotation.value, part).isEmpty || pieces.exists:
          case Piece.Wrap(wrapped, _, Some(replacement)) =>
            replacement.annotation == index && span.covers(wrapped)
          case _ => false,
    )

/**
  * The stylesheet underlining the text of every element highlighted with a
  * prefix where the system forces its own colours, which takes away their
  * backgrounds and so their highlights: put in, once for each prefix, where the
  * passage is, in its shadow root if it is in one, or else in its document's
  * head, and taken out once no decoration uses it. A page whose policy forbids
  * styles of its own goes without it.
  */
private object Forcing:

  /**
    * The sheet of `prefix` for the passage at `root`, counting one more use of
    * it.
    */
  def use(root: dom.Node, prefix: String): dom.Element =
    val container = containerOf(root)
    val sheet     =
      sheetIn(container, prefix).getOrElse(made(root, container, prefix))
    sheet.setAttribute("data-uses", (uses(sheet) + 1).toString)
    sheet

  /** Counts one use less of `sheet`, taking it out once there are none. */
  def release(sheet: dom.Element): Unit =
    if uses(sheet) <= 1 then
      Option(sheet.parentNode).foreach(_.removeChild(sheet))
    else sheet.setAttribute("data-uses", (uses(sheet) - 1).toString)

  /** The sheet of `prefix` in `container`, if it is there. */
  private def sheetIn
    (container: dom.Node, prefix: String)
    : Option[dom.Element] = Option(
    container
      .asInstanceOf[js.Dynamic]
      .querySelector(s"#${ id(prefix) }")
      .asInstanceOf[dom.Element],
  )

  private def made
    (
      root: dom.Node,
      container: dom.Node,
      prefix: String,
    )
    : dom.Element =
    val sheet = document(root).createElement("style")
    sheet.id = id(prefix)
    sheet.textContent =
      s"@media (forced-colors: active) { .$prefix:not(.$prefix-hidden) { text-decoration: underline 2px; } } .$prefix:focus-visible { outline: 2px solid; outline-offset: 1px; }"
    container.appendChild(sheet)
    sheet

  private def uses(sheet: dom.Element): Int =
    Option(sheet.getAttribute("data-uses")).flatMap(_.toIntOption).getOrElse(0)

  private def id(prefix: String): String = s"$prefix-forced-colours"

  /**
    * Where the sheet goes: the shadow root that `root` is in, or else the head
    * of its document, even if `root` is not yet in it.
    */
  private def containerOf(root: dom.Node): dom.Node =
    val top = root.asInstanceOf[js.Dynamic].getRootNode().asInstanceOf[dom.Node]
    if shadow(top) then top
    else
      val page = document(root)
      Option(page.querySelector("head")).getOrElse(page.documentElement)

  /** Whether `node` is a shadow root, not just a fragment of a document. */
  private def shadow(node: dom.Node): Boolean =
    node.nodeType == Passage.fragmentNode && {
      val host = node.asInstanceOf[js.Dynamic].host
      !js.isUndefined(host) && host != null
    }

  private def document(root: dom.Node): dom.Document =
    if root.nodeType == Passage.documentNode then
      root.asInstanceOf[dom.Document]
    else root.ownerDocument

/**
  * A piece put in the page: its `element` there, the one within it that is
  * highlighted, and the text that it took the place of.
  */
private final case class Placed[A]
  (
    piece: Piece[A],
    element: dom.Element,
    styled: dom.Element,
    original: String,
    parts: List[dom.Node],
  ):

  /**
    * Takes the parts of an inserted piece out of the page, so that nothing
    * reading the page finds its text while it is hidden.
    */
  def stow(): Unit = parts
    .filter(_.parentNode == element)
    .foreach(element.removeChild)

  /** Puts the parts of an inserted piece back. */
  def unstow(): Unit = parts
    .filter(_.parentNode != element)
    .foreach(element.appendChild)

/**
  * The edits that put pieces into the page in `passage`, each found before any
  * is made, and made from the end of the passage back, so that each leaves the
  * places of those before it as they were.
  */
private final case class Edits
  (
    passage: Passage,
    origins: Origins,
    prefix: String,
    splitting: (dom.Text, dom.Text) => Unit,
  ):

  private val document = passage.document

  /** The text nodes split, each with the node split off it, in order. */
  private val splits = mutable.ListBuffer.empty[(dom.Text, dom.Text)]

  /** Puts `pieces` into the page, giving each as placed there. */
  def make[A](pieces: List[Piece[A]]): Vector[Placed[A]] =
    val (wraps, inserts) = pieces.partitionMap:
      case wrap: Piece.Wrap[A]     => Left(wrap)
      case insert: Piece.Insert[A] => Right(insert)
    val edits = wraps.flatMap(wrapping) ++ inserting(inserts)
    edits.sortBy(edit => (-edit.offset, edit.rank)).foreach(_.make())
    splits
      .filter((_, rest) => rest.parentNode != null)
      .foreach((node, rest) =>
        try splitting(node, rest)
        catch case NonFatal(_) => (),
      )
    edits.flatMap(_.placed).toVector

  /** The edit wrapping the text of `wrap`, if it lies within one text node. */
  private def wrapping[A](wrap: Piece.Wrap[A]): Option[Edit[A]] = passage
    .within(wrap.span)
    .map((node, from, to) =>
      val element = span(if wrap.marks.isEmpty then s"$prefix-gap" else prefix)
      element.setAttribute(
        Passage.decorated,
        wrap.span.in(passage.text),
      )
      tag(element, wrap)
      Edit(
        wrap.span.start,
        1,
        List(Placed(
          wrap,
          element,
          element,
          wrap.span.in(passage.text),
          Nil,
        )),
        () => replace(node, from, to, element),
      ),
    )

  /**
    * The edits putting in `inserts`, those at one point together, in their
    * order: each at the end of the text before its point if there is one, and
    * made after the wrapping of what begins there, which it goes before; or
    * else at the start of the text after it, and made before.
    */
  private def inserting[A](inserts: List[Piece.Insert[A]]): List[Edit[A]] =
    inserts
      .groupBy(_.at)
      .toList
      .flatMap((at, together) =>
        passage
          .at(at, after = true)
          .map(_ -> 2)
          .orElse(passage.at(at, after = false).map(_ -> 0))
          .map { case ((node, offset), rank) =>
            val placed = together.map(inserted)
            Edit(
              at,
              rank,
              placed,
              () => insert(node, offset, fragment(placed)),
            )
          },
      )

  /** The element of `insert`, holding its text, spaced as it needs. */
  private def inserted[A](insert: Piece.Insert[A]): Placed[A] =
    insert.text match
      case None =>
        val marker = tag(span(s"$prefix-point"), insert)
        Placed(insert, marker, marker, "", Nil)
      case Some(text) =>
        val element = tag(span(s"$prefix-inserted"), insert)
        val styled  = tag(span(prefix), insert)
        styled.textContent = text
        val parts = Option.when(insert.before)(space()).toList ++
          List(styled) ++ Option.when(insert.after)(space()).toList
        parts.foreach(element.appendChild)
        Placed(insert, element, styled, "", parts)

  /** `element`, holding the indices of the annotations of `piece`. */
  private def tag(element: dom.Element, piece: Piece[?]): dom.Element =
    element.setAttribute(
      s"data-$prefix",
      piece.annotations.toList.sorted.mkString(" "),
    )
    element

  private def fragment(placed: List[Placed[?]]): dom.DocumentFragment =
    val fragment = document.createDocumentFragment()
    placed.foreach(each => fragment.appendChild(each.element))
    fragment

  private def space(): dom.Text = document.createTextNode(" ")

  private def span(kind: String): dom.Element =
    val element = document.createElement("span")
    element.classList.add(kind)
    element.setAttribute(Passage.decorated, "")
    element

  /**
    * Puts `element` in place of the text from `from` up to `to` in `node`. If
    * that is where `node` begins, `node` itself stays after it, empty, as the
    * page may hold on to it, and the text goes back into it when restored.
    */
  private def replace
    (
      node: dom.Text,
      from: Int,
      to: Int,
      element: dom.Element,
    )
    : Unit =
    split(node, to)
    origins.derive(element, node)
    split(node, from) match
      case Some(part) => part.parentNode.replaceChild(element, part)
      case None       =>
        node.parentNode.insertBefore(element, node)
        node.data = ""

  /** Puts `inserted` at `at` in `node`, or beside it, if `at` is at an end. */
  private def insert
    (
      node: dom.Text,
      at: Int,
      inserted: dom.Node,
    )
    : Unit = node
    .parentNode
    .insertBefore(
      inserted,
      if at == 0 then node else split(node, at).getOrElse(node.nextSibling),
    )

  /** The text after `at` in `node`, split off it, unless `at` is at an end. */
  private def split(node: dom.Text, at: Int): Option[dom.Text] =
    Option.when(0 < at && at < node.length):
      val rest = node.splitText(at)
      origins.derive(rest, node)
      splits += node -> rest
      rest

/**
  * An edit of the page, made at `offset` in the passage's text, before others
  * there of a higher `rank`, which puts the elements of `placed` there.
  */
private final case class Edit[A]
  (
    offset: Int,
    rank: Int,
    placed: List[Placed[A]],
    make: () => Unit,
  )

/**
  * The text node of the page that each node put there came from, as split from
  * it or put in place of part of it, so that it can be joined back.
  */
private final class Origins:

  /**
    * Each node's origin, by the node itself, held only while the node is, as
    * those of a page's decorations are, which may be restored in any order.
    */
  private val origins = js.Dynamic.newInstance(js.Dynamic.global.WeakMap)()

  /** The text node of the page that `node` came from. */
  def of(node: dom.Node): Option[dom.Text] =
    val origin = origins.get(node.asInstanceOf[js.Any])
    if js.isUndefined(origin) then None else Some(origin.asInstanceOf[dom.Text])

  /**
    * Marks `node` as from the same text node of the page as `text`, and `text`
    * too.
    */
  def derive(node: dom.Node, text: dom.Text): Unit =
    val origin = of(text).getOrElse(text)
    origins.set(
      text.asInstanceOf[js.Any],
      origin.asInstanceOf[js.Any],
    )
    origins.set(
      node.asInstanceOf[js.Any],
      origin.asInstanceOf[js.Any],
    )

private object Origins:

  /** The origins of the nodes of every decoration, which all share them. */
  lazy val shared: Origins = Origins()

  /**
    * Joins each run of text nodes beneath `root` that came from one text node
    * of the page back into it, if the run holds it, or else into its first: a
    * run, as the page may have put something between them since.
    */
  def join(root: dom.Node, origins: Origins): Unit = texts(root)
    .flatMap(text => origins.of(text).map(_ -> text))
    .groupMap(_._1)(_._2)
    .foreach((origin, texts) => runs(texts).foreach(merge(_, origin)))

  private def texts(node: dom.Node): List[dom.Text] = node.nodeType match
    case Passage.textNode => List(node.asInstanceOf[dom.Text])
    case _                => Passage.children(node).flatMap(texts)

  /** `texts`, in runs of those side by side in the page. */
  private def runs(texts: List[dom.Text]): List[List[dom.Text]] = texts
    .foldRight(List.empty[List[dom.Text]]):
      case (text, (next :: run) :: rest) if text.nextSibling == next =>
        (text :: next :: run) :: rest
      case (text, done) => List(text) :: done

  private def merge(run: List[dom.Text], origin: dom.Text): Unit =
    val into = if run.contains(origin) then origin else run.head
    if run.size > 1 then
      into.data = run.map(_.data).mkString
      run.filter(_ != into).foreach(text => text.parentNode.removeChild(text))
