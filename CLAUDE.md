# CLAUDE.md

This file provides guidance to [Claude Code](https://claude.com/product/claude-code) when working with code in this repository.
It is not intended for human eyes.

### Maintenance

You (robot or human) have standing permission to update this file without asking.
Add important patterns, gotchas, or context that would help future sessions.
Keep it concise and actionable.

## Project overview

This is a Scala library that defines a common format for text annotated with typed values, such as corrections and translations.

### Architecture

- The format is kept apart from its uses. The core (`src/`, JVM and JS) defines it and knows neither of the others; `llm/` (JVM and JS) and `dom/` (JS only) each depend on the core alone.
  - Core: `Span` (UTF-16 offsets; equal ends make a point), `Annotation[A]` (one or more spans, in order, none overlapping), `AnnotatedText[A]` (built only through `AnnotatedText.of`, which checks the spans fit; its constructor is `private[annotatedtextformat]`), circe codecs, and JSON Schemas built around a value's schema by `JsonSchema.around`, which lifts the value's `$defs` to the root.
  - `llm`: `Quoted[A]` (parts joined by ` … `, a point marked by `‸`), `Matching` and `Anchoring`, which finds quotes in order and reports those it cannot. It must not depend on Iris, so that a browser plugin's page bundle can hold it.
  - `dom`: `Layout` (segments and lanes, from Themis), `Plan` (pure: what goes in the page), and `Passage`/`Decorator`/`Decoration`, which edit the page's own text nodes as Codeswitch's `Highlighter` did: edits found first and made from the end back, a page's node left in place (emptied) and joined back on restore, a gap for the space beside an emptied span.
    - `Tooltip` (an `atf-tooltip` popover in the top layer, or in the open modal dialog; its content in a shadow root, kept within the window and scrolling within itself; ARIA role, id and label) and `Hovering` (one per page, covering decorations as they are `add`ed and `remove`d; shows the tooltip for the element under the pointer, tapped or focused, finding targets through `composedPath`, so shadow roots work; one Tab stop per annotation; `aria-describedby` while open). Each opening is a `Tooltip.Showing`, whose `update` is dropped once the tooltip has closed or moved on, as a late dictionary answer must be. An interactive `Tooltip` stops the pointer's and keys' events at its host, so the page's bubbling listeners never hear of them, though its capturing ones on the window and document still do. Their geometry (`Box`, `Placement`) is pure, as jsdom measures every box as empty.
- Shared helpers for both uses that are not part of the format, such as `Script`, are `private[annotatedtextformat]` in the core.

### Gotchas

- Scala.js may be linked for ECMAScript 2015, so regular expressions must not use lookbehind or `\p{...}`, which it refuses there at run time; whole words are checked in Scala instead (`Words` in `Matching.scala`). Java's `\s` does not match non-breaking spaces, so use `Token.whitespace`.
- JavaScript writes `20.0` as `20`, so format numbers that go into strings explicitly (as `Colour.faded` does), or the platforms disagree.
- The DOM tests run in Node.js through jsdom, installed by `npm ci` before them (`project/Npm.scala`). Node.js has no DOM globals, so the DOM code never tests a node's class (`case _: dom.Text`, `isInstanceOf`) nor reads `dom.Node.TEXT_NODE`: it compares `nodeType` with the numbers in `Passage`.
- The file tools write `\u00a0` and the like as the characters themselves; write escapes another way (the build's sources hold only visible characters beyond ASCII), and check with a scan for invisible ones.
- scalafmt turns an `if … then … else` inside a pattern guard into invalid syntax; move the condition into a method.
- On Windows, if compiling fails with "A required privilege is not held by the client" while backing up a `.sjsir`, delete `target/out/sjs1/*/*/classes` (sbt 2's disk cache links them) and compile again.
- Scanning a text with `Matcher.find(from)` must step a whole code point at a time: in JavaScript, a search begun inside a surrogate pair finds the match at the pair's start again, forever (`scan` in `Matching.scala`).
- Case is folded character by character (`Matching.folded`), not by `(?i)`, which differs between the JVM and JavaScript.
- `Colour` admits only the forms of a colour, since colours go into styles; never build a `Style.Layer` from a user's text.
- jsdom's HTML parser turns `\r\n` into `\n`; put such text in with `createTextNode`.
- jsdom has no `adoptedStyleSheets` and no `delegatesFocus`, nor does it move the focus by Tab itself; `Tooltip` copies a sheet's rules where it cannot adopt it, and `Hovering` moves the focus itself.
- In the scalajs-dom facade, `HTMLElement.ownerDocument` is an `HTMLDocument`, with `activeElement` and `body`, but `Element.ownerDocument` only a `Document`; cast where needed.
- In Scala.js, reading a JavaScript global that does not exist, as `js.Dynamic.global.CSS` in Node.js, throws; check it first with `js.typeOf(...)`, which compiles to `typeof`.
- To try the library in another project before a release, `sbt 'set ThisBuild / version := "0.1.0-SNAPSHOT"' publishLocal`, and depend on that version.

## Instructions

### Compilation and Diagnostics

- When the user asks for help with a compilation or type error, start by running `sbt compile` to see the error for yourself.
  If there are many errors, making it unclear which one the user is referring to, ask them to clarify, and then focus only on that issue.
- IntelliJ MCP integration is active. When a request seems to implicitly refer to something the user is looking at, always check
  `mcp__ide__getDiagnostics` first to see which file(s) are open and get associated diagnostics (errors, warnings, and info hints with line numbers).

#### Testing

- After making code changes, always run `sbt compile` to verify that issues are fixed and no new ones are introduced.
- Repeatedly retry upon failure until the build succeeds. If you are unsure how to fix an issue, ask for help or refer to existing code for examples.
- Before trying to fix an error, make sure you first understand it fully.
- You should never report that a feature is complete without testing it first.

### Code Style

- You must read the [Code Style Guidelines](docs/STYLE_GUIDE.md).

### Pull Requests

When asked to publish the code changes, your task is to open one or more pull requests (PRs) to merge the changes into `main` on GitHub:

- Use `git` to check what has changed as compared to the `main` branch on `origin`.
- If the changes are thematically linked, they can be published as a single PR.
- Otherwise, you'll need to divide the changes into multiple PRs using your own judgement.
- Each PR should have a singular focus, shouldn't break anything, and should be able to be merged independently.
- Ensure that all code is staged, committed and pushed. Ensure no new files are left uncommitted, and no debug code is left in the codebase.
- When creating a PR, ensure that the title and description are clear, informative, and comprehensive.
- All feature/bugfix/etc branch names should be formatted as "feature_<short description>" or "fix_<short description>" or similar.
- All PR titles should be formatted as "[<scope>] <Short summary>", e.g. "[renderer] Fixed colour inversion bug."
- Use the `gh` CLI to do the above.
