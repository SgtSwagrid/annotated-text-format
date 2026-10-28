<div align="center">

  <h1>🏷️ Annotated Text Format</h1>
  <p>A common format for text annotated with typed values, such as corrections and translations.</p>
  
  <span>
    <a href="https://github.com/SgtSwagrid/annotated-text-format/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/annotated-text-format/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
    <a href="https://search.maven.org/artifact/com.alecdorrington/annotated-text-format_3"><img src="https://img.shields.io/maven-central/v/com.alecdorrington/annotated-text-format_3.svg" alt="Maven Central" /></a>
    <a href="https://alecdorrington.com/annotated-text-format"><img src="https://img.shields.io/badge/docs-latest-blue.svg" alt="Documentation" /></a>
  </span>
  
</div>

## ⬇️ Installation

_Annotated Text Format_ comes in three parts, each an artefact of its own:

| Artefact | What it holds |
| -------- | ------------- |
| `annotated-text-format` | The format itself: `AnnotatedText[A]`, a text with annotations, each attaching a value of type `A` to one or more of its spans; their validation, circe codecs and JSON Schema. |
| `annotated-text-format-llm` | The interface to large language models: annotations quoted as a model writes them, the schema of its reply, which tells it how to quote, and anchoring its quotes to the text. |
| `annotated-text-format-dom` | Rendering in a page: annotations highlighted in the page's own text, shown and hidden, and taken out again, leaving the page as it was; and a tooltip for them, opened by the pointer, a tap or the keyboard. |

Add whichever you need to your `build.sbt`:

```scala
libraryDependencies += "com.alecdorrington" %% "annotated-text-format"     % "0.1.0"
libraryDependencies += "com.alecdorrington" %% "annotated-text-format-llm" % "0.1.0"
libraryDependencies += "com.alecdorrington" %% "annotated-text-format-dom" % "0.1.0"
```

Compiled with Scala `3.9.0`. The format and the interface to models are for the JVM and, through
[Scala.js](https://www.scala-js.org/), for JavaScript, where sbt 2 resolves `%%` to the right artefact (sbt 1 needs
`%%%`); rendering is for JavaScript alone.

## 👁️ See also

- This library was made using [Scala Library Template](https://github.com/SgtSwagrid/scala-library-template).
