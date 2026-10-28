<div align="center">

  <h1>🏷️ Annotated Text Format</h1>
  <p>A common format for text annotated with typed values, such as corrections and translations.</p>
  
  <span>
    <a href="https://github.com/SgtSwagrid/annotated-text-format/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/annotated-text-format/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
    <a href="https://search.maven.org/artifact/com.alecdorrington/annotated-text-format_3"><img src="https://img.shields.io/maven-central/v/com.alecdorrington/annotated-text-format_3.svg" alt="Maven Central" /></a>
    <a href="https://alecdorrington.com/annotated-text-format"><img src="https://img.shields.io/badge/docs-latest-blue.svg" alt="Documentation" /></a>
  </span>
  
</div>

> [!WARNING]
> Annotated Text Format is still a prototype. Anything could be broken or change between minor versions.

## ⬇️ Installation

There are several components which can be installed separately, but the same version number (e.g. [latest](../../releases/latest)) must be used across all of them.
The following applies for [Scala 3](https://www.scala-lang.org/) projects built with [sbt 2](https://www.scala-sbt.org/).
Add the desired components to your `build.sbt` file:

```scala
// Core (required)
libraryDependencies += "com.alecdorrington" %% "annotated-text-format" % "{version}"

// Interface to large language models
libraryDependencies += "com.alecdorrington" %% "annotated-text-format-llm" % "{version}"

// Rendering in a web page
libraryDependencies += "com.alecdorrington" %% "annotated-text-format-dom" % "{version}"
```

## 📦 Components

| Component | What it holds |
| -------- | ------------- |
| `annotated-text-format` | The format itself: `AnnotatedText[A]`, a text with annotations, each attaching a value of type `A` to one or more of its spans; their validation, circe codecs and JSON Schema. |
| `annotated-text-format-llm` | The interface to large language models: annotations quoted as a model writes them, the schema of its reply, which tells it how to quote, and anchoring its quotes to the text. |
| `annotated-text-format-dom` | Rendering in a page: annotations highlighted in the page's own text, shown and hidden, and taken out again, leaving the page as it was; and a tooltip for them, opened by the pointer, a tap or the keyboard. |

## 👁️ See also

- This library was made using [Scala Library Template](https://github.com/SgtSwagrid/scala-library-template).
