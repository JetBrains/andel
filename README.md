# Anděl

Current stage of the project is work in progress. It's not ready to use by any means.

## The goals of the project and priciples behind it are:
* Create a universal code editor component, which could be embedded in a web application
* Great performance
* No limitation for the document size (it should be able to fetch only parts of a document into memory as needed)
* Purely functional implementation. Which means no mutable state except single reference to an immutable model, ui is considered to be pure function of immutable state

## Currently taken design decisions
* Document is stored in a rope-like data structure (tree branching factor is 32).
* Leafs in this rope are blocks of text of size 64 characters.
* Rope maintains a number of metrics. Such as number of linebreaks and characters. Some other associative metrics like brace matches, soft-wraps data and folding are to be stored there in a future.
* Background lexing. Currently tokens array is stored per line. We should get rid of the assumption of short lines in a nearest future. The probable solution is to store it for fixed-size blocks.
* Virtual scroll is performed by wheel handler and translate3d is used to adjust view position for a smooth scrolling.
* Despite reagent is used to render lines array, lines themselves are rendered by supplying a real DOM elements.
* At the moment editor uses standalone running CodeMirror modes to provide syntax highlighting. They are bundled in resource folder.
* No reads from DOM. It implies limitation of using monospaced fonts only, but we are choosing performance in this tradeoff. All needed measurements should be performed once and ahead of time.

## Roadmap
* Repair caret movement and other basic editor stuff after the redesign
* Figure out the best way to store persistent range markers for non-trivial highlighting (they are very massive and should be preserved during the edits)
* Get rid of short lines assumptions in lexer part
* Implement soft-wraps and folding
* Make an extension points to extend editor with custom widgets, code completion, custom popups.

### Credits
Thanks to:
* ClojureScript
* CodeMirror
* Xi-editor
* Reagent
