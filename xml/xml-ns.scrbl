#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     xml
                     xml/ns))

@title{XML: Namespaces}

This package extends the XML reader to extract namespace information
from XML elements and attributes.

@defmodule[xml/ns]

@defproc[(xml-expand-names [doc document?]) document?]{
  Returns a copy of @racket[doc] with chaperones providing namespace information.
  Element and attribute names are replaced with the local part.
}

@defproc[(xml-expand-names/content [content content/c]
                                   [namespaces
                                    (hash/c (or/c symbol? #f)
                                            (or/c string? #f))
                                    (hasheq #f #f)])
         content/c]{
  Returns a copy of @racket[content] with chaperones providing namespace information.
  Element and attribute names are replaced with the local part.
  @racket[namespaces] is a hash mapping name prefixes to namespace URIs.
}

@defproc[(attribute-name-prefix [att attribute?])
         (or/c symbol? #f)]{
  Returns the namespace part, if any, of @racket[att]'s name.
}

@defproc[(attribute-namespace [att attribute?])
         (or/c string? #f)]{
  Returns the resolved namespace, if any, of @racket[att].
}

@defproc[(element-name-prefix [elt element?])
         (or/c symbol? #f)]{
  Returns the namespace part, if any, of @racket[elt]'s name.
}

@defproc[(element-namespace [elt element?])
         (or/c string? #f)]{
  Returns the resolved namespace, if any, of @racket[elt].
}

@defstruct[(exn:fail:xmlns exn:fail)
           ([element element?])]{
Raised for errors when expanding element and attribute names.
}
