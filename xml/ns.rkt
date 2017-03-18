#lang racket/base

(require racket/contract/base
         xml)

(define-values (prop:name-prefix has-name-prefix? get-name-prefix)
  (make-impersonator-property 'name-prefix))

(define-values (prop:namespace has-namespace? get-namespace)
  (make-impersonator-property 'namespace))

(define (attribute-name-prefix att)
  (and (has-name-prefix? att) (get-name-prefix att)))

(define (attribute-namespace att)
  (and (has-namespace? att) (get-namespace att)))

(define (element-name-prefix elt)
  (and (has-name-prefix? elt) (get-name-prefix elt)))

(define (element-namespace elt)
  (and (has-namespace? elt) (get-namespace elt)))

(define (expand-names doc)
  (struct-copy document doc
               [element (expand-names/content (document-element doc))]))

(define (expand-names/content content [namespaces (hasheq #f #f)])
  (expand-names/content/cache content namespaces (make-hasheq)))

(define (expand-names/content/cache content namespaces name-cache)
  (define (get-ns namespaces ns-name)
    (hash-ref namespaces ns-name (λ () (error "unknown namespace:" ns-name))))
  (define (split-name name)
    (define ns+base
      (hash-ref! name-cache
                 name
                 (λ ()
                   (cond
                     [(regexp-match #rx"^(([^:]+):)?([^:]+)$"
                                    (symbol->string name))
                      => (λ (m)
                           (define ns-name (list-ref m 2))
                           (cons (and ns-name (string->symbol ns-name))
                                 (string->symbol (list-ref m 3))))]
                     [else (error "invalid name:" name)]))))
    (values (car ns+base) (cdr ns+base)))
  (cond
    [(element? content)
     (define elt content)
     (define elt-namespaces
       (for/fold ([namespaces namespaces])
                 ([att (in-list (element-attributes elt))])
         (define-values (ns-name base-name)
           (split-name (attribute-name att)))
         (if (eq? (or ns-name base-name) 'xmlns)
             (let ([ns (attribute-value att)])
               (hash-set namespaces
                         (and ns-name base-name)
                         (if (equal? "" ns) #f ns)))
             namespaces)))
     (define-values (elt-ns-name elt-type)
       (split-name (element-name elt)))
     (define elt-ns (get-ns elt-namespaces elt-ns-name))
     (define expanded-atts
       (for/list ([att (in-list (element-attributes elt))])
         (define-values (ns-name base-name)
           (split-name (attribute-name att)))
         (if (eq? (or ns-name base-name) 'xmlns)
             att
             (chaperone-struct
              (struct-copy attribute att
                           [name base-name])
              struct:attribute
              prop:name-prefix ns-name
              prop:namespace (and ns-name (get-ns elt-namespaces ns-name))))))
     (define expanded-content
       (for/list ([content (in-list (element-content elt))])
         (expand-names/content/cache content elt-namespaces name-cache)))
     (chaperone-struct
      (struct-copy element elt
                   [name elt-type]
                   [attributes expanded-atts]
                   [content expanded-content])
      struct:element
      prop:name-prefix elt-ns-name
      prop:namespace elt-ns)]
    [else content]))

(provide/contract
 [attribute-name-prefix (-> attribute? (or/c #f symbol?))]
 [attribute-namespace (-> attribute? (or/c #f string?))]
 [element-name-prefix (-> element? (or/c #f symbol?))]
 [element-namespace (-> element? (or/c #f string?))]
 [expand-names (-> document? document?)]
 [expand-names/content (->* (content/c)
                            ((hash/c (or/c #f symbol?)
                                     (or/c #f string?)
                                     #:flat? #t))
                            content/c)])

(module+ test
  (require rackunit)
  (define (extract-meta str)
    (define doc
      (expand-names
       (read-xml (open-input-string str))))
    (let next-elt ([e (document-element doc)])
      (list
       (element-name-prefix e)
       (element-name e)
       (element-namespace e)
       (for/list ([a (element-attributes e)]
                  #:unless (regexp-match? #rx"^xmlns$|^xmlns:" (symbol->string (attribute-name a))))
         (list (attribute-name-prefix a)
               (attribute-name a)
               (attribute-namespace a)))
       (for/list ([c (element-content e)]
                  #:when (element? c))
         (next-elt c)))))
  (define doc1
#<<END
<section xmlns='urn:com:books-r-us'>
  <title>Book-Signing Event</title>
  <signing>
    <author title="Mr" name="Vikram Seth" />
    <book title="A Suitable Boy" price="$22.95" />
  </signing>
</section>
END
    )
  (define doc1-meta
    '(#f section "urn:com:books-r-us"
         ()
         ((#f title "urn:com:books-r-us" () ())
          (#f signing "urn:com:books-r-us"
              ()
              ((#f author "urn:com:books-r-us"
                   ((#f title #f)
                    (#f name #f))
                   ())
               (#f book "urn:com:books-r-us"
                   ((#f title #f)
                    (#f price #f))
                   ()))))))
  (define doc2
#<<END
<RESERVATION xmlns:HTML="http://www.w3.org/TR/REC-html40">
<NAME HTML:CLASS="largeSansSerif">Layman, A</NAME>
<SEAT CLASS="Y" HTML:CLASS="largeMonotype">33B</SEAT>
<HTML:A HREF='/cgi-bin/ResStatus'>Check Status</HTML:A>
<DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>
END
    )
  (define doc2-meta
    '(#f RESERVATION #f
         ()
         ((#f NAME #f
              ((HTML CLASS "http://www.w3.org/TR/REC-html40"))
              ())
          (#f SEAT #f
              ((#f CLASS #f)
               (HTML CLASS "http://www.w3.org/TR/REC-html40"))
              ())
          (HTML A "http://www.w3.org/TR/REC-html40"
                ((#f HREF #f))
                ())
          (#f DEPARTURE #f () ()))))
  (check-equal? (extract-meta doc1) doc1-meta)
  (check-equal? (extract-meta doc2) doc2-meta))