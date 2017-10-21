#lang racket/base

(require racket/contract/base
         xml)

(provide
 (contract-out
  [attribute-name-prefix (-> attribute? (or/c #f symbol?))]
  [attribute-namespace (-> attribute? (or/c #f string?))]
  [element-name-prefix (-> element? (or/c #f symbol?))]
  [element-namespace (-> element? (or/c #f string?))]
  [xml-expand-names (-> document? document?)]
  [xml-expand-names/content (->* (content/c)
                                 ((hash/c (or/c #f symbol?)
                                          (or/c #f string?)
                                          #:flat? #t))
                                 content/c)])
 (struct-out exn:fail:xmlns))

(struct exn:fail:xmlns exn:fail (element))

(define (raise-xmlns-error element form . vs)
  (raise
   (exn:fail:xmlns
    (apply format form vs)
    (current-continuation-marks)
    element)))

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

(define (xml-expand-names doc)
  (struct-copy document doc
               [element (xml-expand-names/content (document-element doc))]))

(define (xml-expand-names/content content [namespaces (hasheq #f #f)])
  (xml-expand-names/content/cache content namespaces (make-hasheq)))

(define (xml-expand-names/content/cache content namespaces name-cache)
  (cond
    [(element? content) (xml-expand-names/element/cache content namespaces name-cache)]
    [else content]))

(define (xml-expand-names/element/cache elt namespaces name-cache)
  (define (ns-ref namespaces ns-name)
    (hash-ref namespaces ns-name (λ () (raise-xmlns-error elt "undeclared prefix: ~a" ns-name))))
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
                     [else (raise-xmlns-error elt "invalid name: ~a" name)]))))
    (values (car ns+base) (cdr ns+base)))
  (define elt-namespaces
    (for/fold ([namespaces namespaces])
              ([att (in-list (element-attributes elt))])
      (define-values (ns-name base-name)
        (split-name (attribute-name att)))
      (if (eq? (or ns-name base-name) 'xmlns)
          (let ([ns (attribute-value att)])
            (hash-set namespaces
                      (and ns-name base-name)
                      (if (equal? "" ns)
                          (begin
                            (unless (not ns-name)
                              (raise-xmlns-error elt "empty namespace declaration"))
                            #f)
                          ns)))
          namespaces)))
  (define-values (elt-ns-name elt-type)
    (split-name (element-name elt)))
  (define elt-ns (ns-ref elt-namespaces elt-ns-name))
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
           prop:namespace (and ns-name (ns-ref elt-namespaces ns-name))))))
  (for/fold ([checked-attributes (hasheq)])
            ([att (in-list expanded-atts)])
    (define att-name (attribute-name att))
    (define att-ns (attribute-namespace att))
    (when (and att-ns (equal? att-ns (hash-ref checked-attributes att-name #f)))
      (raise-xmlns-error elt "duplicate attribute: ~a" att-name))
    (hash-set checked-attributes att-name att-ns))
  (define expanded-content
    (for/list ([content (in-list (element-content elt))])
      (xml-expand-names/content/cache content elt-namespaces name-cache)))
  (chaperone-struct
   (struct-copy element elt
                [name elt-type]
                [attributes expanded-atts]
                [content expanded-content])
   struct:element
   prop:name-prefix elt-ns-name
   prop:namespace elt-ns))

(module+ test
  (require rackunit)
  (define (extract-meta str)
    (define doc
      (xml-expand-names
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
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta "<:x />")))
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta "<x: xmlns:x='http://a.example.com/schema' />")))
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta "<: />")))
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta "<a:b:c xmlns:a='http://a.example.com/schema' xmlns:b='http://b.example.com/schema' />")))
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta "<unknown:x />")))
  (check-equal?
   (extract-meta "<x xmlns='' />")
   '(#f x #f () ()))
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta "<x xmlns:undeclare='' />")))
  (check-equal?
   (extract-meta #<<END
<html:html xmlns:html='http://www.w3.org/1999/xhtml'>
  <html:head><html:title>Frobnostication</html:title></html:head>
  <html:body><html:p>Moved to 
    <html:a href='http://frob.example.com'>here.</html:a></html:p></html:body>
</html:html>
END
)
   '(html html "http://www.w3.org/1999/xhtml"
          ()
          ((html head "http://www.w3.org/1999/xhtml"
                 ()
                 ((html title "http://www.w3.org/1999/xhtml" () ())))
           (html body "http://www.w3.org/1999/xhtml"
                 ()
                 ((html p "http://www.w3.org/1999/xhtml"
                        ()
                        ((html a "http://www.w3.org/1999/xhtml"
                               ((#f href #f))
                               ()))))))))
  (check-equal?
   (extract-meta #<<END
<bk:book xmlns:bk='urn:loc.gov:books'
         xmlns:isbn='urn:ISBN:0-395-36341-6'>
    <bk:title>Cheaper by the Dozen</bk:title>
    <isbn:number>1568491379</isbn:number>
</bk:book>
END
)
   '(bk book "urn:loc.gov:books"
        ()
        ((bk title "urn:loc.gov:books" () ())
         (isbn number "urn:ISBN:0-395-36341-6" () ()))))
  (check-equal?
   (extract-meta #<<END
<book xmlns='urn:loc.gov:books'
      xmlns:isbn='urn:ISBN:0-395-36341-6'>
    <title>Cheaper by the Dozen</title>
    <isbn:number>1568491379</isbn:number>
    <notes>
      <p xmlns='http://www.w3.org/1999/xhtml'>
          This is a <i>funny</i> book!
      </p>
    </notes>
</book>
END
)
   '(#f book "urn:loc.gov:books"
        ()
        ((#f title "urn:loc.gov:books" () ())
         (isbn number "urn:ISBN:0-395-36341-6" () ())
         (#f notes "urn:loc.gov:books"
             ()
             ((#f p "http://www.w3.org/1999/xhtml"
                  ()
                  ((#f i "http://www.w3.org/1999/xhtml" () ()))))))))
  (check-equal?
   (extract-meta #<<END
<Beers>
  <table xmlns='http://www.w3.org/1999/xhtml'>
   <th><td>Name</td><td>Origin</td><td>Description</td></th>
   <tr> 
     <td><brandName xmlns="">Huntsman</brandName></td>
     <td><origin xmlns="">Bath, UK</origin></td>
     <td>
       <details xmlns=""><class>Bitter</class><hop>Fuggles</hop>
         <pro>Wonderful hop, light alcohol, good summer beer</pro>
         <con>Fragile; excessive variance pub to pub</con>
         </details>
        </td>
      </tr>
    </table>
  </Beers>
END
)
   '(#f Beers #f
        ()
        ((#f table "http://www.w3.org/1999/xhtml"
             ()
             ((#f th "http://www.w3.org/1999/xhtml"
                  ()
                  ((#f td "http://www.w3.org/1999/xhtml" () ())
                   (#f td "http://www.w3.org/1999/xhtml" () ())
                   (#f td "http://www.w3.org/1999/xhtml" () ())))
              (#f tr "http://www.w3.org/1999/xhtml"
                  ()
                  ((#f td "http://www.w3.org/1999/xhtml"
                       ()
                       ((#f brandName #f () ())))
                   (#f td "http://www.w3.org/1999/xhtml"
                       ()
                       ((#f origin #f () ())))
                   (#f td "http://www.w3.org/1999/xhtml"
                       ()
                       ((#f details #f
                            ()
                            ((#f class #f () ())
                             (#f hop #f () ())
                             (#f pro #f () ())
                             (#f con #f () ()))))))))))))
  (check-exn
   exn:fail:xmlns?
   (λ () (extract-meta #<<END
<x xmlns:n1="http://www.w3.org" 
   xmlns:n2="http://www.w3.org" >
  <bad n1:a="1"  n2:a="2" />
</x>
END
)))
  (check-equal?
   (extract-meta #<<END
<x xmlns:n1="http://www.w3.org" 
   xmlns="http://www.w3.org" >
  <good a="1"     b="2" />
  <good a="1"     n1:a="2" />
</x>
END
)
   '(#f x "http://www.w3.org"
        ()
        ((#f good "http://www.w3.org"
             ((#f a #f)
              (#f b #f))
             ())
         (#f good  "http://www.w3.org"
             ((#f a #f)
              (n1 a "http://www.w3.org"))
             ())))))
