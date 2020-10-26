# XPath

Uses

1. find elements within an XML document
1. test whether particular element(s) exist in an XML document


Background

* A DSL for selecting nodes from an XML document
* Spec:
    * Versions (1.0 is defacto default)
        * As of 2020-04-09, current version is 3.1 https://www.w3.org/TR/xpath-3/
        * 1.0 spec: https://www.w3.org/TR/1999/REC-xpath-19991116/
        * Many/most implementations only support 1.0
            * e.g. Nokogiri depends on libxml2 which supports 1.0 and has [no plans for 2.0 support](https://www.mail-archive.com/xml@gnome.org/msg04082.html) because the spec changed a lot from 1.0 to 2.0
            * e.g. Firefox Gecko implements 1.0
    * defines _path expressions_
* can be used in any XML like document: XML, HTML, SVG
* implemented in all language stacks
    * not all browsers/stacks implement **all** features
* xpath has functions e.g.
    * `position()`
        * lets you choose an element by position
    * `number()`
    * `string()`
    * `string-length()`
    * ...
* syntax
    * starting points for the search
        * `//`
            * recursive descent operator
            * starting the path with `//` means _starting anywhere within the document_
        * `/`
            * selects the document root
            * ignores whatever context node you might have passed in to begin with
    * attribute names being with `@`
    * conditions go within `[]`
        * logical AND conditions by specifying them concurrently e.g. `//things[@type="good"][@size="big"]` will return all `<thing>` elements which have an attribute `type="good"` AND `size="big"`
        * OR ?
        * NOT ?
* axes
    * there are 13 axes
    * an axis represents a relationship to the _context node_ and is used to locate nodes relative to that node in the tree
    * not all implementations support all axes

* models the document as a tree of nodes
* types of nodes
    1. element nodes
    1. attribute node
    1. text node
* defines a way to compute a string value for each type of node
* some types of nodes have "names"
    * a "name" is a tuple of (local-part, namespace-uri)
        * the namespace-uri can be null
* xpath expressions return one of 4 basic types when evaluated
    1. node-set
        * unordered collection of nodes **without duplicates**
    1. boolean (true or false)
    1. number ( **floating point** number)
    1. string (a sequence of **UCS** characters)
* note that "a single node" is not a basic type - nodes are always in a node-set
* expressions are evaluated with respect to a **context** (think of it like "context" in the sense of evaluating a function in a lang with lexical scope
* context consists of
    1. a node (the context node)
    1. a pair of non-zero positive integers (context-position, cotext-size)
        * context-position is always less than or equal to context size
    1. a set of variable bindings
        * a mapping from variable names to variable values
        * variable values can have any type valid for the return value of an expression **and** other types (???)
    1. a function library
        * a mapping of function names to functions
        * each function takes 0 or more args
        * each function returns one of the 4 basic types (see above)
        * the spec defines a set of core functions
        * XSLT and XPointer define additional functions and data types
    1. a set of namespace declarations in scope for the expression
        * a mapping from prefixes to namespace URIs

* the expression is a set of _location steps_ each separated by `/`
* the set of nodes selected by each step becomes the context node for the location step to it's right e.g.
    * example:
        ```
        a/b/c
        ```
    * the nodes selected by a become the context node for b
    * the nodes selecte by b become the context node for c and so on
* each _location step_ has 3 parts:
    1. an axis
        * specifies the _tree relationship_ between the nodes selected by this location step and the context node
    2. a node test
        * specifies _node type_ and _expanded name_ of the nodes selected by this location step
    3. 0-many predicates
        * use arbitrary expressions to further refine the set of nodes selected
