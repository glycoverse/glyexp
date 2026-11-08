# Keep glyco-omics names valid during SE operations

These methods preserve the complete, unique axis-name contract of
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
and
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
during common SE operations. Repeated row or column selections, and
binding objects with overlapping names on the bound axis, now error
instead of returning an invalid object. Returned glyco-omics objects
must also retain their required annotation fields. Use distinct
observation identifiers before binding; keep repeated biological labels
in annotation columns. Subsetting to zero rows or columns is supported.

## Usage

``` r
# S4 method for class 'GlycomicSE'
names(x) <- value

# S4 method for class 'GlycomicSE'
rowData(x, ...) <- value

# S4 method for class 'GlycomicSE,DataFrame'
colData(x, ...) <- value

# S4 method for class 'GlycomicSE,NULL'
colData(x, ...) <- value

# S4 method for class 'GlycomicSE,list'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'GlycomicSE,SimpleList'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'GlycomicSE,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'GlycomicSE,ANY,ANY,SummarizedExperiment'
x[i, j, ...] <- value

# S4 method for class 'GlycomicSE'
rbind(..., deparse.level = 1)

# S4 method for class 'GlycomicSE'
cbind(..., deparse.level = 1)

# S4 method for class 'GlycoproteomicSE'
names(x) <- value

# S4 method for class 'GlycoproteomicSE'
rowData(x, ...) <- value

# S4 method for class 'GlycoproteomicSE,DataFrame'
colData(x, ...) <- value

# S4 method for class 'GlycoproteomicSE,NULL'
colData(x, ...) <- value

# S4 method for class 'GlycoproteomicSE,list'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'GlycoproteomicSE,SimpleList'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'GlycoproteomicSE,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'GlycoproteomicSE,ANY,ANY,SummarizedExperiment'
x[i, j, ...] <- value

# S4 method for class 'GlycoproteomicSE'
rbind(..., deparse.level = 1)

# S4 method for class 'GlycoproteomicSE'
cbind(..., deparse.level = 1)
```

## Arguments

- x:

  A glyco-omics SE object.

- value:

  Replacement names, annotations, assays, or a subexperiment.

- ...:

  Additional arguments passed to the parent SE method, or objects to
  bind.

- withDimnames:

  Whether the parent SE method checks assay names.

- i, j:

  Row and column indices.

- drop:

  Passed to the parent SE method.

- deparse.level:

  Passed to the parent binding method.

## Value

A valid glyco-omics SE object.

## Details

`names(x) <- value` is equivalent to replacing row names. Replacing
`colData` follows SE's positional semantics and synchronizes its row
names to the assay; supplied names must be complete and unique. To align
differently ordered components by name, use the constructors.
Replacement assays may be unnamed or have names identical to the object
in the same order, even when `withDimnames = FALSE`; resolved names are
stored on the replacement assay.

`combineRows()` and `combineCols()` retain their upstream behavior: they
return an ordinary `SummarizedExperiment`, outside this subclass
contract. Convert the result explicitly to restore glyco-omics
validation.
