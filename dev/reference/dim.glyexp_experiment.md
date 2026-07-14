# Dimensions of an experiment

Retrieve the dimensions of an experiment object, i.e. the number of
variables and samples.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
dim(x)

# S3 method for class 'glyexp_experiment'
dim(x) <- value
```

## Arguments

- x:

  An experiment object.

- value:

  Ignored.

## Value

A vector with two elements: the number of variables and the number of
samples.
