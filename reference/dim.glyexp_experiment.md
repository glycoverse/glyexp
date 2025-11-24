# Dimensions of an experiment

Retrieve the dimensions of an experiment object, i.e. the number of
variables and samples.

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

## Examples

``` r
dim(real_experiment)
#> [1] 4262   12
```
