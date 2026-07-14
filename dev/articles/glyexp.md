# Get Started with glyexp

In this context, you typically work with three types of data in
glycomics and glycoproteomics experiments:

1.  **Expression data (assay)** - the actual measurements of your
    biological molecules (glycans, glycopeptides, etc.)
2.  **Molecular annotations (row data)** - the identifiers for your
    molecules (structures, sequences, etc.)
3.  **Experimental metadata (column data)** - the context of your
    samples (time points, treatments, experimental conditions)

The `SummarizedExperiment` class in the `SummarizedExperiment` packag
serves as a structured container that keeps all three data types
organized and interconnected. `glyexp` provides two subclasses of
`SummarizedExperiment`: `GlycomicSE` for glycomics data, and
`GlycoproteomicSE` for glycoproteomics data.

## For those familiar with SummarizedExperiment

`GlycomicSE` and `GlycoproteomicSE` are both subclasses of
`SummarizedExperiment` with a few restrictions:

1.  `GlycomicSE` requires a `glycan_composition` column as a
    [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
    vector in the `rowData`.
2.  `GlycoproteomicSE` requires `glycan_composition`, as well as
    `protein` (character) and `protein_site` (integer) columns.
3.  There can be only one assay.

That’s all. All your familiar operations provided by
`SummarizedExperiment` and `tidySummarizedExperiment` can also be used
on `GlycomicSE` and `GlycoproteomicSE`.

## Getting Started with glyexp

Let’s begin with a simple example to illustrate the basic concepts.

``` r

# `real_experiment` is a built-in `GlycoproteomicSE` object
exp <- real_experiment
exp
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```

The summary provides an overview of your entire experiment - variables,
observations, and all the metadata.

### The Expression Matrix

The expression matrix contains your numerical data - rows are variables
(molecules), columns are observations (samples).

``` r

assay(exp)[1:5, 1:3]
#>                                              C1         C2           C3
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           NA         NA     10655.62
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1  414080036  609889761  78954431.49
#> P04196-344-Hex(5)HexNAc(4)            581723113  604842244 167889901.32
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 3299649335 2856490652 957651065.86
#> P10909-291-Hex(6)HexNAc(5)-1           30427048   34294394   6390129.81
```

### Variable Information

The variable information table (or “raw data”, in the
`SummarizedExperiment` terminology) contains detailed annotations for
each molecule.

``` r

head(rowData(exp))
#> DataFrame with 6 rows and 7 columns
#>                                             peptide peptide_site     protein
#>                                         <character>    <integer> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           NKTQGK            1      P08185
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1 HSHNNNSSDLHPHK            5      P04196
#> P04196-344-Hex(5)HexNAc(4)           HSHNNNSSDLHPHK            5      P04196
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 HSHNNNSSDLHPHK            5      P04196
#> P10909-291-Hex(6)HexNAc(5)-1               HNSTGCLR            2      P10909
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)   HSHNNNSSDLHPHK            5      P04196
#>                                      protein_site        gene
#>                                         <integer> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)            176    SERPINA6
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1          344         HRG
#> P04196-344-Hex(5)HexNAc(4)                    344         HRG
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2          344         HRG
#> P10909-291-Hex(6)HexNAc(5)-1                  291         CLU
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)            344         HRG
#>                                          glycan_composition
#>                                       <glyrepr_composition>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)   Hex(5)HexNAc(4)NeuAc..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1 Hex(5)HexNAc(4)NeuAc..
#> P04196-344-Hex(5)HexNAc(4)                  Hex(5)HexNAc(4)
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 Hex(5)HexNAc(4)NeuAc..
#> P10909-291-Hex(6)HexNAc(5)-1                Hex(6)HexNAc(5)
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)   Hex(5)HexNAc(4)NeuAc..
#>                                            glycan_structure
#>                                         <glyrepr_structure>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)   NeuAc(??-?)Hex(??-?)..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1 NeuAc(??-?)Hex(??-?)..
#> P04196-344-Hex(5)HexNAc(4)           Hex(??-?)HexNAc(??-?..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 NeuAc(??-?)Hex(??-?)..
#> P10909-291-Hex(6)HexNAc(5)-1         Hex(??-?)HexNAc(??-?..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)   NeuAc(??-?)Hex(??-?)..
```

### Sample Information

The sample information table (or “column data”) records the experimental
conditions for each sample.

``` r

head(colData(exp))
#> DataFrame with 6 rows and 1 column
#>       group
#>    <factor>
#> C1        C
#> C2        C
#> C3        C
#> H1        H
#> H2        H
#> H3        H
```

### Rownames and colnames

Row names in
[`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
and
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
match the row and column names in the assay, repectively.

``` r

identical(rownames(rowData(exp)), rownames(assay(exp)))
#> [1] TRUE
identical(rownames(colData(exp)), colnames(assay(exp)))
#> [1] TRUE
```

As a shorthand, you can just use
[`rownames()`](https://rdrr.io/r/base/colnames.html) and
[`colnames()`](https://rdrr.io/r/base/colnames.html) on the container.

``` r

rownames(exp)[1:5]
#> [1] "P08185-176-Hex(5)HexNAc(4)NeuAc(2)"  
#> [2] "P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1"
#> [3] "P04196-344-Hex(5)HexNAc(4)"          
#> [4] "P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2"
#> [5] "P10909-291-Hex(6)HexNAc(5)-1"
colnames(exp)[1:5]
#> [1] "C1" "C2" "C3" "H1" "H2"
```

## Data Manipulation with glyexp

glyexp provides dplyr-style functions for manipulating
`SummarizedExperiment` objects.

For every dplyr function, glyexp provides two specialized versions:

- **`_col()`** functions: work on your sample metadata
- **`_row()`** functions: work on your variable annotations

Here’s an example of filtering for group “A” samples:

``` r

subset_exp <- filter_col(exp, group == "H")
```

Let’s check what happened to our sample info:

``` r

colData(subset_exp)
#> DataFrame with 3 rows and 1 column
#>       group
#>    <factor>
#> H1        H
#> H2        H
#> H3        H
```

Check the expression matrix:

``` r

assay(subset_exp)[1:5,]
#>                                                H1         H2           H3
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)   3.105412e+04         NA     457398.3
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1           NA   11724908    3892287.5
#> P04196-344-Hex(5)HexNAc(4)           6.977076e+08  703566323  421872371.5
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 2.600523e+09 3229968280 2653736044.6
#> P10909-291-Hex(6)HexNAc(5)-1         5.159133e+07   37479075   62332706.5
```

The expression matrix is automatically filtered to match!

This is
[`filter_col()`](https://glycoverse.github.io/glyexp/dev/reference/filter_col.md):
it filters the sample information and automatically updates the
expression matrix to match.

Variable filtering works the same way:

``` r

exp |> filter_col(group == "H")
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 3 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```

Notice how these functions support the pipe operator (`|>`)? That’s the
`dplyr` DNA in action!

The pattern is straightforward: glyexp functions expect and return
`SummarizedExperiment` objects, including `GlycomicSE` and
`GlycoproteomicSE`, and they preserve the row and column identifiers
during operations.

To learn more about `dplyr`-style operations, check out [this
vignette](https://glycoverse.github.io/glyexp/articles/dplyr-style-functions.html).
To learn basic `SummarizedExperiment` operations, check out [this
vignette](https://bioconductor.org/packages//release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html).

## Background and Design Principles

glyexp builds on insights from related data containers:

**SummarizedExperiment** The foundational omics data container from
Bioconductor. Well-established for RNA-seq analysis.

**tidySummarizedExperiment** An attempt to bring tidy principles to
SummarizedExperiment from the tidySummarizedExperiment package. While
the concept is sound, storing all components in a single tibble doesn’t
align with the mental model of separated data types.

**massdataset** A related package for mass spectrometry data. It
provides tidy operations, clean data separation, and data processing
history tracking. We appreciate its approach to reproducibility.

While object-oriented programming has its merits, glyexp takes a
functional programming approach. Your analysis code serves as the
reproducibility record - clear, transparent, and familiar to R users.

**Design Philosophy** glyexp uses functional programming because it
aligns with how R users work. The design emphasizes clear, chainable
functions.

Thank you to all the developers who contributed to these foundational
packages.

## What’s Next?

Now you have the basic understanding of `glyexp`. Next, you can learn
how to use it in your analysis.

- [Creating
  Experiments](https://glycoverse.github.io/glyexp/articles/create-exp.html)
- [Dplyr-Style
  Functions](https://glycoverse.github.io/glyexp/articles/dplyr-style-functions.html)
