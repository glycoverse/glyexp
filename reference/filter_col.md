# Filter samples or variables of an experiment

Getting a subset of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
or `SummarizedExperiment` by filtering samples or variables.

The same syntax as
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
is used. For example, to get a subset of an experiment keeping only "HC"
samples, use `filter_col(exp, group == "HC")`. This actually calls
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
on the sample information tibble with condition `group == "HC"`, and
then updates the expression matrix accordingly.

## Usage

``` r
filter_col(exp, ..., .drop_levels = FALSE)

filter_row(exp, ..., .drop_levels = FALSE)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  or `SummarizedExperiment` object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expression to filter samples or variables. passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  internally.

- .drop_levels:

  Logical. If `TRUE`, drop unused factor levels for columns referenced
  in the filtering expressions.

## Value

An object of the same class as `exp`.

## Details

One difference between `filter_col()` or `filter_row()` and
[dplyr::filter](https://dplyr.tidyverse.org/reference/filter.html) is
that, when filtering on factor columns, unused levels can be dropped by
setting `.drop_levels` to `TRUE`.

## Identifier columns

For an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object, `sample` is a physical column in `sample_info`, and `variable`
is a physical column in `var_info`.

For a `SummarizedExperiment`, sample and variable identifiers live in
`colnames(exp)` and `rownames(exp)`, rather than in
[`SummarizedExperiment::colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
or
[`SummarizedExperiment::rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
Observation verbs expose `colnames(exp)` as a virtual `.sample` column,
and variable verbs expose `rownames(exp)` as a virtual `.variable`
column. These dot-prefixed names distinguish dimension identifiers from
regular metadata columns. After the operation, the virtual column is
removed and its values are written back to the corresponding dimension
names.

Consequently, `sample` in `colData(exp)` and `variable` in
`rowData(exp)` remain ordinary metadata columns. The names `.sample` and
`.variable` are reserved; an input containing either name in the
corresponding metadata raises an error rather than overwriting that
column.

## Examples

``` r
library(SummarizedExperiment)

# Add a variable annotation to a bundled experiment
exp <- real_experiment |>
  mutate_row(type = "glycopeptide")

# Filter samples
sub_exp_1 <- filter_col(exp, group == "H")
colData(sub_exp_1)
#> DataFrame with 3 rows and 1 column
#>       group
#>    <factor>
#> H1        H
#> H2        H
#> H3        H

# Filter variables
sub_exp_2 <- filter_row(exp, type == "glycopeptide")
rowData(sub_exp_2)
#> DataFrame with 4262 rows and 8 columns
#>                                                             peptide
#>                                                         <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)                           NKTQGK
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1                 HSHNNNSSDLHPHK
#> P04196-344-Hex(5)HexNAc(4)                           HSHNNNSSDLHPHK
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2                 HSHNNNSSDLHPHK
#> P10909-291-Hex(6)HexNAc(5)-1                               HNSTGCLR
#> ...                                                             ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)           NCGVNCSGDVFTALIGEIAS..
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2          QDQCIYNTTYLNVQR
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2               ALPQPQNVTSLLGCTH
#> P01008-187-Hex(12)HexNAc(2)                     SLTFNETYQDISELVYGAK
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2    VSNQTLSLFFTVLQDVPVR
#>                                              peptide_site     protein
#>                                                 <integer> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)                      1      P08185
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1                    5      P04196
#> P04196-344-Hex(5)HexNAc(4)                              5      P04196
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2                    5      P04196
#> P10909-291-Hex(6)HexNAc(5)-1                            2      P10909
#> ...                                                   ...         ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)                      5      P09871
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2              7      P02763
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2                    7      P02790
#> P01008-187-Hex(12)HexNAc(2)                             5      P01008
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2            3      P01023
#>                                              protein_site        gene
#>                                                 <integer> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)                    176    SERPINA6
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1                  344         HRG
#> P04196-344-Hex(5)HexNAc(4)                            344         HRG
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2                  344         HRG
#> P10909-291-Hex(6)HexNAc(5)-1                          291         CLU
#> ...                                                   ...         ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)                    174         C1S
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2             93        ORM1
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2                  453         HPX
#> P01008-187-Hex(12)HexNAc(2)                           187    SERPINC1
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2         1424         A2M
#>                                                  glycan_composition
#>                                               <glyrepr_composition>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           Hex(5)HexNAc(4)NeuAc..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1         Hex(5)HexNAc(4)NeuAc..
#> P04196-344-Hex(5)HexNAc(4)                          Hex(5)HexNAc(4)
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2         Hex(5)HexNAc(4)NeuAc..
#> P10909-291-Hex(6)HexNAc(5)-1                        Hex(6)HexNAc(5)
#> ...                                                             ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)           Hex(5)HexNAc(4)NeuAc..
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2   Hex(7)HexNAc(6)dHex(..
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2         Hex(6)HexNAc(5)NeuAc..
#> P01008-187-Hex(12)HexNAc(2)                        Hex(12)HexNAc(2)
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2 Hex(5)HexNAc(4)dHex(..
#>                                                    glycan_structure
#>                                                 <glyrepr_structure>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           NeuAc(??-?)Hex(??-?)..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1         NeuAc(??-?)Hex(??-?)..
#> P04196-344-Hex(5)HexNAc(4)                   Hex(??-?)HexNAc(??-?..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2         NeuAc(??-?)Hex(??-?)..
#> P10909-291-Hex(6)HexNAc(5)-1                 Hex(??-?)HexNAc(??-?..
#> ...                                                             ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)           NeuAc(??-?)Hex(??-?)..
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2   NeuAc(??-?)Hex(??-?)..
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2         NeuAc(??-?)Hex(??-?)..
#> P01008-187-Hex(12)HexNAc(2)                  Hex(??-?)Hex(??-?)He..
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2 NeuAc(??-?)Hex(??-?)..
#>                                                      type
#>                                               <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           glycopeptide
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1         glycopeptide
#> P04196-344-Hex(5)HexNAc(4)                   glycopeptide
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2         glycopeptide
#> P10909-291-Hex(6)HexNAc(5)-1                 glycopeptide
#> ...                                                   ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)           glycopeptide
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2   glycopeptide
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2         glycopeptide
#> P01008-187-Hex(12)HexNAc(2)                  glycopeptide
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2 glycopeptide

# Use pipe
sub_exp_3 <- exp |>
  filter_col(group == "H") |>
  filter_row(type == "glycopeptide")
sub_exp_3
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 3 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, type <chr>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
