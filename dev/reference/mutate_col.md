# Mutate sample or variable information

Mutate the sample or variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
or `SummarizedExperiment`.

The same syntax as
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
is used. For example, to add a new column to the sample information
tibble, use `mutate_col(exp, new_column = value)`. This actually calls
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
on the sample information tibble with `new_column = value`.

If an identifier column is modified, its new values must be unique;
otherwise, an error is thrown. The assay column names or row names will
be updated accordingly.

## Usage

``` r
mutate_col(exp, ...)

mutate_row(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  or `SummarizedExperiment` object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs, passed to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  internally.

## Value

An object of the same class as `exp`.

## Identifier columns

For an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
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

If the corresponding dimension names are `NULL`, the virtual identifier
is unavailable and referring to it raises an error.
`mutate_col(.sample = ...)` or `mutate_row(.variable = ...)` can be used
to create the missing names.

## Examples

``` r
library(SummarizedExperiment)

# Add metadata to a bundled experiment
exp <- real_experiment |>
  mutate_row(type = "glycopeptide")

# Add a new column to sample information tibble or variable information tibble
exp |>
  mutate_col(new_column = 1) |>
  colData()
#> DataFrame with 12 rows and 2 columns
#>        group new_column
#>     <factor>  <numeric>
#> C1         C          1
#> C2         C          1
#> C3         C          1
#> H1         H          1
#> H2         H          1
#> ...      ...        ...
#> M2         M          1
#> M3         M          1
#> Y1         Y          1
#> Y2         Y          1
#> Y3         Y          1

exp |>
  mutate_row(new_column = "A") |>
  rowData()
#> DataFrame with 4262 rows and 9 columns
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
#>                                                      type  new_column
#>                                               <character> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           glycopeptide           A
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1         glycopeptide           A
#> P04196-344-Hex(5)HexNAc(4)                   glycopeptide           A
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2         glycopeptide           A
#> P10909-291-Hex(6)HexNAc(5)-1                 glycopeptide           A
#> ...                                                   ...         ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)           glycopeptide           A
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2   glycopeptide           A
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2         glycopeptide           A
#> P01008-187-Hex(12)HexNAc(2)                  glycopeptide           A
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2 glycopeptide           A

# Modify existing columns
exp |>
  mutate_col(group = dplyr::if_else(group == "H", "healthy", "other")) |>
  colData()
#> DataFrame with 12 rows and 1 column
#>           group
#>     <character>
#> C1        other
#> C2        other
#> C3        other
#> H1      healthy
#> H2      healthy
#> ...         ...
#> M2        other
#> M3        other
#> Y1        other
#> Y2        other
#> Y3        other

exp |>
  mutate_row(type = dplyr::if_else(type == "glycopeptide", "good", "bad")) |>
  rowData()
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
#>                                                    glycan_structure        type
#>                                                 <glyrepr_structure> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           NeuAc(??-?)Hex(??-?)..        good
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1         NeuAc(??-?)Hex(??-?)..        good
#> P04196-344-Hex(5)HexNAc(4)                   Hex(??-?)HexNAc(??-?..        good
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2         NeuAc(??-?)Hex(??-?)..        good
#> P10909-291-Hex(6)HexNAc(5)-1                 Hex(??-?)HexNAc(??-?..        good
#> ...                                                             ...         ...
#> P09871-174-Hex(5)HexNAc(4)NeuAc(1)           NeuAc(??-?)Hex(??-?)..        good
#> P02763-93-Hex(7)HexNAc(6)dHex(1)NeuAc(4)-2   NeuAc(??-?)Hex(??-?)..        good
#> P02790-453-Hex(6)HexNAc(5)NeuAc(3)-2         NeuAc(??-?)Hex(??-?)..        good
#> P01008-187-Hex(12)HexNAc(2)                  Hex(??-?)Hex(??-?)He..        good
#> P01023-1424-Hex(5)HexNAc(4)dHex(1)NeuAc(1)-2 NeuAc(??-?)Hex(??-?)..        good

# SummarizedExperiment identifiers use virtual dot-prefixed columns
mutate_col(exp, .sample = paste0("new_", .sample))
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, type <chr>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
mutate_row(exp, .variable = paste0("new_", .variable))
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, type <chr>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
