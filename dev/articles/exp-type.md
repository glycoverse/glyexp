# Experiment Types

This vignette describes the four experiment types in `glyexp`:
“glycomics”, “glycoproteomics”, “traitomics”, and “traitproteomics”. You
can check your experiment type using
[`get_exp_type()`](https://glycoverse.github.io/glyexp/dev/reference/get_meta_data.md).

``` r
library(glyexp)
library(glyrepr)
```

## Why Experiment Types Matter

Experiment types refer to:

1.  A label stored in the `exp_type` field of your
    [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
    object’s metadata
2.  The expected structure of your variable information table

When your experiment has a specific type, glycoverse functions know what
to expect from your data structure. These are guidelines rather than
strict rules.

**Note:** If you’re not using `glydet` or `glymotif`, you can skip the
`traitomics` and `traitproteomics` sections.

## The Four Experiment Types

The `glyexp` package recognizes four distinct experiment types:

- **“glycomics”**: Glycan-focused experiments
- **“glycoproteomics”**: Glycans with protein context
- **“traitomics”**: Derived traits from glycan data
- **“traitproteomics”**: Derived traits at the protein level

The first two are standard glycobiology experiments. When you use
`glyread` to import data, it automatically determines the type.

The last two are unique to the glycoverse ecosystem - they result from
functions in `glymotif` or `glydet`.

### Glycomics

“glycomics” is the most straightforward type. Each row in the variable
information table represents a single glycan (or a spectrum match for MS
data).

Expected columns in variable information table:

- `glycan_composition`: Glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object
- `glycan_structure`: (Optional) Glycan structure as a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object

``` r
get_var_info(real_experiment2)
#> # A tibble: 67 × 3
#>    variable                             glycan_composition      glycan_structure
#>    <glue>                               <comp>                  <struct>        
#>  1 Man(3)GlcNAc(3)                      Man(3)GlcNAc(3)         GlcNAc(?1-?)Man…
#>  2 Man(3)GlcNAc(7)                      Man(3)GlcNAc(7)         GlcNAc(?1-?)[Gl…
#>  3 Man(5)GlcNAc(2)                      Man(5)GlcNAc(2)         Man(?1-?)[Man(?…
#>  4 Man(4)Gal(2)GlcNAc(4)Neu5Ac(2)       Man(4)Gal(2)GlcNAc(4)N… Neu5Ac(?2-?)Gal…
#>  5 Man(3)Gal(1)GlcNAc(3)                Man(3)Gal(1)GlcNAc(3)   Gal(?1-?)GlcNAc…
#>  6 Man(3)Gal(2)GlcNAc(4)Fuc(2)          Man(3)Gal(2)GlcNAc(4)F… Gal(?1-?)GlcNAc…
#>  7 Man(3)GlcNAc(3)Fuc(1)                Man(3)GlcNAc(3)Fuc(1)   GlcNAc(?1-?)Man…
#>  8 Man(3)GlcNAc(4)                      Man(3)GlcNAc(4)         GlcNAc(?1-?)Man…
#>  9 Man(3)Gal(2)GlcNAc(5)Neu5Ac(1)       Man(3)Gal(2)GlcNAc(5)N… Neu5Ac(?2-?)Gal…
#> 10 Man(3)Gal(1)GlcNAc(5)Fuc(1)Neu5Ac(1) Man(3)Gal(1)GlcNAc(5)F… Neu5Ac(?2-?)Gal…
#> # ℹ 57 more rows
```

### Glycoproteomics

“glycoproteomics” adds protein context. Each row represents a
glycopeptide or glycoform, indicating not just what glycans are present
but where they’re attached.

Variable information table should include:

- `protein`: Protein UniProt accession (character string)
- `protein_site`: Glycosylation site position on the protein (integer)
- `glycan_composition`: Glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object
- `glycan_structure`: (Optional) Glycan structure as a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object

``` r
get_var_info(real_experiment)
#> # A tibble: 4,262 × 8
#>    variable   peptide peptide_site protein protein_site gene  glycan_composition
#>    <chr>      <chr>          <int> <chr>          <int> <chr> <comp>            
#>  1 P08185-N1… NKTQGK             1 P08185           176 SERP… Hex(5)HexNAc(4)Ne…
#>  2 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  3 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)   
#>  4 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  5 P10909-N2… HNSTGC…            2 P10909           291 CLU   Hex(6)HexNAc(5)   
#>  6 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  7 P04196-N3… HSHNNN…            6 P04196           345 HRG   Hex(5)HexNAc(4)   
#>  8 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)dH…
#>  9 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(4)HexNAc(3)   
#> 10 P04196-N3… HSHNNN…            5 P04196           344 HRG   Hex(4)HexNAc(4)Ne…
#> # ℹ 4,252 more rows
#> # ℹ 1 more variable: glycan_structure <struct>
```

### Traitomics

Both `traitomics` and `traitproteomics` emerge when you transform data
using `glydet::derive_traits()` or `glymotif::quantify_motifs()`.

When you apply `derive_traits()` to a `glycomics` experiment, you get a
`traitomics` experiment. Instead of measuring individual glycans, you’re
looking at derived traits - higher-level summaries of your glycan data.

The variable information table is flexible - no mandatory columns.
Common columns include:

- A `trait` column (from `derive_traits()`)
- A `motif` column (from `quantify_motifs()`)

### Traitproteomics

When you apply `derive_traits()` to a `glycoproteomics` experiment, you
get a `traitproteomics` experiment. Instead of tracking individual
glycans at each protein site, you measure derived traits at each site.

Essential columns:

- `protein`: Protein UniProt accession (character string)
- `protein_site`: Glycosylation site position (integer)

Plus:

- A `trait` column (from `derive_traits()`)
- A `motif` column (from `quantify_motifs()`)

In `glycoproteomics`, different sites can have different glycan
repertoires. In `traitproteomics`, every site is scored on the same set
of traits or motifs - standardized metrics across glycosylation sites.

## Do You Need to Worry About Experiment Types?

Not really! Experiment types work behind the scenes. You’ll encounter
them in:

1.  **Documentation** - References to specific experiment types
2.  **Error messages** - Like “Expecting a `glycoproteomics` experiment,
    but got a `traitproteomics` experiment”

Different functions have different expectations:

- `glymotif::quantify_motifs()` requires `glycomics` or
  `glycoproteomics` data
- `glystats::gly_enrich_go()` expects `glycoproteomics` or
  `traitproteomics` experiments

**When to set experiment type:** Manually when creating an experiment
object using
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).
Set the `exp_type` field to match your data structure.

**Warning:** Do not manually modify the `exp_type` field after creation.
This breaks the contract between your data and glycoverse functions.
