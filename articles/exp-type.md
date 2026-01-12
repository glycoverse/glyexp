# Experiment Types

Welcome to the world of glycoverse experiments! This vignette will walk
you through the four distinct experiment types in the `glyexp` package:
“glycomics”, “glycoproteomics”, “traitomics”, and “traitproteomics”.
Think of these as different flavors of glycobiology data, each with
their own personality and purpose. You can check what type your
experiment is using
[`get_exp_type()`](https://glycoverse.github.io/glyexp/reference/get_meta_data.md).

``` r
library(glyexp)
library(glyrepr)
```

## What’s the big deal with experiment types?

When we talk about experiment types, we’re really talking about two
things:

1.  A label stored in the `exp_type` field of your
    [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
    object’s metadata
2.  The expected structure of your variable information table

Here’s the thing: when your experiment has a specific type, all the
functions in glycoverse know what to expect from your data structure.
It’s like having a contract between your data and the analysis
functions. Don’t worry though— these are more like friendly guidelines
than iron-clad rules. We’ll show you what this means in practice with
some examples.

**Quick tip:** If you’re not diving into `glydet` or `glymotif`, you can
safely skip the `traitomics` and `traitproteomics` sections and focus on
the first two types.

## Meet the four experiment types

The `glyexp` package recognizes four distinct experiment types:

- **“glycomics”** - Your classic glycan-focused experiments
- **“glycoproteomics”** - When glycans meet proteins  
- **“traitomics”** - Derived traits from glycan data
- **“traitproteomics”** - Derived traits at the protein level

The first two are your bread-and-butter glycobiology experiments. When
you use `glyread` to import your data, it’ll automatically figure out
whether you’re dealing with glycomics or glycoproteomics and set the
type accordingly— pretty neat, right?

The last two are special creatures unique to the glycoverse ecosystem.
They’re what you get when functions from `glymotif` or `glydet` work
their magic on your data. Let’s dive into each type to see what makes
them tick.

### Glycomics

Let’s start with the most straightforward type: “glycomics”. This is
your classic glycan-focused experiment where each row in the variable
information table represents a single glycan (or a spectrum match if
you’re working with MS data).

What to expect in your variable information table:

- `glycan_composition`: The glycan’s composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object
- `glycan_structure`: (Optional) The glycan’s structure as a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object

Here’s what this looks like in practice:

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

Now we step up to “glycoproteomics”—where things get more interesting!
Here, each row represents a glycopeptide or glycoform (or their peptide
spectrum matches). Think of it as glycomics data with protein context,
telling you not just what glycans are there, but where they’re attached.

Your variable information table should include:

- `protein`: The protein UniProt accession (character string)
- `protein_site`: The glycosylation site position on the protein
  (integer)
- `glycan_composition`: The glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object
- `glycan_structure`: (Optional) The glycan structure as a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object

Here’s a real example to illustrate:

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

Here’s where things get uniquely glycoverse! Both `traitomics` and
`traitproteomics` are special experiment types that emerge when you
transform your data using `glydet::derive_traits()` or
`glymotif::quantify_motifs()`.

Starting with `traitomics`: when you feed a `glycomics` experiment to
`derive_traits()`, you get back a `traitomics` experiment. Instead of
measuring individual glycans in each sample, you’re now looking at
derived traits—think of it as a higher-level summary of your glycan
data.

The variable information table is pretty flexible here—no mandatory
columns. However, you’ll typically see:

- A `trait` column (if using `derive_traits()`)
- A `motif` column (if using `quantify_motifs()`)

### Traitproteomics

Last but not least, we have `traitproteomics`—the most sophisticated of
our experiment types. When you apply `derive_traits()` to a
`glycoproteomics` experiment, you get a `traitproteomics` experiment
back.

Here’s the key insight: instead of tracking individual glycans at each
protein site, you’re now measuring derived traits at each site. Think of
each glycosylation site as having its own little glycome, and you’re
summarizing the characteristics of that mini-glycome.

Essential columns in the variable information table:

- `protein`: The protein UniProt accession (character string)  
- `protein_site`: The glycosylation site position (integer)

Plus you’ll get:

- A `trait` column (from `derive_traits()`)
- A `motif` column (from `quantify_motifs()`)

Here’s a key difference: in `glycoproteomics`, different sites can have
completely different glycan repertoires. But in `traitproteomics`, every
site gets scored on the same set of traits or motifs—it’s like having
standardized metrics across all your glycosylation sites.

## Do you need to worry about experiment types?

The short answer? Not really! Experiment types work behind the scenes
quite intuitively. You’ll mostly encounter them in two situations:

1.  **Reading documentation** - When you see references to specific
    experiment types
2.  **Error messages** - Like “Expecting a `glycoproteomics` experiment,
    but got a `traitproteomics` experiment”

The reason these types matter is that different functions have different
expectations. For instance:

- `glymotif::quantify_motifs()` wants `glycomics` or `glycoproteomics`
  data  
- `glystats::gly_enrich_go()` expects `glycoproteomics` or
  `traitproteomics` experiments

**When you might need to care:** The main scenario is when you’re
manually creating an experiment object using
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
In that case, you’ll want to set the `exp_type` field to match your data
structure.

**Important warning:** Once you’ve created an experiment object, resist
the temptation to manually fiddle with the `exp_type` field. This will
break the implicit contract between your data and the glycoverse
functions—and nobody wants that!
