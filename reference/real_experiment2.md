# Real glycomics experiment

A real glycomics experiment object. This dataset is derived from the
unpublished glycoPOST dataset GPST000313. It contains serum N-glycome
profiles from 144 samples with different liver conditions: healthy (H),
hepatitis (M), cirrhosis (Y), and hepatocellular carcinoma (C).

## Usage

``` r
real_experiment2
```

## Format

An
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object with 67 variables and 144 samples.

### Variable information:

- `glycan_composition`: glycan composition
  (glyrepr::glycan_composition())

- `glycan_structure`: glycan structure (glyrepr::glycan_structure())

### Sample information:

- `sample`: sample ID

- `group`: disease group, one of "H" (healthy), "M" (hepatitis), "Y"
  (cirrhosis), and "C" (hepatocellular carcinoma)
