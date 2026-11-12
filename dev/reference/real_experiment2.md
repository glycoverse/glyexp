# Real glycomics experiment

A real glycomics experiment object. This dataset is derived from the
GlycoPOST dataset GPST000584. It contains serum N-glycome profiles from
144 samples with different liver conditions: healthy (H), hepatitis (M),
cirrhosis (Y), and hepatocellular carcinoma (C).

## Usage

``` r
real_experiment2
```

## Format

An
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
object with 67 variables and 144 samples.

### Variable information:

- `glycan_composition`: glycan composition
  (glyrepr::glycan_composition())

- `glycan_structure`: glycan structure (glyrepr::glycan_structure())

### Sample information:

- `group`: disease group, one of "H" (healthy), "M" (hepatitis), "Y"
  (cirrhosis), and "C" (hepatocellular carcinoma)

## References

Fu, B. et al. (2026). Large-scale serum N-glycomics tracks
N-glycosylation dynamics in hepatocellular carcinoma progression and
enables early diagnosis. *Nature Communications*, 17, 1885.
[doi:10.1038/s41467-026-68579-x](https://doi.org/10.1038/s41467-026-68579-x)
