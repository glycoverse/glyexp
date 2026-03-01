# glyexp: Glycoproteomics and Glycomics Experiments

Provides a tidy data framework for managing glycoproteomics and
glycomics experimental data. The core feature is the 'experiment()'
class, which serves as a unified data container integrating expression
matrices, variable information (proteins, peptides, glycan compositions,
etc.), and sample metadata (groups, batches, clinical variables, etc.).
The package enforces data consistency, validates column types according
to experiment types (glycomics, glycoproteomics, traitomics,
traitproteomics), and provides dplyr-style data manipulation functions
(filter, mutate, select, arrange, slice, join) for seamless data
wrangling. As the data core of the 'glycoverse' ecosystem, it provides a
consistent interface that other packages can reliably extract
information from, enabling smooth data exchange and analysis workflows.

## See also

Useful links:

- <https://glycoverse.github.io/glyexp/>

- <https://github.com/glycoverse/glyexp>

- Report bugs at <https://github.com/glycoverse/glyexp/issues>

## Author

**Maintainer**: Bin Fu <23110220018@m.fudan.edu.cn>
([ORCID](https://orcid.org/0000-0001-8567-2997)) \[copyright holder\]
