# linkbreedeR

R package for running genetic analysis tools (BLUPF90+, PLINK, RENUMF90) directly from R.

## Installation

```r
# Install from GitHub
devtools::install_github("Thymine2001/linkbreedeR")
```

## Usage

```r
library(linkbreedeR)

# Run BLUPF90+
run_blupf90("parameter_file.par")

# Run PLINK
run_plink("--bfile mydata --freq")
run_plink("--bfile mydata --assoc")
run_plink("--bfile 1 --make-bed --out 1")

# Run RENUMF90
run_renumf90("parameter_file.par")
```

## Features

- ✅ Cross-platform support (Windows, macOS, Linux)
- ✅ All binaries included - no separate installation needed
- ✅ Automatic OS detection
- ✅ Simple R interface

## Tools Included

- **BLUPF90+** - Genomic BLUP analysis
- **PLINK** - Genome-wide association studies and QC
- **RENUMF90** - Data renumbering and format conversion
