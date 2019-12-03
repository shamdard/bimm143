class11
================
Shabnam Hamdard
11/5/2019

## The PDB database for biomolecular structure data

``` r
# Read CSV
data <- read.csv("Data Export Summary.csv", row.names = 1)
data
```

    ##                     Proteins Nucleic.Acids Protein.NA.Complex Other  Total
    ## X-Ray                 131278          2059               6759     8 140104
    ## NMR                    11235          1303                261     8  12807
    ## Electron Microscopy     2899            32                999     0   3930
    ## Other                    280             4                  6    13    303
    ## Multi Method             144             5                  2     1    152

``` r
x <- data$Total
x
```

    ## [1] 140104  12807   3930    303    152

``` r
y <- sum(x)

fraction <- x/y

fraction
```

    ## [1] 0.890702879 0.081419744 0.024984742 0.001926305 0.000966331

``` r
percentage <- fraction * 100

percentage
```

    ## [1] 89.0702879  8.1419744  2.4984742  0.1926305  0.0966331

Proportion of entries from each method

``` r
round(data$Total/sum(data$Total) * 100, 2)
```

    ## [1] 89.07  8.14  2.50  0.19  0.10

Proportion that are proteins

``` r
round(sum(data$Proteins)/sum(data$Total) * 100, 2)
```

    ## [1] 92.71

## HIV-Pr Structure Analysis

``` r
library(bio3d)

pdb <- read.pdb("1hsg.pdb")
pdb
```

    ## 
    ##  Call:  read.pdb(file = "1hsg.pdb")
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 1686,  XYZs#: 5058  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 172  (residues: 128)
    ##      Non-protein/nucleic resid values: [ HOH (127), MK1 (1) ]
    ## 
    ##    Protein sequence:
    ##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
    ##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
    ##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
    ##       VNIIGRNLLTQIGCTLNF
    ## 
    ## + attr: atom, xyz, seqres, helix, sheet,
    ##         calpha, remark, call

``` r
# read.pdb()
# atom.select()
# write.pdb()
# trim.pdb()
```

``` r
ligand <- atom.select(pdb, "ligand", value=TRUE)
write.pdb(ligand, file="1hsg_ligand.pdb")
```

``` r
protein <- atom.select(pdb, "protein", value=TRUE)
write.pdb(protein, file="1hsg_protein.pdb")
```
