All contents of this repository (and associated website) are licensed under the [CC-BY license](https://creativecommons.org/licenses/by/4.0/). Each published paper has its own citation. To cite the data, please use:

Green, E.P. (2018). Nivi RED data repository. [![DOI](forthcoming)]()

* * * 

To replicate papers and analyses:

1. Clone this repository.
2. Working directory should be the root of this repo (not `data and replication files`).
3. Open the `manuscript.Rnw` file in `data and replication files/journal` in RStudio. 
4. `manuscript.Rnw` will run `data and replication files/scripts/analysis.R`, which loads data from `data and replication files/input` and outputs to `data and replication files/output`.

System requirements:

1. R
2. RStudio running knitr
3. R packages, see `data and replication files/scripts/analysis.R` file for details
4. LaTeX