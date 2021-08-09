---
editor_options: 
  markdown: 
    wrap: 72
---

# Depth diversity gradients of macrophytes: shape, drivers and recent shifts - Research Compendium

<!-- badges: start -->

<!-- badges: end -->

Anne Lewerentz¹\*, Markus Hoffmann², Juliano Sarmento Cabral¹

¹ Ecosystem Modelling, Center for Computational and Theoretical Ecology
(CCTB), University of Würzburg, Clara-Oppenheimer-Weg 32, 97074
Würzburg, Germany, ² Limnological Station, Chair of Aquatic Systems
Biology, Technical University of Munich, Germany

\* Corresponding author:
[anne.lewerentz\@uni-wuerzburg.de](mailto:anne.lewerentz@uni-wuerzburg.de)

This repository is a R package which includes all data, analysis files
and results to reproduce the data analysis of Lewerentz et al 2021.

## Journal reference
Research Compendium *Release 0.9* corresponds to *Preprint*:

* Anne Lewerentz, Markus Hoffmann, Juliano Sarmento Cabral. Depth
diversity gradients of macrophytes: shape, drivers and recent shifts.
Authorea. April 20, 2021. 10.22541/au.161893346.60777770/v1

Research Compendium *Release 1.0* will correspond to:

* *Final publication*


## Data source

Raw data source of macrophytes data and environmental measurements is:
Bayerisches Landesamt für Umwelt, www.lfu.bayern.de . Licence CC BY 4.0


## Reproduce the results

### Install the package

To install the package in R follow this code:

    install.packages("devtools")
    library("devtools")
    install_github("https://github.com/AnneLew/Lewerentz-etal_2021_Macrophytes-DDG")
    library("Lewerentz-etal_2021_Macrophytes-DDG")


### Run R scripts

To reproduce the results run the R scripts in the following order:
| Order | Script Name | Description |
| --- | --- | --- |
| 1 | `Read_in_data.R` | description |
| 2 | ... | descr |


## Structure of research compendium

-   `data-raw/`: Raw datasets for biotic and abiotic data and R code to
    generate data in preparation files `data/`
-   `data/`: Cleaned data used for the analysis
-   `analysis/`: R code to reproduce tables, figures and analysis
-   `man/`:
-   `R/`:
