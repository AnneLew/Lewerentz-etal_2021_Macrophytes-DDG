# Depth diversity gradients of macrophytes: shape, drivers and recent shifts 

**Research Compendium**

<!-- badges: start -->

<!-- badges: end -->

Anne Lewerentz¹\*, Markus Hoffmann², Juliano Sarmento Cabral¹

¹ Ecosystem Modelling, Center for Computational and Theoretical Ecology (CCTB), University of Würzburg, Clara-Oppenheimer-Weg 32, 97074 Würzburg, Germany

² Limnological Station, Chair of Aquatic Systems Biology, Technical University of Munich, Germany

\* Corresponding author: [anne.lewerentz\@uni-wuerzburg.de](mailto:anne.lewerentz@uni-wuerzburg.de)

*This repository is a R package which includes all data, analysis files and results to reproduce the following publications given in the Journal reference.*

## Journal references

*Release 0.9* corresponds to a *Preprint*:

-   Anne Lewerentz, Markus Hoffmann, Juliano Sarmento Cabral. Depth diversity gradients of macrophytes: shape, drivers and recent shifts. Authorea. April 20, 2021. [DOI: 10.22541/au.161893346.60777770/v1](https://www.authorea.com/users/409017/articles/518802-depth-diversity-gradients-of-macrophytes-shape-drivers-and-recent-shifts?commit=50fa6e716400b5957bbe4ecfedd786b3ef8255de)

*Release 1.0* will correspond to the final Version:

-   *Final publication will be added as soon as it is published*

## Data source

Source of all used raw data is: Bayerisches Landesamt für Umwelt, www.lfu.bayern.de (Published under *Licence CC BY 4.0*).


## Structure of research compendium

-   `data-raw/`: Raw datasets for biotic and abiotic data and R code to generate data in preparation files `data/`
-   `data/`: Cleaned data used for the analysis
-   `analysis/`: R code in .Rmd files to reproduce tables, figures and analysis of main file and Supplementary material


## How to reproduce the results

### Install the package

To install the package in R follow this code:

    #install.packages("devtools") # install devtools if you don't have it already
    devtools::install_github("https://github.com/AnneLew/Lewerentz-etal_2021_Macrophytes-DDG")
    library("MacrophytesDDG")

### Run R scripts

To reproduce the results run the R scripts in the following order:

| Order | Script Name                          | Description                                                                                                                                                                             |
|-------|--------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1     | `data-raw/DATASET.R`                | Preparation of Community table for submerged macrophytes (Makroph_comm_S; Makroph) and Calculation of mean annual values for selected physical chemical measurements (Chem.Mean.YearDF) |
| 2     | `data-raw/DATASETPrep.R`             | Calcuation of Biodiversity metrices & Depth diversity gradient matrices; Output datasets: MakrophS_ALL; Makroph_Lake_DepthS; Makroph_Depth; Makroph_Lake_ALL; PEAK; Chem_uniform_LOIx; PEAK_Chem_norm      |
| 3     | `analysis/analysis.Rmd`              | Reproduces figures, tables and analysis used in main manuscript                                                                                                                         |
| 4     | `analysis/SupplementaryMaterial.Rmd` | Reproduces figures, tables and analysis shown in SupplementaryMaterial                                                                                                                  |

