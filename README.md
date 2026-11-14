# hgraph v1.0.0 
# =================== 
*Housing search as a graph* 

*Serious Search Identification in User-generated data*
                                                            
* Copyright 2024-2026 <Anonymous submission>

hgraph is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see licence.txt

* **Lab site:** Anonymous submission
* **GitHub repo:** Anonymous submission
* **Contact:** Anonymous submission

------------------------------------------------------------------------

This repository contains the R code used to process the data, generate all results, and produce the figures presented in the document *'**Housing search as a graph: Using network science to delineate urban housing submarkets**'*.  
A link to the document will be added if it is published.

The scripts provide a complete workflow for graph extraction, analyses, and visualization.  
They can also be applied to custom data sets to delineate urban housing submarkets through user-generated data in online listing consultation.


------------------------------------------------------------------------




If you use this source code, please cite article:

``` bibtex
```

------------------------------------------------------------------------

**Content**

- [Organization](#organization)
- [Installation](#installation)
- [Use](#use)
- [Dependencies](#dependencies)
- [References](#references)

------------------------------------------------------------------------

## Organization

- `in`: folder containing all the input data files.
  - `events.parquet`: data set  (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) format) of listings viewed by online searchers. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view);  `visitid` (view ID in the data set);  `is_logged` (indication of wether the user was logged while viewing the listing).
  - `features.parquet`: data set (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) of every single listings' features. This table contains colums : `id_listing` (listing ID); `price` (property price); `area` (property area); `room_count` (property room count); `fct_room_count` (property room count set to factor variable); `sqm_price` (property square meter price); `item_type` (property type, wether a house or appartment);
  - `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits.
  - `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. In the French context, departments correspond to the NUTS 3 level of the EU territorial classification system, which is an administrative unit with populations ranging from 150,000 to 800,000 inhabitants [[Eu'26](#references)].
- `out`: folder containing the outputs of the processing.
  - `pdf`: pdf files with the plots of the processing.
  - `png`: png files with the plots of the processing.
  - `Rdata`: Rdata files containing `R`objects such as dataframes.
  - `svg`: svg files with the plots of the processing.
  - `tex`: tex files (output in [Latex](https://fr.wikipedia.org/wiki/LaTeX) format) with the tables produced by the processing and provided in the document.
  - `parquet`: forlder containing output data sets (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet). For example, the output data set of 'Serious and non serious users'.
  - `CSV`: forlder containing output data sets in [CSV](https://fr.wikipedia.org/wiki/Comma-separated_values) format.
- `src`: folder containing the source code.
  - `plot`: folder containing `R` scripts for plots.
    - `map_plot.R`: `R` script to plot Online search intensity by city.
    - `sample_search_cluster_plot.R`: `R` script to plot serious search graphics.
    - `save_tables.R`: `R` script to save tables in tex format.
    - `serious_search_pdf_plot.R`: `R`script to save graphics in pdf format.
    - `serious_searcher_svg_plots.R`: `R`script to save graphics in svg format.
  - `serious_search`: folder containing `R` scripts for the analysis of serious search.
    - `contact_indicators.R`: `R`script to compute contact indicators.
    - `revisit_contact_temporality.R`: `R`script to analyze behavioural temporality between Revist and Contact.
    - `revisit_contact_temporality.R`: `R`script to analyze the revisiting temporality.
    - `serious_search_through_revisit.R`: `R`script to compute listings' revisit indicators and search variability indicators.
    - `serious_search_through_revisit_analysis.R`: `R`script to analyze search engagement through listings' revisit indicators and search variability indicators.
    - `serious_search_through_revisit_sample_analysis.R` : `R`script to analyze search engagement through listings' revisit indicators and search variability indicators on a sample of the online searchers.
  - `create_directories.R` : `R` scripts to create directories.
  - `load_data.R`: `R` script to load data.
  - `packages_loading.R` : `R` script to install (if not ye installed) and load the packages used in this work, along with customer functions.
  - `processing_functions.R`: `R` scripts for data processing ad hoc functions.
  - `main.R`: the main `R` scripts to run the full processings.
  - `serious_search_through_revisit_along_with_contact_indicators.R`: Identify serious search with revisit and contact (phone number displaying and mail form submission) indicators.
  - `serious_search_through_revisit_without_contact_indicators.R`: Identify serious search with revisit only.
- `Serious_search_identification.Rproj`: `R` projet file to run the projet.

## Installation

### R and Packages
To run this program you need to install the `R` language environment and the required packages :
1. install the [`R` language](https://cran.r-project.org/)
2. Download this project from GitHub and unzip or clone it.
3. Run `packages_loading.R` to install the required packages (this step is included in `main.R`; a minimal Internet connection is needed).

### Data
You need to set up the files in folder `in`: `events.parquet`, `features.gpkg`, `mail_phone.parquet`, `geom_sf_cities.gpkg` and `geom_sf_departements.gpkg`. See the examples provided in folder `in`.


## Use

### To Replicate the Paper Experiments

#### Data Preparation
Unfortunately, we are not allowed to publish the data used in this paper. However, we provide a small fictional data set designed to test the program and reproduce experiments similar to those presented in our paper (see folder `in`). 


#### Processing
Once the data set is ready, do the following to apply the process described in the paper to these data: Run the `main.R` script using `source("main.R")`.

**Note** : [Rstudio](https://docs.posit.co/ide/user/)  user may first run `Serious_search_identification.Rproj` file to work under the project. For example, this will automatically set the working directory. But this is not necessary.


### To Apply the Paper Process to Custom Data

#### Data Preparation
First, you need to set files `events.parquet`, `features.gpkg`, `mail_phone.parquet` `geom_sf_cities.gpkg` and `geom_sf_departements.gpkg`:

* `events.parquet`: data set  (in parquet format) of listings view by online searchers. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view);  `visitid` (view ID in the data set);  `is_logged` (indication of wether the user was logged while viewing the listing).
* `features.gpkg`: data set (in gpkg format) of every single listings' features. This table contains colums : `id_listing` (listing ID); `price` (property price); `area` (property area); `room_count` (property room count); `fct_room_count` (property room count set to factor variable); `sqm_price` (property square meter price); `item_type` (property type, wether a house or appartment);
* `mail_phone.parquet`: a data set (in parquet format) of contact indicators. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view); `visitid` (view ID in the data set); `event_action` (indication of the contact action a user made : displaying a phone number or submiting a mail form); `is_logged` (indication of wether the user was logged while viewing the listing).
* `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits. This table contains clomuns : `city_ID` (city ID), `dep_ID` (department ID), `reg_ID` (region ID),  `geom` (city geometry) 
* `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. This table contains clomuns :  `dep_ID` (department ID), `reg_ID` (region ID), `geom` (department geometry). 

#### Processing
  1. Copy your own `events.parquet`, `features.gpkg`, `geom_sf_cities.gpkg`, `geom_sf_departements.gpkg` and `mail_phone.parquet` in the `in` folder;
  2. Run the `main.R` script using `source("main.R")`. 

### To Perform Only the Serious Search Identification
These instructions are meant to perform a quick identification of serious search, without necessarily replicating all the experiments of the paper.
The procedure is slightly different depending on whether contact data are available to the user. Contact data refers to phone number displaying as well as mail form submission (cf. the paper for more detail).

##### Data Preparation
First, you need to set files `events.parquet`, `features.gpkg`, `mail_phone.parquet`, `geom_sf_cities.gpkg` and `geom_sf_departements.gpkg`:
  
* `events.parquet`: data set  (in parquet format) of listings view by online searchers. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view);  `visitid` (view ID in the data set); `is_logged` (indication of wether the user was logged while viewing the listing).
* `features.gpkg`: data set (in gpkg format) of every single listings' features. This table contains colums : `id_listing` (listing ID); `price` (property price); `area` (property area); `room_count` (property room count); `fct_room_count` (property room count set to factor variable); `sqm_price` (property square meter price); `item_type` (property type, wether a house or appartment);
* `mail_phone.parquet`: **only if the contact data are available.** This file contains contact indicators (in parquet format). Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view); `visitid` (view ID in the data set); `event_action` (indication of the contact action a user made : displaying a phone number or submiting a mail form); `is_logged` (indication of wether the user was logged while viewing the listing).
* `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits. This table contains clomuns : `city_ID` (city ID), `dep_ID` (department ID), `reg_ID` (region ID),  `geom` (city geometry) 
* `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. This table contains clomuns :  `dep_ID` (department ID), `reg_ID` (region ID), `geom` (department geometry).


#### Processing
1. Copy your own `events.parquet`, `features.gpkg`, `geom_sf_cities.gpkg`, `geom_sf_departements.gpkg`, and possibly `mail_phone.parquet` in the `in` folder;
2.
   -If contact indicators are available and want to incorporate them in the process, run the `serious_search_through_revisit_along_with_contact_indicators.R` script using `source("serious_search_through_revisit_along_with_contact_indicators.R")`.
  - If contact indicators are not available, run the `serious_search_through_revisit_without_contact_indicators.R` script using `source("serious_search_through_revisit_without_contact_indicators.R")`.
     
#### Output
The output of this processing is saved under `out/parquet/serious_search/serious_search_data.parquet` for [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) format and `out/CSV/serious_search/serious_search_data.CSV` for [CSV](https://fr.wikipedia.org/wiki/Comma-separated_values) format. Serious search is identified through the column `is_serious`, which is a logical variable.

## Dependencies


```{r}
# packages used

pkgs <- c(
  "lubridate",
  "gridExtra",
  "arrow",
  "purrr",
  "dplyr",
  "tidyr",
  "knitr",
  "stringr",
  "questionr",
  "readr",
  "ggplot2",
  "rlang",
  "stats",
  "skimr",
  "entropy",
  "car",
  "corrplot",
  "data.table",
  "future",
  "furrr",
  "igraph",
  "plotly",
  "broom.helpers",
  "sf",
  "cartography",
  "geosphere",
  "ggspatial",
  "RColorBrewer",
  "classInt",
  "cartograflow",
  "spdep",
  "vegan",
  "ineq",
  "lmtest",
  "pROC",
  "pscl",
  "DescTools",
  "ROCR",
  "ResourceSelection",
  "MASS",
  "AER",
  "memoise",
  "rprojroot",
  "stringi",
  "plotly",
  "DataExplorer",
  "DT",
  "Hmisc",
  "leaflet",
  "moments",
  "DescTools",
  "FactoMineR",
  "cluster",
  "ClusterR",
  "ggdendro",
  "distances",
  "fpc",
  "ggfortify",
  "GGally",
  "factoextra",
  "gridExtra",
  "grid",
  "NbClust",
  "clusterCrit",
  "xtable",
  "gt",
  "patchwork",
  "gtsummary",
  "webshot2",
  "GGally",
  "forestmodel",
  "effects",
  "ggeffects" 
)
```

```
sessioninfo::session_info(pkgs, dependencies = FALSE)
─ Session info ──────────────────────────────────────────────────────
 setting  value
 version  R version 4.6.1 (2026-06-24 ucrt)
 os       Windows 11 x64 (build 26200)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  French_France.utf8
 ctype    French_France.utf8
 tz       Europe/Paris
 date     2026-11-07
 rstudio  2026.10.1+239 Blue Mistflower (desktop)
 pandoc   NA
 quarto   1.5.57 @ C:\\PROGRA~1\\Quarto\\bin\\quarto.exe

─ Packages ──────────────────────────────────────────────────────────
 package           * version  date (UTC) lib source
 AER                 1.2-17   2026-07-11 [1] CRAN (R 4.6.1)
 arrow               25.0.0   2026-07-16 [1] CRAN (R 4.6.1)
 broom.helpers       1.22.0   2025-09-17 [1] CRAN (R 4.6.1)
 car                 3.1-5    2026-02-03 [1] CRAN (R 4.6.1)
 cartograflow        1.0.5    2023-10-17 [1] CRAN (R 4.6.1)
 cartography         3.1.5    2025-07-23 [1] CRAN (R 4.6.1)
 classInt            0.4-11   2025-01-08 [1] CRAN (R 4.6.1)
 cluster             2.1.8.2  2026-02-05 [2] CRAN (R 4.6.1)
 clusterCrit         1.3.0    2023-11-23 [1] CRAN (R 4.6.0)
 ClusterR            1.3.6    2025-12-22 [1] CRAN (R 4.6.1)
 corrplot            0.95     2024-10-14 [1] CRAN (R 4.6.1)
 data.table          1.18.4   2026-05-06 [1] CRAN (R 4.6.1)
 DataExplorer        0.9.0    2026-03-08 [1] CRAN (R 4.6.1)
 DescTools           0.99.60  2025-03-28 [1] CRAN (R 4.6.1)
 distances           0.1.13   2025-11-24 [1] CRAN (R 4.6.1)
 dplyr               1.2.1    2026-04-03 [1] CRAN (R 4.6.1)
 DT                  0.34.0   2025-09-02 [1] CRAN (R 4.6.1)
 effects             4.2-5    2026-02-17 [1] CRAN (R 4.6.1)
 entropy             1.3.2    2025-04-07 [1] CRAN (R 4.6.1)
 factoextra          2.1.0    2026-06-26 [1] CRAN (R 4.6.1)
 FactoMineR          2.16     2026-07-02 [1] CRAN (R 4.6.1)
 forestmodel         0.6.2    2020-07-19 [1] CRAN (R 4.6.1)
 fpc                 2.2-14   2026-01-14 [1] CRAN (R 4.6.1)
 furrr               0.4.0    2026-03-31 [1] CRAN (R 4.6.1)
 future              1.75.0   2026-07-20 [1] CRAN (R 4.6.1)
 geosphere           1.6-8    2026-04-05 [1] CRAN (R 4.6.1)
 GGally              2.4.0    2025-08-23 [1] CRAN (R 4.6.1)
 ggdendro            0.2.0    2024-02-23 [1] CRAN (R 4.6.1)
 ggeffects           2.3.2    2025-12-16 [1] CRAN (R 4.6.1)
 ggfortify           0.4.19   2025-07-27 [1] CRAN (R 4.6.1)
 ggplot2             4.0.3    2026-04-22 [1] CRAN (R 4.6.1)
 ggspatial           1.1.10   2025-08-24 [1] CRAN (R 4.6.1)
 gridExtra           2.3.1    2026-06-25 [1] CRAN (R 4.6.1)
 gt                  1.3.0    2026-01-22 [1] CRAN (R 4.6.1)
 gtsummary           2.5.1    2026-05-30 [1] CRAN (R 4.6.1)
 Hmisc               5.2-6    2026-06-19 [1] CRAN (R 4.6.1)
 igraph              2.3.3    2026-06-26 [1] CRAN (R 4.6.1)
 ineq                0.2-13   2014-07-21 [1] CRAN (R 4.6.1)
 knitr               1.51     2025-12-20 [1] CRAN (R 4.6.1)
 leaflet             2.2.3    2025-09-04 [1] CRAN (R 4.6.1)
 lmtest              0.9-40   2022-03-21 [1] CRAN (R 4.6.1)
 lubridate           1.9.5    2026-02-04 [1] CRAN (R 4.6.1)
 MASS                7.3-65   2025-02-28 [2] CRAN (R 4.6.1)
 memoise             2.0.1    2021-11-26 [1] CRAN (R 4.6.1)
 moments             0.14.1   2022-05-02 [1] CRAN (R 4.6.1)
 NbClust             3.0.1    2022-05-02 [1] CRAN (R 4.6.1)
 patchwork           1.3.2    2025-08-25 [1] CRAN (R 4.6.1)
 plotly              4.12.1   2026-07-22 [1] CRAN (R 4.6.1)
 pROC                1.19.0.1 2025-07-31 [1] CRAN (R 4.6.1)
 pscl                1.5.9    2024-01-31 [1] CRAN (R 4.6.1)
 purrr               1.2.2    2026-04-10 [1] CRAN (R 4.6.1)
 questionr           0.8.2    2026-01-21 [1] CRAN (R 4.6.1)
 RColorBrewer        1.1-3    2022-04-03 [1] CRAN (R 4.6.1)
 readr               2.2.0    2026-02-19 [1] CRAN (R 4.6.1)
 ResourceSelection   0.3-6    2023-07-08 [1] CRAN (R 4.6.1)
 rlang               1.3.0    2026-07-05 [1] CRAN (R 4.6.1)
 ROCR                1.0-12   2026-01-23 [1] CRAN (R 4.6.1)
 rprojroot           2.1.1    2025-08-26 [1] CRAN (R 4.6.1)
 sf                  1.1-1    2026-05-06 [1] CRAN (R 4.6.1)
 skimr               2.2.2    2026-01-10 [1] CRAN (R 4.6.1)
 spdep               1.4-2    2026-02-13 [1] CRAN (R 4.6.1)
 stringi             1.8.7    2025-03-27 [1] CRAN (R 4.6.0)
 stringr             1.6.0    2025-11-04 [1] CRAN (R 4.6.1)
 tidyr               1.3.2    2025-12-19 [1] CRAN (R 4.6.1)
 vegan               2.7-5    2026-05-25 [1] CRAN (R 4.6.1)
 webshot2            0.1.2    2025-04-23 [1] CRAN (R 4.6.1)
 xtable              1.8-8    2026-02-22 [1] CRAN (R 4.6.1)
```

## References

* **[Eu'26]** **Eurostat** (2026).  
*NUTS — Nomenclature des unités territoriales statistiques: Principes.*  
URL: https://ec.europa.eu/eurostat/fr/web/nuts/principles

* **[Ba'04]** **Barrat, A., M. Barthelemy, R. Pastor-Satorras, and A. Vespignani.** (2004).
*The Architecture of Complex Weighted Networks.* Proceedings of the National Academy of Sciences 101 (11): 3747–52. https://doi.org/10.1073/pnas.0400087101.

* **[Ba'15]** **Bates, D., M. Mächler, B. Bolker, and S. Walker.** (2015).
*Fitting Linear Mixed-Effects Models Using lme4.* Journal of Statistical Software 67 (1): 1–48. https://doi.org/10.18637/jss.v067.i01.

* **[Br'96]** **Brunsdon, C., A. S. Fotheringham, and M. E. Charlton.** (1996).
*Geographically Weighted Regression: A Method for Exploring Spatial Nonstationarity.* Geographical Analysis 28 (4): 281–98.

* **[Cs'04]** **Csárdi, G., K. Nepusz T.and Müller, S. Horvát, V. Traag, F. Zanini, and D. Noom.** (2023).
*Igraph for r: R Interface of the Igraph Library for Graph Theory and Network Analysis.* Zenodo, ahead of print. https://doi.org/10.5281/zenodo.7682609.

* **[Fa'14]** **Farine, D.R.** (2014).
*Measuring Phenotypic Assortment in Animal Social Networks: Weighted Associations Are More Robust Than Binary Edges.* Animal Behaviour 89: 141–53. https://doi.org/10.1016/j.anbehav.2014.01.001.

* **[Mu'20]** **Murakami, D., N. Tsutsumida, T. Yoshida, T. Nakaya, and B. Lu.** (2020).
*Scalable GWR: A Linear-Time Algorithm for Large-Scale Geographically Weighted Regression with Polynomial Kernels.* Annals of the American Association of Geographers 111 (2): 459–80. https://doi.org/10.1080/24694452.2020.1774350.

* **[Na'17]** **Nakagawa, S., P. C. D. Johnson, and H. Schielzeth.** (2017).
*The Coefficient of Determination R2 and Intra-Class Correlation Coefficient from Generalized Linear Mixed-Effects Models Revisited and Expanded.* Journal of the Royal Society Interface 14 (134): 20170213. https://doi.org/10.1098/rsif.2017.0213.

* **[Ne'03]** **Newman, M. E. J.** (2003).
*Mixing Patterns in Networks.* Physical Review E 67 (February): 026126. https://doi.org/10.1103/PhysRevE.67.026126.


* **[Ba'04]** **Traag, V. A., L. Waltman, and N. J. Van Eck.** (2019).
*From Louvain to Leiden: Guaranteeing Well-Connected Communities.* Scientific Reports 9 (1): 5233. https://doi.org/10.1038/s41598-019-41695-z.



