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
  - `mail_phone.parquet`: a data set (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) format) of contact indicators. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view); `visitid` (view ID in the data set); `event_action` (indication of the contact action a user made : displaying a phone number or submiting a mail form); `is_logged` (indication of wether the user was logged while viewing the listing).
  - `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits.
  - `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. In the French context, departments correspond to the NUTS 3 level of the EU territorial classification system, which is an administrative unit with populations ranging from 150,000 to 800,000 inhabitants [[Eu'26](#references)].
- `out`: folder containing the outputs of the processing.
  - `pdf`: pdf files with the plots of the processing.
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


# package & session info
sessioninfo::session_info(pkgs)

```

```
─ Session info ───────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.6.1 (2026-06-24 ucrt)
 os       Windows 11 x64 (build 26200)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  French_France.utf8
 ctype    French_France.utf8
 tz       Europe/Paris
 date     2026-07-23
 rstudio  2026.07.1+147 Pacific Dogwood (desktop)
 pandoc   3.8.3 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
 quarto   1.5.57 @ C:\\PROGRA~1\\Quarto\\bin\\quarto.exe

─ Packages ───────────────────────────────────────────────────────────────────
 package           * version    date (UTC) lib source
 abind               1.4-8      2024-09-12 [1] CRAN (R 4.6.1)
 AER               * 1.2-17     2026-07-11 [1] CRAN (R 4.6.1)
 arrow             * 25.0.0     2026-07-16 [1] CRAN (R 4.6.1)
 AsioHeaders         1.30.2-1   2025-04-15 [1] CRAN (R 4.6.1)
 askpass             1.2.1      2024-10-04 [1] CRAN (R 4.6.1)
 assertthat          0.2.1      2019-03-21 [1] CRAN (R 4.6.1)
 backports           1.5.1      2026-04-03 [1] CRAN (R 4.6.0)
 base64enc           0.1-6      2026-02-02 [1] CRAN (R 4.6.0)
 bigD                0.3.1      2025-04-03 [1] CRAN (R 4.6.1)
 bit                 4.6.0      2025-03-06 [1] CRAN (R 4.6.1)
 bit64               4.8.2      2026-05-19 [1] CRAN (R 4.6.1)
 bitops              1.0-9      2024-10-03 [1] CRAN (R 4.6.0)
 boot                1.3-32     2025-08-29 [2] CRAN (R 4.6.1)
 broom               1.0.13     2026-05-14 [1] CRAN (R 4.6.1)
 broom.helpers     * 1.22.0     2025-09-17 [1] CRAN (R 4.6.1)
 bslib               0.11.0     2026-05-16 [1] CRAN (R 4.6.1)
 cachem              1.1.0      2024-05-16 [1] CRAN (R 4.6.1)
 callr               3.8.0      2026-06-05 [1] CRAN (R 4.6.1)
 car               * 3.1-5      2026-02-03 [1] CRAN (R 4.6.1)
 carData           * 3.0-6      2026-01-30 [1] CRAN (R 4.6.1)
 cards               0.8.1      2026-07-06 [1] CRAN (R 4.6.1)
 cardx               0.3.4      2026-07-06 [1] CRAN (R 4.6.1)
 cartograflow      * 1.0.5      2023-10-17 [1] CRAN (R 4.6.1)
 cartography       * 3.1.5      2025-07-23 [1] CRAN (R 4.6.1)
 caTools             1.18.4     2026-07-20 [1] CRAN (R 4.6.1)
 cellranger          1.1.0      2016-07-27 [1] CRAN (R 4.6.1)
 checkmate           2.3.4      2026-02-03 [1] CRAN (R 4.6.1)
 chromote            0.5.1      2025-04-24 [1] CRAN (R 4.6.1)
 class               7.3-23     2025-01-01 [2] CRAN (R 4.6.1)
 classInt          * 0.4-11     2025-01-08 [1] CRAN (R 4.6.1)
 cli                 3.6.6      2026-04-09 [1] CRAN (R 4.6.1)
 clipr               0.8.1      2026-05-25 [1] CRAN (R 4.6.1)
 cluster           * 2.1.8.2    2026-02-05 [2] CRAN (R 4.6.1)
 clusterCrit       * 1.3.0      2023-11-23 [1] CRAN (R 4.6.0)
 ClusterR          * 1.3.6      2025-12-22 [1] CRAN (R 4.6.1)
 codetools           0.2-20     2024-03-31 [2] CRAN (R 4.6.1)
 colorspace          2.1-3      2026-07-12 [1] CRAN (R 4.6.1)
 commonmark          2.0.0      2025-07-07 [1] CRAN (R 4.6.1)
 corrplot          * 0.95       2024-10-14 [1] CRAN (R 4.6.1)
 cowplot             1.2.0      2025-07-07 [1] CRAN (R 4.6.1)
 cpp11               0.5.5      2026-05-06 [1] CRAN (R 4.6.1)
 crayon              1.5.3      2024-06-20 [1] CRAN (R 4.6.1)
 crosstalk           1.2.2      2025-08-26 [1] CRAN (R 4.6.1)
 curl                7.1.0      2026-04-22 [1] CRAN (R 4.6.1)
 data.table        * 1.18.4     2026-05-06 [1] CRAN (R 4.6.1)
 data.tree           1.2.0      2025-08-25 [1] CRAN (R 4.6.1)
 DataExplorer      * 0.9.0      2026-03-08 [1] CRAN (R 4.6.1)
 datawizard          1.3.1      2026-04-26 [1] CRAN (R 4.6.1)
 DBI                 1.3.0      2026-02-25 [1] CRAN (R 4.6.1)
 deldir              2.0-4      2024-02-28 [1] CRAN (R 4.6.0)
 dendextend          1.19.1     2025-07-15 [1] CRAN (R 4.6.1)
 DEoptimR            1.2-0      2026-06-07 [1] CRAN (R 4.6.1)
 Deriv               4.2.0      2025-06-20 [1] CRAN (R 4.6.1)
 DescTools         * 0.99.60    2025-03-28 [1] CRAN (R 4.6.1)
 digest              0.6.39     2025-11-19 [1] CRAN (R 4.6.1)
 diptest             0.77-2     2025-08-20 [1] CRAN (R 4.6.0)
 distances         * 0.1.13     2025-11-24 [1] CRAN (R 4.6.1)
 doBy                4.7.2      2026-07-01 [1] CRAN (R 4.6.1)
 dplyr             * 1.2.1      2026-04-03 [1] CRAN (R 4.6.1)
 DT                * 0.34.0     2025-09-02 [1] CRAN (R 4.6.1)
 e1071               1.7-17     2025-12-18 [1] CRAN (R 4.6.1)
 effects           * 4.2-5      2026-02-17 [1] CRAN (R 4.6.1)
 ellipse             0.5.0      2023-07-20 [1] CRAN (R 4.6.1)
 emmeans             2.0.4      2026-07-15 [1] CRAN (R 4.6.1)
 entropy           * 1.3.2      2025-04-07 [1] CRAN (R 4.6.1)
 estimability        2.0.0      2026-06-26 [1] CRAN (R 4.6.1)
 evaluate            1.0.5      2025-08-27 [1] CRAN (R 4.6.1)
 Exact               3.3        2024-07-21 [1] CRAN (R 4.6.0)
 expm                1.0-0      2024-08-19 [1] CRAN (R 4.6.1)
 factoextra        * 2.1.0      2026-06-26 [1] CRAN (R 4.6.1)
 FactoMineR        * 2.16       2026-07-02 [1] CRAN (R 4.6.1)
 farver              2.1.2      2024-05-13 [1] CRAN (R 4.6.1)
 fastmap             1.2.0      2024-05-15 [1] CRAN (R 4.6.1)
 flashClust          1.1-4      2026-03-03 [1] CRAN (R 4.6.0)
 flexmix             2.3-20     2025-02-28 [1] CRAN (R 4.6.1)
 fontawesome         0.5.3      2024-11-16 [1] CRAN (R 4.6.1)
 forcats             1.0.1      2025-09-25 [1] CRAN (R 4.6.1)
 forecast            9.0.2      2026-03-18 [1] CRAN (R 4.6.1)
 foreign             0.8-91     2026-01-29 [2] CRAN (R 4.6.1)
 forestmodel       * 0.6.2      2020-07-19 [1] CRAN (R 4.6.1)
 Formula             1.2-5      2023-02-24 [1] CRAN (R 4.6.1)
 fpc               * 2.2-14     2026-01-14 [1] CRAN (R 4.6.1)
 fracdiff            1.5-4      2026-04-28 [1] CRAN (R 4.6.1)
 fs                  2.1.0      2026-04-18 [1] CRAN (R 4.6.1)
 furrr             * 0.4.0      2026-03-31 [1] CRAN (R 4.6.1)
 future            * 1.75.0     2026-07-20 [1] CRAN (R 4.6.1)
 generics            0.1.4      2025-05-09 [1] CRAN (R 4.6.1)
 geosphere         * 1.6-8      2026-04-05 [1] CRAN (R 4.6.1)
 GGally            * 2.4.0      2025-08-23 [1] CRAN (R 4.6.1)
 ggdendro          * 0.2.0      2024-02-23 [1] CRAN (R 4.6.1)
 ggeffects         * 2.3.2      2025-12-16 [1] CRAN (R 4.6.1)
 ggfortify         * 0.4.19     2025-07-27 [1] CRAN (R 4.6.1)
 ggplot2           * 4.0.3      2026-04-22 [1] CRAN (R 4.6.1)
 ggpubr              1.0.0      2026-07-06 [1] CRAN (R 4.6.1)
 ggrepel             0.9.8      2026-03-17 [1] CRAN (R 4.6.1)
 ggsci               5.1.0      2026-06-26 [1] CRAN (R 4.6.1)
 ggsignif            0.6.4      2022-10-13 [1] CRAN (R 4.6.1)
 ggspatial         * 1.1.10     2025-08-24 [1] CRAN (R 4.6.1)
 ggstats             0.13.0     2026-03-06 [1] CRAN (R 4.6.1)
 ggtext              0.1.2      2022-09-16 [1] CRAN (R 4.6.1)
 gld                 2.6.8      2025-09-14 [1] CRAN (R 4.6.1)
 globals             0.19.1     2026-03-13 [1] CRAN (R 4.6.1)
 glue                1.8.1      2026-04-17 [1] CRAN (R 4.6.1)
 gmp                 0.7-5.1    2026-02-09 [1] CRAN (R 4.6.1)
 gplots              3.3.0      2025-11-30 [1] CRAN (R 4.6.1)
 gridExtra         * 2.3.1      2026-06-25 [1] CRAN (R 4.6.1)
 gridtext            0.1.6      2026-02-19 [1] CRAN (R 4.6.1)
 gt                  1.3.0      2026-01-22 [1] CRAN (R 4.6.1)
 gtable              0.3.6      2024-10-25 [1] CRAN (R 4.6.1)
 gtools              3.9.5      2023-11-20 [1] CRAN (R 4.6.1)
 gtsummary         * 2.5.1      2026-05-30 [1] CRAN (R 4.6.1)
 haven               2.5.5      2025-05-30 [1] CRAN (R 4.6.1)
 highr               0.12       2026-03-06 [1] CRAN (R 4.6.1)
 Hmisc             * 5.2-6      2026-06-19 [1] CRAN (R 4.6.1)
 hms                 1.1.4      2025-10-17 [1] CRAN (R 4.6.1)
 htmlTable           2.5.0      2026-04-22 [1] CRAN (R 4.6.1)
 htmltools           0.5.9      2025-12-04 [1] CRAN (R 4.6.1)
 htmlwidgets         1.6.4      2023-12-06 [1] CRAN (R 4.6.1)
 httpuv              1.6.17     2026-03-18 [1] CRAN (R 4.6.1)
 httr                1.4.8      2026-02-13 [1] CRAN (R 4.6.1)
 igraph            * 2.3.3      2026-06-26 [1] CRAN (R 4.6.1)
 ineq              * 0.2-13     2014-07-21 [1] CRAN (R 4.6.1)
 insight             1.5.2      2026-06-28 [1] CRAN (R 4.6.1)
 irlba               2.3.7      2026-01-30 [1] CRAN (R 4.6.1)
 isoband             0.3.0      2025-12-07 [1] CRAN (R 4.6.1)
 jpeg                0.1-11     2025-03-21 [1] CRAN (R 4.6.0)
 jquerylib           0.1.4      2021-04-26 [1] CRAN (R 4.6.1)
 jsonlite            2.0.0      2025-03-27 [1] CRAN (R 4.6.1)
 juicyjuice          0.1.0      2022-11-10 [1] CRAN (R 4.6.1)
 kernlab             0.9-33     2024-08-13 [1] CRAN (R 4.6.0)
 KernSmooth          2.23-26    2025-01-01 [2] CRAN (R 4.6.1)
 knitr             * 1.51       2025-12-20 [1] CRAN (R 4.6.1)
 labeling            0.4.3      2023-08-29 [1] CRAN (R 4.6.1)
 labelled            2.16.0     2025-10-22 [1] CRAN (R 4.6.1)
 later               1.4.8      2026-03-05 [1] CRAN (R 4.6.1)
 lattice             0.22-9     2026-02-09 [2] CRAN (R 4.6.1)
 lazyeval            0.2.3      2026-04-04 [1] CRAN (R 4.6.1)
 leaflet           * 2.2.3      2025-09-04 [1] CRAN (R 4.6.1)
 leaflet.providers   3.0.0      2026-03-18 [1] CRAN (R 4.6.1)
 leaps               3.2        2024-06-10 [1] CRAN (R 4.6.1)
 lifecycle           1.0.5      2026-01-08 [1] CRAN (R 4.6.1)
 listenv             1.0.0      2026-06-22 [1] CRAN (R 4.6.1)
 litedown            0.10       2026-07-11 [1] CRAN (R 4.6.1)
 lme4                2.0-6      2026-07-16 [1] CRAN (R 4.6.1)
 lmom                3.3        2026-03-24 [1] CRAN (R 4.6.0)
 lmtest            * 0.9-40     2022-03-21 [1] CRAN (R 4.6.1)
 lubridate         * 1.9.5      2026-02-04 [1] CRAN (R 4.6.1)
 magrittr            2.0.5      2026-04-04 [1] CRAN (R 4.6.1)
 markdown            2.0        2025-03-23 [1] CRAN (R 4.6.1)
 MASS              * 7.3-65     2025-02-28 [2] CRAN (R 4.6.1)
 Matrix              1.7-5      2026-03-21 [2] CRAN (R 4.6.1)
 MatrixModels        0.5-4      2025-03-26 [1] CRAN (R 4.6.1)
 mclust              6.1.3      2026-07-05 [1] CRAN (R 4.6.1)
 memoise           * 2.0.1      2021-11-26 [1] CRAN (R 4.6.1)
 mgcv                1.9-4      2025-11-07 [2] CRAN (R 4.6.1)
 mime                0.13       2025-03-17 [1] CRAN (R 4.6.0)
 miniUI              0.1.2      2025-04-17 [1] CRAN (R 4.6.1)
 minqa               1.2.8      2024-08-17 [1] CRAN (R 4.6.1)
 mitools             2.4        2019-04-26 [1] CRAN (R 4.6.1)
 modelr              0.1.11     2023-03-22 [1] CRAN (R 4.6.1)
 modeltools          0.2-24     2025-05-02 [1] CRAN (R 4.6.1)
 moments           * 0.14.1     2022-05-02 [1] CRAN (R 4.6.1)
 multcompView        0.1-11     2026-02-16 [1] CRAN (R 4.6.1)
 mvtnorm             1.4-2      2026-07-12 [1] CRAN (R 4.6.1)
 NbClust           * 3.0.1      2022-05-02 [1] CRAN (R 4.6.1)
 networkD3           0.4.1      2025-04-14 [1] CRAN (R 4.6.1)
 nlme                3.1-169    2026-03-27 [2] CRAN (R 4.6.1)
 nloptr              2.2.1      2025-03-17 [1] CRAN (R 4.6.1)
 nnet                7.3-20     2025-01-01 [2] CRAN (R 4.6.1)
 numDeriv            2016.8-1.1 2019-06-06 [1] CRAN (R 4.6.0)
 openssl             2.4.2      2026-06-09 [1] CRAN (R 4.6.1)
 otel                0.2.0      2025-08-29 [1] CRAN (R 4.6.1)
 parallelly          1.48.0     2026-06-29 [1] CRAN (R 4.6.1)
 patchwork         * 1.3.2      2025-08-25 [1] CRAN (R 4.6.1)
 pbapply             1.7-4      2025-07-20 [1] CRAN (R 4.6.1)
 pbkrtest            0.5.5      2025-07-18 [1] CRAN (R 4.6.1)
 permute           * 0.9-10     2026-02-06 [1] CRAN (R 4.6.1)
 pillar              1.11.1     2025-09-17 [1] CRAN (R 4.6.1)
 pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.6.1)
 plotly            * 4.12.1     2026-07-22 [1] CRAN (R 4.6.1)
 plyr                1.8.9      2023-10-02 [1] CRAN (R 4.6.1)
 png                 0.1-9      2026-03-15 [1] CRAN (R 4.6.0)
 polynom             1.4-1      2022-04-11 [1] CRAN (R 4.6.1)
 prabclus            2.3-5      2026-01-14 [1] CRAN (R 4.6.1)
 prettyunits         1.2.0      2023-09-24 [1] CRAN (R 4.6.1)
 pROC              * 1.19.0.1   2025-07-31 [1] CRAN (R 4.6.1)
 processx            3.9.0      2026-04-22 [1] CRAN (R 4.6.1)
 progress            1.2.3      2023-12-06 [1] CRAN (R 4.6.1)
 promises            1.5.0      2025-11-01 [1] CRAN (R 4.6.1)
 proxy               0.4-29     2025-12-29 [1] CRAN (R 4.6.1)
 ps                  1.9.3      2026-04-20 [1] CRAN (R 4.6.1)
 pscl              * 1.5.9      2024-01-31 [1] CRAN (R 4.6.1)
 purrr             * 1.2.2      2026-04-10 [1] CRAN (R 4.6.1)
 quantreg            6.1        2025-03-10 [1] CRAN (R 4.6.1)
 questionr         * 0.8.2      2026-01-21 [1] CRAN (R 4.6.1)
 R.cache             0.17.0     2025-05-02 [1] CRAN (R 4.6.1)
 R.methodsS3         1.8.2      2022-06-13 [1] CRAN (R 4.6.1)
 R.oo                1.27.1     2025-05-02 [1] CRAN (R 4.6.1)
 R.utils             2.13.0     2025-02-24 [1] CRAN (R 4.6.1)
 R6                  2.6.1      2025-02-15 [1] CRAN (R 4.6.1)
 rappdirs            0.3.4      2026-01-17 [1] CRAN (R 4.6.1)
 raster              3.6-32     2025-03-28 [1] CRAN (R 4.6.1)
 rbibutils           2.4.1      2026-01-21 [1] CRAN (R 4.6.1)
 RColorBrewer      * 1.1-3      2022-04-03 [1] CRAN (R 4.6.1)
 Rcpp                1.1.2      2026-07-05 [1] CRAN (R 4.6.1)
 RcppArmadillo       15.4.0-1   2026-06-19 [1] CRAN (R 4.6.1)
 RcppEigen           0.3.4.0.2  2024-08-24 [1] CRAN (R 4.6.1)
 Rdpack              2.6.6      2026-02-08 [1] CRAN (R 4.6.1)
 reactable           0.4.5      2025-12-01 [1] CRAN (R 4.6.1)
 reactR              0.6.1      2024-09-14 [1] CRAN (R 4.6.1)
 readr             * 2.2.0      2026-02-19 [1] CRAN (R 4.6.1)
 readxl              1.5.0      2026-05-16 [1] CRAN (R 4.6.1)
 reformulas          0.4.4      2026-02-02 [1] CRAN (R 4.6.1)
 rematch             2.0.0      2023-08-30 [1] CRAN (R 4.6.1)
 repr                1.1.7      2024-03-22 [1] CRAN (R 4.6.1)
 reshape2            1.4.5      2025-11-12 [1] CRAN (R 4.6.1)
 ResourceSelection * 0.3-6      2023-07-08 [1] CRAN (R 4.6.1)
 rlang             * 1.3.0      2026-07-05 [1] CRAN (R 4.6.1)
 rmarkdown           2.31       2026-03-26 [1] CRAN (R 4.6.1)
 robustbase          0.99-7     2026-02-05 [1] CRAN (R 4.6.1)
 ROCR              * 1.0-12     2026-01-23 [1] CRAN (R 4.6.1)
 rootSolve           1.8.2.4    2023-09-21 [1] CRAN (R 4.6.0)
 rosm                0.3.1      2026-01-21 [1] CRAN (R 4.6.1)
 rpart               4.1.27     2026-03-27 [2] CRAN (R 4.6.1)
 rprojroot         * 2.1.1      2025-08-26 [1] CRAN (R 4.6.1)
 rstatix             1.0.0      2026-07-03 [1] CRAN (R 4.6.1)
 rstudioapi          0.19.0     2026-06-11 [1] CRAN (R 4.6.1)
 s2                  1.1.11     2026-06-01 [1] CRAN (R 4.6.1)
 S7                  0.2.2      2026-04-22 [1] CRAN (R 4.6.1)
 sandwich          * 3.1-2      2026-07-12 [1] CRAN (R 4.6.1)
 sass                0.4.10     2025-04-11 [1] CRAN (R 4.6.1)
 scales              1.4.0      2025-04-24 [1] CRAN (R 4.6.1)
 scatterplot3d       0.3-45     2026-02-23 [1] CRAN (R 4.6.1)
 sf                * 1.1-1      2026-05-06 [1] CRAN (R 4.6.1)
 shiny               1.14.0     2026-06-21 [1] CRAN (R 4.6.1)
 showtext            0.9-8      2026-03-21 [1] CRAN (R 4.6.1)
 showtextdb          3.0        2020-06-04 [1] CRAN (R 4.6.1)
 skimr             * 2.2.2      2026-01-10 [1] CRAN (R 4.6.1)
 sourcetools         0.1.7-2    2026-03-28 [1] CRAN (R 4.6.1)
 sp                  2.2-3      2026-07-19 [1] CRAN (R 4.6.1)
 SparseM             1.84-2     2024-07-17 [1] CRAN (R 4.6.1)
 spData            * 2.3.5      2026-05-04 [1] CRAN (R 4.6.1)
 spdep             * 1.4-2      2026-02-13 [1] CRAN (R 4.6.1)
 stringi           * 1.8.7      2025-03-27 [1] CRAN (R 4.6.0)
 stringr           * 1.6.0      2025-11-04 [1] CRAN (R 4.6.1)
 styler              1.11.0     2025-10-13 [1] CRAN (R 4.6.1)
 survey              4.5        2026-02-24 [1] CRAN (R 4.6.1)
 survival          * 3.8-6      2026-01-16 [2] CRAN (R 4.6.1)
 sys                 3.4.3      2024-10-04 [1] CRAN (R 4.6.1)
 sysfonts            0.8.9      2024-03-02 [1] CRAN (R 4.6.1)
 terra               1.9-34     2026-06-19 [1] CRAN (R 4.6.1)
 tibble              3.3.1      2026-01-11 [1] CRAN (R 4.6.1)
 tidyr             * 1.3.2      2025-12-19 [1] CRAN (R 4.6.1)
 tidyselect          1.2.1      2024-03-11 [1] CRAN (R 4.6.1)
 timechange          0.4.0      2026-01-29 [1] CRAN (R 4.6.1)
 timeDate            4052.112   2026-01-28 [1] CRAN (R 4.6.1)
 tinytex             0.60       2026-06-16 [1] CRAN (R 4.6.1)
 tzdb                0.5.0      2025-03-15 [1] CRAN (R 4.6.1)
 units               1.0-1      2026-03-11 [1] CRAN (R 4.6.1)
 urca                1.3-4      2024-05-27 [1] CRAN (R 4.6.1)
 utf8                1.2.6      2025-06-08 [1] CRAN (R 4.6.1)
 V8                  8.2.0      2026-04-21 [1] CRAN (R 4.6.1)
 vctrs               0.7.3      2026-04-11 [1] CRAN (R 4.6.1)
 vegan             * 2.7-5      2026-05-25 [1] CRAN (R 4.6.1)
 viridis             0.6.5      2024-01-29 [1] CRAN (R 4.6.1)
 viridisLite         0.4.3      2026-02-04 [1] CRAN (R 4.6.1)
 vroom               1.7.1      2026-03-31 [1] CRAN (R 4.6.1)
 webshot2          * 0.1.2      2025-04-23 [1] CRAN (R 4.6.1)
 websocket           1.4.4      2025-04-10 [1] CRAN (R 4.6.1)
 withr               3.0.3      2026-06-19 [1] CRAN (R 4.6.1)
 wk                  0.9.5      2025-12-18 [1] CRAN (R 4.6.1)
 xfun                0.60       2026-07-09 [1] CRAN (R 4.6.1)
 xml2                1.6.0      2026-06-22 [1] CRAN (R 4.6.1)
 xtable              1.8-8      2026-02-22 [1] CRAN (R 4.6.1)
 yaml                2.3.12     2025-12-10 [1] CRAN (R 4.6.1)
 zip                 3.0.1      2026-07-13 [1] CRAN (R 4.6.1)
 zoo               * 1.8-15     2025-12-15 [1] CRAN (R 4.6.1)

```

## References

* **[Eu'26]** **Eurostat** (2026).  
*NUTS — Nomenclature des unités territoriales statistiques: Principes.*  
URL: https://ec.europa.eu/eurostat/fr/web/nuts/principles

* **[Hu'20]** **Hurlbert, S. H.** (1971).  
*The nonconcept of species diversity.*  
Ecology, 52(4), 577–586.  
DOI: https://doi.org/10.2307/1934145

* **[Mc'77]** **McFadden, D.** (1977).  
*Quantitative Methods for Analyzing Travel Behaviour of Individuals.*  
Cowles Foundation Discussion Paper 707.  
URL: https://elischolar.library.yale.edu/cowles-discussion-paper-series/707


* **[Ok'24]** **Oksanen, J., et al.** (2024).  
*vegan: Community Ecology Package.*  
R package version 2.6-8.  
URL: https://CRAN.R-project.org/package=vegan

* **[Ma'04]** **Pagès, J.** (2004).  
*Analyse factorielle de données mixtes.*  
Revue de statistique appliquée, 52(4), 93–111.


* **[Pa'25]** **Pannu, A., & El-Saeiti, I. N.** (2025).  
*valuating predictive accuracy and model selection in logistic regression: a
statistical approach using sensitivity, specificity, and ROC analysis.*  
International Journal of Management, 16(1).  
DOI: https://doi.org/10.34218/IJM_16_01_002



* **[Ro'87]** **Rousseeuw, P. J.** (1987).  
*Silhouettes: a graphical aid to the interpretation and validation of cluster analysis.*  
Journal of Computational and Applied Mathematics, 53–65.  
DOI: https://doi.org/10.1016/0377-0427(87)90125-7

* **[Si'49]** **Simpson, E. H.** (1949).  
*Measurement of diversity.*  
Nature, 163(4148), 688.  
DOI: https://doi.org/10.1038/163688a0


* **[Zo'07]** **Zou, K. H., O’Malley, A. J., & Mauri, L.** (2007).  
*Receiver-operating characteristic analysis for evaluating diagnostic tests and predictive models.*  
Circulation, 115(5), 654–657.  
DOI: https://doi.org/10.1161/CIRCULATIONAHA.105.594929
