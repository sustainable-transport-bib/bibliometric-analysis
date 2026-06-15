# Bibliometric Analysis Repository

## Overview

This repository contains the data files and R scripts used to support a bibliometric review in the area of sustainable transportation research.
The repository is designed to provide a structured and transparent analytical workflow for examining publication patterns, citation profiles, and research leadership within the field. 

## Scope of Analysis

The bibliometric analysis is organized into four complementary levels:

- **Author-level analysis** examines leading contributors, publication activity, citation performance, and author influence.
- **Country-level analysis** evaluates the geographic distribution of research output and identifies leading countries in the field.
- **Institution-level analysis** investigates the contribution and visibility of research organizations and academic institutions.
- **Journal-level analysis** explores the main publication outlets, journal productivity, and citation prominence. 

Together, these four levels support a broad understanding of the structure and development of sustainable transportation research and help identify leading actors and influential publication sources in the domain.  

## Repository Contents

### Main input data
- `bibliometric file initial.xls` — primary input dataset used in the bibliometric review.

### R scripts
- `AUTHOR_LEVEL_ANALYSIS.R` — performs author-level bibliometric analysis.
- `COUNTRY_LEVEL_ANALYSIS.R` — performs country-level bibliometric analysis.
- `INSTITUTION_LEVEL_ANALYSIS.R` — performs institution-level bibliometric analysis.
- `JOURNAL_LEVEL_ANALYSIS.R` — performs journal-level bibliometric analysis.

### Supplementary Excel files
The additional Excel files included in this repository are supplementary files used in the preparation of bubble-graph visualizations and related supporting analyses.

## Outputs

The scripts are designed to generate analytical outputs such as:

- summary tables,
- rankings,
- figures,
- and visualization-ready results.

Because the scripts are largely self-contained, they can be executed independently according to the specific analytical level of interest. [web:135][web:139]

## Usage

1. Open the relevant R script according to the level of analysis required.
2. Use the main bibliometric input file provided in the repository.
3. Run the script in R version 4.5.1.
4. Review the generated tables, figures, rankings, and related outputs.

No fixed execution order is required, as the scripts were designed to function independently for their respective analytical tasks.

## Notes
This repository is intended to support the bibliometric review and to provide transparent access to the analytical materials associated with the study.
The structure of the repository is designed to make it easier to follow the analytical workflow and interpret the generated outputs in relation to sustainable transportation research.  
