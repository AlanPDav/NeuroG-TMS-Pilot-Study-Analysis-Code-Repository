# NeuroG-TMS-Pilot-Study-Analysis-Code-Repository

Human neural progenitor cells from the olfactory epithelium reflect the response to antidepressants and repetitive transcranial magnetic stimulation in depression‑diagnosed patients: A Pilot Study

1 · Purpose

This repository bundles the three R scripts used to process cytokine (ELISA), neurogenesis and clinical data for the NeuroG‑TMS pilot study.  Raw data contain sensitive participant information and are not included.

2 · File Inventory

neurog‑tms/
├── R/
│   ├── Dep_all.R          # Statistics & plots for depression‑related measures
│   ├── Clin_all.R         # Demographic and clinical‑score analyses (all groups)
│   └── modified_ELISA.R   # Cytokine cleaning, EDA & group comparisons
├── data/
│   └── raw/               # Place de‑identified CSV/XLSX files here (not tracked)
├── outputs/               # Figures (.png/.pdf) & tables (.xlsx/.csv) created by scripts
├── .gitignore             # Excludes data/raw/* and large artefacts
└── README.md              # You are here

3 · Requirements

Software

Version

R

≥ 4.2

R packages

pacman, tidyverse, ggplot2, ggpubr, moonBook, Hmisc, corrplot, openxlsx, nlme, rstatix, DataExplorer, qwraps2

Install once:

install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggpubr, moonBook, Hmisc, corrplot,
               openxlsx, nlme, rstatix, DataExplorer, qwraps2)

4 · Data Placement (private)

Copy the following de‑identified files into data/raw/ before running any script:

ELISA.csv

clinical_all.csv

Any additional files referenced inside the scripts

The datasets are not committed to version control to protect confidentiality.

5 · Reproducing the Analysis

Set your working directory to the project root and run the scripts you need, e.g.:

source("R/modified_ELISA.R")  # Cytokine distributions, normality tests, group comparisons
source("R/Dep_all.R")          # Depression‑specific analyses (MDD vs controls vs BPD)
source("R/Clin_all.R")         # Demographic tables & summary figures

Each script writes its artefacts to outputs/ and logs session information for reproducibility.

6 · Output Examples

Script

Key artefacts (saved to outputs/)

modified_ELISA.R

figures/IL6_boxplot.png, tables/kruskal_IL6.xlsx

Dep_all.R

figures/HDRS_change_MDD.png, mixed‑effects model summaries

Clin_all.R

tables/demographics_summary.xlsx, sex‑distribution charts

7 · How to Cite

Dávalos‑Trejo, A. et al. (2025). Human neural progenitor cells from the olfactory epithelium reflect the response to antidepressants and repetitive transcranial magnetic stimulation in depression‑diagnosed patients: A Pilot Study. Manuscript in preparation.

8 · License

Released under the MIT License – see LICENSE for details.

9 · Contact

For questions or contributions, open an issue or email Alan Dávalos at alanpdavg@gmail.com.
