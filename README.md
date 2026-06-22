# How Short-Term Rentals Affected the St. Lucian Housing Market
### A Bartik Shift-Share Instrumental Variable Analysis

**Author:** Mave Kimara Alexander | 113077424  
**Degree:** International MBA  
**Date:** 2026

---

## Overview

This repository contains the replication code for my thesis, which investigates whether the expansion of short-term rental (STR) platforms has worsened housing affordability for local renters in Saint Lucia. Using a Bartik shift-share instrumental variable design applied to full-population census microdata from 2010 and 2022, the study finds that constituencies with greater pre-platform natural tourism endowments experienced significantly larger increases in renter populations during the STR era.

---

## Repository Contents

| File | Description |
|---|---|
| `eci_str_replication.R` | Standalone reproducible R script — all data loading, models, figures, and tables |
| `eci_str_thesis.qmd` | Full Quarto document — renders the complete PDF thesis |
| `NTE_constituency.csv` | Natural Tourism Endowment scores by constituency (Bartik share) |
| `natural_amenities_nte_assigned.csv` | Raw OSM natural features with GPS coordinates, weights, and constituency assignment |
| `nte_district.csv` | District-level NTE used in district robustness checks |
| `Selected-Tourism-Statistics.csv` | Annual stay-over arrivals 2010–2025, CSO Saint Lucia |
| `hh_panel_replication.csv` | Household-level analysis panel (100,829 rows × 16 cols) — exact variables used in all regressions, derived from the 2010 and 2022 census microdata with no personal identifiers |
| `iddetail_str.csv` | Building-level STR classification (77,615 rows) — district, narrow STR flag, broad STR flag; derived from the 2022 IDDetail enumeration |
| `cons_wide.csv` | Constituency-level aggregated housing outcomes (2010 & 2022) — derived from census microdata; behind Figures 3, 5, 9, 10 and Table 6 |
| `dist_wide.csv` | District-level aggregated housing outcomes (2010 & 2022) — derived from census microdata; behind Figure 6 |
| `NTE_Map_Saint_Lucia.png` | District-level NTE choropleth map — embedded in Figure 2 of the thesis |
| `map_001.png` | Saint Lucia constituency boundary reference map — embedded in Figure 2 of the thesis |

---

## Data Not Included (Confidential)

The original analysis was conducted on the full census microdata files provided by the **Central Statistical Office (CSO), Saint Lucia**. These files cannot be shared publicly. For this replication package, the variables required for all regressions and figures were extracted from the raw files and saved as `hh_panel_replication.csv` and `iddetail_str.csv` (see Repository Contents above). All results in the thesis are reproducible from those derived files alone.

Researchers who wish to replicate the full derivation pipeline from raw microdata should contact the CSO directly to request access to the files below.

| File | Description |
|---|---|
| `PersonHHoldMerge 2022 Annon.sav` | 2022 Population and Housing Census — household microdata |
| `person_house_merged.sav` | 2010 Population and Housing Census — household microdata |
| `IDDetail_merged_Anon_Weights_DwellStatus.sav` | 2022 Building Enumeration File — dwelling status for 77,742 structures |

---

## How to Replicate

### Requirements

Install the following R packages:

```r
install.packages(c(
  "haven", "tidyverse", "labelled", "knitr", "scales",
  "ggrepel", "kableExtra", "fixest", "modelsummary"
))
```

### Running the analysis

The three `.sav` census files are required to run the full script. If you do not have them, `hh_panel_replication.csv` and `iddetail_str.csv` (included in this repo) contain all variables needed to reproduce every regression and figure — no `.sav` files required for those steps.

1. Obtain the three confidential `.sav` files from the CSO and place them in the same directory as the R script (or skip this step if using the pre-processed CSVs).
2. Open `eci_str_replication.R` and update the `setwd()` path at the top of the script to match your local directory:

```r
setwd("path/to/your/project")
```

3. Run the script top-to-bottom. Figures are saved as PNG files in the working directory; regression tables print to the console.

### Rendering the full PDF

With [Quarto](https://quarto.org) installed, render the thesis document from the terminal:

```bash
quarto render "eci_str_thesis.qmd"
```

---

## Identification Strategy

The study uses a **Bartik shift-share instrument**:

$$Z_{ct} = NTE_c \times \Delta g \times \text{Post}_t$$

- **NTE** (Natural Tourism Endowment): each constituency's pre-platform share of island-wide natural amenities (beaches, peaks, waterfalls, volcanic features, nature reserves, viewpoints) — sourced from OpenStreetMap, May 2026.
- **Δg** (national shift): growth in island-wide stay-over arrivals from the pre-STR mean (2010–2014: 316,385) to the post-launch mean (2015–2019, 2022: 375,610), equal to **+18.7%**.
- Because NTE is fixed by geography and predates the platform era, it is unaffected by post-2014 housing market changes.

---

## Key Results

| Research Question | Finding | p-value |
|---|---|---|
| RQ2 — Stock depletion | High-NTE constituencies saw significantly larger increases in renter rates (β̂ RF = 2.106) | < 0.05 |
| RQ1 — Rent inflation | ~18% rent premium in highest vs. lowest NTE constituency | Not significant at 17-cluster level |
| RQ3 — Ownership | Owner-occupancy *fell* in high-NTE areas — tenure conversion, not gentrification | < 0.10 |
| RQ4 — Crowding | No robust evidence of crowding effect in either direction | Not significant |

---

## AI Use

Claude Code (Anthropic, claude-sonnet-4-6) was used as a coding assistant during this project. Specifically, it assisted with: debugging R data wrangling code, refining ggplot2 figure formatting, structuring the Bartik IV pipeline, and preparing this replication package (renaming files, writing the `.gitignore`, and setting up the GitHub repository). All econometric design decisions, interpretation of results, and written analysis are the author's own.

---

## Contact

For questions about the data or methodology, please contact the CSO Saint Lucia or reach out via the university.
