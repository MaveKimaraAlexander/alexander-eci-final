---
output:
  pdf_document: default
  html_document: default
  word_document: default
---
# Thesis Explainer: Code, Methodology, and Findings
**How Short-Term Rentals Affected the St. Lucian Housing Market**
*Plain-language guide to what the code does and what the results mean*

---

## Part 1 — The Big Picture: What Was the Study Doing?

The core question: **Did Airbnb-style platforms (STRs) make housing worse for people in Saint Lucia?**

To answer this, the study compared how renter rates, owner rates, rents, and crowding changed between **2010 and 2022** across all **17 constituencies** — and tested whether areas that were *naturally* more attractive for tourism (beaches, volcanoes, etc.) experienced **bigger housing market changes** during the Airbnb era.

The challenge: Airbnb didn't set up randomly. It went to areas that were already popular. So a simple comparison of "touristy areas vs. non-touristy areas" would just reflect pre-existing differences, not causation. The Bartik design solves this.

---

## Part 2 — The Bartik Instrument: What It Is and Why

### The Problem It Solves

You can't just regress "STR units" on "renter rate" — STR platforms went to places that were already popular, so correlation does not equal causation. Maybe Gros Islet had rising rents *before* Airbnb because it was always desirable.

### The Solution: Bartik Shift-Share

The Bartik instrument combines two things:

| Component | What it is | How measured |
|---|---|---|
| **Share** = NTE | Each constituency's *pre-existing* natural attractiveness | Weighted score of beaches, peaks, waterfalls, volcanoes, etc. from OpenStreetMap. **Fixed before Airbnb existed.** |
| **Shift** = $\Delta g$ | How much national tourism *grew* after Airbnb launched | 18.7% increase in stay-over arrivals from pre-2014 average to post-launch average |

**The instrument = NTE × $\Delta g$ × Post**

This says: *"Areas with more natural beauty should have been hit harder by the national tourism boom after 2014."* Because the natural features (beaches, Pitons, hot springs) existed long before Airbnb, they couldn't have been *caused* by housing market changes.

### In Plain English

- Soufrière has the Pitons and volcanic springs → high NTE
- Dennery North has no famous natural features → low NTE
- When Airbnb launched and tourism grew 18.7% nationally, Soufrière should have absorbed more of that growth than Dennery North
- If renter rates rose more in Soufrière than in Dennery North *after* 2014, and this lines up with the NTE, that's the causal signal

---

## Part 3 — The Data

| Dataset | What it contains | Years |
|---|---|---|
| 2010 Census (person_house_merged.sav) | Tenure type, household size, bedrooms for ~50,000 households | 2010 (pre-Airbnb) |
| 2022 Census (PersonHHoldMerge 2022 Annon.sav) | Same + monthly rent | 2022 (post-Airbnb) |
| IDDetail (DwellingStatus) | Building-level records — which buildings are STR-flagged | 2022 |
| NTE_constituency.csv | Natural Tourism Endowment score per constituency | Fixed (pre-2014) |
| Selected-Tourism-Statistics.csv | Annual stay-over arrivals | 2010–2022 |

### How Tenure Was Coded

**Owner-occupied:**
- 2010: H13_OWN codes 1 (owned fully) or 2 (mortgage)
- 2022: h2_3a codes 1–6 (six categories = owned fully + mortgage, split by gender and joint ownership)

**Renter:**
- 2010: H13_OWN codes 3 (private) or 4 (government)
- 2022: h2_3a codes 7 (private) or 8 (government)

**Rent-free:**
- 2010: code 5
- 2022: code 9

```r
# 2022 owner/renter coding
owner    = if_else(tenure %in% 1:6,  1, 0)   # codes 1-6 = all ownership types
renter   = if_else(tenure %in% 7:8,  1, 0)   # codes 7-8 = renting
rentfree = if_else(tenure == 9,       1, 0)

# 2010 owner/renter coding
owner    = if_else(tenure %in% c(1,2), 1, 0)   # 1=owned, 2=mortgage
renter   = if_else(tenure %in% c(3,4), 1, 0)   # 3=private rent, 4=govt rent
rentfree = if_else(tenure == 5,         1, 0)
```

### How STR Units Were Counted

The 2022 building file (IDDetail) has a `DwellingStatus` variable. Two codes = STR:
- Code 8: "Short Term Occupation" (n = 162 buildings)
- Code 12: "AirBNB" (n = 83 buildings)
- **Total narrow STR = 245 buildings**

Because the building file is at the **district level** (10 districts) but the analysis is at the **constituency level** (17 constituencies), STR units were split across constituencies using each constituency's share of 2010 household weights within its district.

```r
str_unit = if_else(DwellingStatus %in% c(8, 12), 1L, 0L)  # narrow
str_unit_broad = if_else(DwellingStatus %in% c(4, 8, 12), 1L, 0L)  # broad (adds seasonal vacant)
```

### The Tourism Shift

```r
pre_avg  = mean(arrivals for 2010-2014)          # = 316,385
post_avg = mean(arrivals for 2015-2019, 2022)    # = 375,610 (pandemic years excluded)
national_shift = (post_avg - pre_avg) / pre_avg  # = 0.187 = 18.7%
```

### The Bartik Instrument (constructed in the data)

```r
# nte_post = the instrument: NTE × national shift × post-period indicator
nte_post = nte * national_shift * post   # post = 0 in 2010, 1 in 2022
```

This variable is **0 for every household in 2010** (because post = 0).
For 2022, it equals each constituency's NTE × 0.187.

---

## Part 4 — The Regression Models

### What "TWFE" and "fixed effects" mean

**Two-Way Fixed Effects (TWFE)** means the regression controls for:
1. **Constituency fixed effects ($\gamma_c$):** removes all time-invariant differences between constituencies (e.g., Gros Islet was always more urban)
2. **Year fixed effects ($\delta_t$):** removes any change that happened *everywhere* between 2010 and 2022

After controlling for both, the coefficient on `nte_post` captures only: **"did high-NTE constituencies change more than low-NTE constituencies after 2014?"**

---

## Part 5 — Research Question by Research Question

---

### RQ1: Rent Inflation
*"Do renters in tourism-intensive areas face higher rents?"*

**Was the Bartik used here? NO.**

The 2010 census didn't collect rent data, so there is no rent *change* to measure. The rent model is a **2022 cross-section only** — one year of data across the 17 constituencies.

**Model:**
```r
log_rent ~ nte + bedrooms + household_size + persons_per_bedroom
# Sample: renter households in 2022 only
# No fixed effects (NTE is a constituency-level variable — it would be collinear with constituency FEs)
# SEs clustered by constituency (17 clusters)
```

**What the coefficient means:**
- The NTE coefficient on log(rent) tells you how much higher rents are in high-NTE constituencies vs. low-NTE ones, holding dwelling size constant

**Finding:**
- Implied rent premium of **~11%** between highest-NTE and lowest-NTE constituency
- **p = 0.833 (not statistically significant)** — with only 17 clusters the standard errors are far too wide to draw a confident conclusion, and the point estimate itself is modest rather than economically large
- This is the **weakest finding** in the thesis

---

### RQ2: Stock Depletion (Renter Rate)
*"Did STR expansion increase the renter share of households in high-tourism areas?"*

**Was the Bartik used here? YES — this is the MAIN result.**

**Step 1: Reduced Form** (direct effect of the instrument on the outcome)
```r
lpm <- feols(renter ~ nte_post | constituency + year,
             data = hh_panel, weights = ~hh_weight, vcov = ~constituency)
```
- Outcome: 1 if the household rents, 0 otherwise
- `nte_post` = NTE × 0.187 × Post (the Bartik instrument)
- Constituency + year fixed effects
- Household-weighted
- **Result: $\hat\beta$ = 2.106, SE = 0.586, Wald F = 12.9, p = 0.002 — SIGNIFICANT**

**What 2.106 means:**
Moving from a constituency with NTE = 0 (no natural features) to a constituency with NTE = 1 (all natural features) is associated with a **2.11 percentage point larger increase** in the renter rate during the STR era. This is after controlling for constituency baseline levels and year trends.

In practice, most constituencies have NTE between 0.01 and 0.25, so the differences are more modest — but the gradient is clear and statistically significant (p = 0.002).

**Step 2: First Stage** (does the instrument predict STR units?)
```r
fs_narrow <- feols(str_count ~ nte_post | constituency + year,
                   data = panel, vcov = "hetero")
```
- Outcome: estimated STR units per constituency
- If NTE × $\Delta g$ × Post predicts where STR units concentrated, the instrument is "relevant"
- **Issue: First-stage F < 10** — this is because there are only 34 observations (17 constituencies × 2 years), which is very few

**Step 3: 2SLS** (causal effect of STR units on renter rate)
```r
iv_narrow <- feols(renter ~ 1 | constituency + year | str_count ~ nte_post,
                   data = hh_panel, weights = ~hh_weight, vcov = "hetero")
```
- This instruments `str_count` with `nte_post`
- The 2SLS coefficient answers: "for each additional STR unit (instrumented), how much did the renter rate change?"
- **Caveat: The first-stage F is below 10, so the 2SLS estimates should be interpreted cautiously. The reduced form (Step 1) is the primary evidence.**

**Three specifications tested:**
1. 17 constituencies × narrow STR (codes 8+12) → **PRIMARY**
2. 17 constituencies × broad STR (codes 4+8+12) → robustness
3. 10 districts × narrow STR → robustness (different geography)
All three point in the same direction.

---

### RQ3: Homeownership (Tenure-Shift Externality)
*"Did high-NTE areas also see rising owner rates?"*

**Was the Bartik used here? YES (reduced form only).**

```r
lpm_owner <- feols(owner ~ nte_post | constituency + year,
                   data = hh_panel, weights = ~hh_weight, vcov = ~constituency)
```
- Same structure as the renter model, but outcome = 1 if owner-occupied
- **Finding: NEGATIVE coefficient** — ownership FELL in high-NTE areas (the opposite of the original hypothesis)
- $\hat\beta$ = -1.975, p = 0.068 (17 constituencies); $\hat\beta$ = -2.726, p < 0.001 (10 districts)

**What this means:**
- Renter rates rose (RQ2) while owner rates fell in high-NTE constituencies — the two did not rise together
- Interpretation: rather than rising property values simultaneously attracting wealthier owner-occupiers/investors *and* renters, the data point to a single tenure-conversion channel — previously owner-occupied (and rent-free) housing being converted into rental and STR stock as tourism pressure increased
- The scatter plot subtitle now reads: **"negative slope = owner-occupancy fell in high-NTE areas"**
- The district-level result is statistically significant (p < 0.001); the constituency-level result is marginal (p = 0.068)

---

### RQ4: Crowding (Persons per Bedroom)
*"Did crowding change in high-NTE areas?"*

**Was the Bartik used here? YES (reduced form only).**

```r
lpm_ppbr <- feols(persons_per_bedroom ~ nte_post | constituency + year,
                  data = hh_panel, weights = ~hh_weight, vcov = ~constituency)
```
- Outcome: persons per bedroom (continuous, not binary — so this is OLS not LPM)
- **Finding: NOT ROBUST** — the sign flips depending on geographic unit, and neither estimate is significant
- $\hat\beta$ = +0.692, p = 0.276 (17 constituencies — positive, i.e. crowding rose, not significant)
- $\hat\beta$ = -0.208, p = 0.638 (10 districts — negative, i.e. crowding fell, not significant)

**What this means:**
- There is no reliable evidence that crowding changed systematically with tourism exposure in either direction
- The displacement-via-crowding mechanism originally hypothesised is not supported by this analysis
- The scatter plot subtitle now reads: **"Sign is sensitive to geographic aggregation; neither estimate is significant (Table 13)"**

---

## Part 6 — Robustness Checks (Was the Main Result Real?)

| Test | What it does | What it checks |
|---|---|---|
| **Broad STR measure** | Adds code 4 (seasonal vacant) to STR count | Are results sensitive to the STR definition? |
| **District aggregation** | Runs same model at 10-district level instead of 17 constituencies | Are results sensitive to geography? |
| **Permutation test** | Randomly shuffles NTE across constituencies 999 times, re-runs the model each time | Could the result arise by chance with this spatial distribution? |
| **Leave-one-out** | Drops one constituency at a time, re-runs 17 times | Is the result driven by one outlier (e.g. Gros Islet)? |
| **Balance test** | Regresses 2010 baseline characteristics (renter rate, owner rate, etc.) on NTE | Did high-NTE areas already have different housing markets *before* Airbnb? |
| **Anderson-Rubin CI** | Builds a confidence interval for the 2SLS estimate that is valid even with a weak first stage | If F < 10, does the weak instrument distort the confidence intervals? |

**Key results from robustness checks:**
- **Permutation test:** The observed coefficient is very unlikely to arise from random NTE assignment → confirms the spatial pattern is real
- **Leave-one-out:** All 17 sub-samples produce positive coefficients → no single constituency is driving the result
- **Balance test:** NTE is correlated with some 2010 baseline levels, but this doesn't threaten the design because fixed effects absorb level differences — only pre-existing *trends* would be a problem
- **Anderson-Rubin:** If the AR confidence interval is similar in width to the 2SLS Wald interval, the weak first stage is not distorting inference

---

## Part 7 — Summary of All Findings

| RQ | Question | Method | Key Result | Significance |
|---|---|---|---|---|
| **RQ2** | Did STR cause more renting in high-NTE areas? | Bartik reduced form (TWFE) | **+2.106 pp more renting per unit NTE** | p = 0.002 — STRONG |
| **RQ1** | Are rents higher in high-NTE areas? | 2022 cross-section OLS | **~11% rent premium at highest NTE** | p = 0.833 — NOT SIGNIFICANT |
| **RQ3** | Did ownership rise in high-NTE areas? | Bartik reduced form (TWFE) | **Negative — ownership FELL, not rose** | p = 0.068 (17 const.), p < 0.001 (10 dist.) |
| **RQ4** | Did crowding fall in high-NTE areas? | Bartik reduced form (TWFE) | **Not robust — sign flips by geography** | p = 0.276 (17 const., positive), p = 0.638 (10 dist., negative) |

### The Story in One Paragraph

After Airbnb and similar platforms launched in Saint Lucia around 2014, constituencies with the most natural attractions (beaches, volcanoes, waterfalls) — Soufrière, Gros Islet, Anse-la-Raye — saw the sharpest rises in renter populations. These same areas saw ownership rates *fall* rather than rise, and show no robust change in crowding. This pattern points to a single tenure-conversion channel rather than the two-channel sorting story originally hypothesised: tourism expansion tightened the rental market and converted previously owner-occupied or informally-held housing into rental (and STR) stock, rather than simultaneously attracting new owner-occupier and investor demand. The rent premium in high-tourism areas (~11%) is directionally consistent with this story but can't be confirmed statistically with only 17 observations.

---

## Part 8 — What the Bartik Was and Was NOT Used For

| Outcome | Bartik used? | Notes |
|---|---|---|
| Renter rate (RQ2) | **YES** — main result | Reduced form + first stage + 2SLS |
| STR unit count | **YES** — first stage | NTE × $\Delta g$ × Post predicts where STRs concentrated |
| Log monthly rent (RQ1) | **NO** | 2022 cross-section only; no rent data in 2010 |
| Owner rate (RQ3) | **YES** — reduced form | Same instrument, different outcome |
| Persons per bedroom (RQ4) | **YES** — reduced form | Same instrument, different outcome |

---

*Document generated 2026-06-16. All statistics from MAVE_Thesis_Progress_Update_6.qmd.*
