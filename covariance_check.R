setwd("C:/Users/mavek/OneDrive/Desktop/IMBA Douments/semester 4/Thesis/Thesis Data")

suppressPackageStartupMessages({
  library(haven); library(tidyverse); library(labelled); library(fixest)
})

# ── rebuild panel (same code as run_regressions.R) ───────────────────────────
cons_name_map <- c(
  "100"="Gros Islet","200"="Babonneau","300"="Castries North",
  "400"="Castries East","500"="Castries Central","600"="Castries South",
  "700"="Anse-la-Raye/Canaries","800"="Soufriere","900"="Choiseul",
  "1000"="Laborie","1100"="Vieux-Fort South","1200"="Vieux-Fort North",
  "1300"="Micoud South","1400"="Micoud North","1500"="Dennery South",
  "1600"="Dennery North","1700"="Castries South-East"
)
cat("Loading data...\n")
raw_2022 <- read_sav("PersonHHoldMerge 2022 Annon.sav")
hh_2022 <- raw_2022 |>
  distinct(CompositeKey,.keep_all=TRUE) |>
  select(CompositeKey,CONSTITUENCY,Npersons,h2_3a,h2_3b1,h2_15,HHLD_WEIGHT) |>
  rename(household_id=CompositeKey,constituency_id=CONSTITUENCY,
         household_size=Npersons,tenure=h2_3a,monthly_rent=h2_3b1,
         bedrooms=h2_15,hh_weight=HHLD_WEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure,monthly_rent,bedrooms,household_size,constituency_id),
           ~if_else(.%in%c(-999999999,999999999),NA_real_,as.numeric(.))),
    constituency=cons_name_map[as.character(as.integer(constituency_id))],
    renter=if_else(tenure%in%7:8,1,0)
  ) |> filter(!is.na(constituency))

raw_2010 <- read_sav("person_house_merged.sav")
hh_2010 <- raw_2010 |>
  mutate(hid=paste(DISTRICT,ED,HH,sep="_")) |>
  group_by(hid) |>
  mutate(HWEIGHT=suppressWarnings(max(HWEIGHT,na.rm=TRUE)),
         HWEIGHT=if_else(is.infinite(HWEIGHT),NA_real_,HWEIGHT)) |>
  ungroup() |> distinct(hid,.keep_all=TRUE) |>
  select(hid,poldist,NPERS,H13_OWN,H24_BEDROOMS,HWEIGHT) |>
  rename(household_id=hid,constituency_id=poldist,household_size=NPERS,
         tenure=H13_OWN,bedrooms=H24_BEDROOMS,hh_weight=HWEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure,bedrooms,household_size,constituency_id),
           ~if_else(.%in%c(-999999999,999999999),NA_real_,as.numeric(.))),
    constituency=cons_name_map[as.character(as.integer(constituency_id))],
    renter=if_else(tenure%in%c(3,4),1,0)
  ) |> filter(!is.na(constituency))

tpi_data <- read.csv("TPI_constituency.csv") |> select(constituency,tpi)

tourism_raw <- read.csv("Selected-Tourism-Statistics.csv",header=FALSE,stringsAsFactors=FALSE)
stay_annual <- tourism_raw |>
  setNames(paste0("V",seq_len(ncol(tourism_raw)))) |>
  filter(grepl("Stay-Over Arrivals",V2,fixed=TRUE)) |>
  mutate(year=as.integer(substr(trimws(V5),1,4)),
         amount=as.numeric(gsub(",","",trimws(V6)))) |>
  filter(!is.na(year),!is.na(amount)) |>
  group_by(year) |> summarise(arrivals=sum(amount),.groups="drop")
national_shift <- (mean(stay_annual$arrivals[stay_annual$year%in%c(2015:2019,2022)]) -
                   mean(stay_annual$arrivals[stay_annual$year%in%2010:2014])) /
                   mean(stay_annual$arrivals[stay_annual$year%in%2010:2014])

gt_raw <- read.csv("google_trends_Airbnb.csv",header=TRUE,stringsAsFactors=FALSE)
colnames(gt_raw) <- c("date","index")
gt_raw$year  <- as.integer(substr(trimws(gt_raw$date),1,4))
gt_raw$index <- suppressWarnings(as.numeric(gt_raw$index))
gt_annual <- aggregate(index~year,data=gt_raw[!is.na(gt_raw$year)&!is.na(gt_raw$index),],FUN=mean)
gt_shift <- (mean(gt_annual$index[gt_annual$year%in%c(2015:2019,2022)]) -
             mean(gt_annual$index[gt_annual$year%in%2010:2014])) /
             mean(gt_annual$index[gt_annual$year%in%2010:2014])
combined_shift <- sqrt(gt_shift*national_shift)

hh_panel <- bind_rows(
  hh_2010|>mutate(year=2010L,post=0L,monthly_rent=NA_real_,bedrooms=as.numeric(bedrooms)),
  hh_2022|>mutate(year=2022L,post=1L,bedrooms=as.numeric(bedrooms))
) |>
  left_join(tpi_data,by="constituency") |>
  mutate(
    tpi_post          = tpi*national_shift*post,
    tpi_post_gt       = tpi*gt_shift*post,
    tpi_post_combined = tpi*combined_shift*post
  )

cat("Panel rows:", nrow(hh_panel), "\n")
cat("Constituencies:", n_distinct(hh_panel$constituency), "\n\n")

# ── ISSUE 1: Within-constituency clustering ───────────────────────────────────
# TPI varies only at constituency level → all HH within same constituency share
# the same treatment value → errors are correlated within constituencies.
# vcov="hetero" treats each HH as independent → SEs are too small.
# Fix: cluster at the constituency level.
# Problem: N=17 clusters → cluster-robust SEs are themselves downward-biased.
# Better fix: wild cluster bootstrap (Cameron & Miller 2015).

cat("=== ISSUE 1: Clustering at constituency level ===\n")
cat("Comparing hetero vs cluster-robust SEs for LPM (GT shift):\n\n")

lpm_hetero  <- feols(renter~tpi_post_gt|constituency+year,
                     data=hh_panel,weights=~hh_weight,vcov="hetero")
lpm_cluster <- feols(renter~tpi_post_gt|constituency+year,
                     data=hh_panel,weights=~hh_weight,vcov=~constituency)

cat(sprintf("  hetero  SE: %.6f  |  t = %.2f\n", se(lpm_hetero)["tpi_post_gt"],
            coef(lpm_hetero)["tpi_post_gt"]/se(lpm_hetero)["tpi_post_gt"]))
cat(sprintf("  cluster SE: %.6f  |  t = %.2f\n", se(lpm_cluster)["tpi_post_gt"],
            coef(lpm_cluster)["tpi_post_gt"]/se(lpm_cluster)["tpi_post_gt"]))
cat(sprintf("  SE inflation factor: %.2fx\n\n",
            se(lpm_cluster)["tpi_post_gt"]/se(lpm_hetero)["tpi_post_gt"]))

# ── All 3 LPM models with cluster SEs ────────────────────────────────────────
lpm1c <- feols(renter~tpi_post_gt       |constituency+year,data=hh_panel,weights=~hh_weight,vcov=~constituency)
lpm2c <- feols(renter~tpi_post          |constituency+year,data=hh_panel,weights=~hh_weight,vcov=~constituency)
lpm3c <- feols(renter~tpi_post_combined |constituency+year,data=hh_panel,weights=~hh_weight,vcov=~constituency)

lpm_compare <- tibble(
  model     = c("(1) GT Shift","(2) Arrivals Shift","(3) Combined Shift"),
  beta      = c(coef(lpm1c)["tpi_post_gt"], coef(lpm2c)["tpi_post"], coef(lpm3c)["tpi_post_combined"]),
  se_hetero = c(se(lpm_hetero)["tpi_post_gt"],
                se(feols(renter~tpi_post|constituency+year,data=hh_panel,weights=~hh_weight,vcov="hetero"))["tpi_post"],
                se(feols(renter~tpi_post_combined|constituency+year,data=hh_panel,weights=~hh_weight,vcov="hetero"))["tpi_post_combined"]),
  se_cluster = c(se(lpm1c)["tpi_post_gt"], se(lpm2c)["tpi_post"], se(lpm3c)["tpi_post_combined"]),
  p_cluster  = c(pvalue(lpm1c)["tpi_post_gt"], pvalue(lpm2c)["tpi_post"], pvalue(lpm3c)["tpi_post_combined"]),
  nobs      = nobs(lpm1c),
  r2        = round(r2(lpm1c,"r2"),4)
) |> mutate(across(c(beta,se_hetero,se_cluster),~round(.,6)),
            p_cluster=round(p_cluster,4),
            inflation=round(se_cluster/se_hetero,2))

cat("LPM cluster results:\n")
print(lpm_compare)
write.csv(lpm_compare,"lpm_cluster_results.csv",row.names=FALSE)

# ── Rent regression with cluster SEs ─────────────────────────────────────────
cat("\n=== Rent regression: hetero vs cluster SEs ===\n")
hh_rent_22 <- hh_panel |>
  filter(year==2022,tenure%in%7:8,!is.na(monthly_rent),
         monthly_rent>0,monthly_rent<99999,
         !is.na(bedrooms),bedrooms>0,!is.na(household_size)) |>
  mutate(log_rent=log(monthly_rent))

rent2_hetero  <- feols(log_rent~tpi+bedrooms+household_size,data=hh_rent_22,weights=~hh_weight,vcov="hetero")
rent2_cluster <- feols(log_rent~tpi+bedrooms+household_size,data=hh_rent_22,weights=~hh_weight,vcov=~constituency)

cat("Rent (with controls) — TPI coefficient:\n")
cat(sprintf("  hetero  SE: %.4f  |  t = %.2f  |  p = %.4f\n",
            se(rent2_hetero)["tpi"], coef(rent2_hetero)["tpi"]/se(rent2_hetero)["tpi"],
            pvalue(rent2_hetero)["tpi"]))
cat(sprintf("  cluster SE: %.4f  |  t = %.2f  |  p = %.4f\n",
            se(rent2_cluster)["tpi"], coef(rent2_cluster)["tpi"]/se(rent2_cluster)["tpi"],
            pvalue(rent2_cluster)["tpi"]))
cat(sprintf("  SE inflation factor: %.2fx\n\n",
            se(rent2_cluster)["tpi"]/se(rent2_hetero)["tpi"]))

rent1_cluster <- feols(log_rent~tpi,data=hh_rent_22,weights=~hh_weight,vcov=~constituency)

rent_cluster_results <- tibble(
  model = c("(4) No controls","(5) Unit controls"),
  term  = "tpi",
  beta_tpi   = c(coef(rent1_cluster)["tpi"],  coef(rent2_cluster)["tpi"]),
  se_hetero  = c(se(feols(log_rent~tpi,data=hh_rent_22,weights=~hh_weight,vcov="hetero"))["tpi"],
                 se(rent2_hetero)["tpi"]),
  se_cluster = c(se(rent1_cluster)["tpi"],     se(rent2_cluster)["tpi"]),
  p_cluster  = c(pvalue(rent1_cluster)["tpi"], pvalue(rent2_cluster)["tpi"]),
  nobs = nobs(rent1_cluster),
  r2   = c(round(r2(rent1_cluster,"r2"),4), round(r2(rent2_cluster,"r2"),4))
) |> mutate(across(c(beta_tpi,se_hetero,se_cluster),~round(.,4)),
            p_cluster=round(p_cluster,4),
            inflation=round(se_cluster/se_hetero,2))

print(rent_cluster_results)
write.csv(rent_cluster_results,"rent_cluster_results.csv",row.names=FALSE)

# Full rent table with cluster SEs
rent_full <- tibble(
  model          = c(rep("(4) No controls",2), rep("(5) Unit controls",4)),
  term           = c("tpi","(Intercept)", "tpi","bedrooms","household_size","(Intercept)"),
  estimate       = c(coef(rent1_cluster), coef(rent2_cluster)),
  se_cluster     = c(se(rent1_cluster),   se(rent2_cluster)),
  p_cluster      = c(pvalue(rent1_cluster), pvalue(rent2_cluster))
) |> mutate(across(c(estimate,se_cluster),~round(.,4)), p_cluster=round(p_cluster,4))
write.csv(rent_full,"rent_full_cluster.csv",row.names=FALSE)
cat("\nFull rent table with cluster SEs:\n")
print(rent_full)

# ── ISSUE 2: Mechanical share correlation (Bartik-specific) ──────────────────
cat("\n=== ISSUE 2: TPI share correlation across constituencies ===\n")
tpi_vals <- tpi_data$tpi
cat(sprintf("  Sum of TPI shares: %.4f (should = 1)\n", sum(tpi_vals)))
cor_mat <- cor(outer(tpi_vals,tpi_vals))
cat(sprintf("  TPI shares are mechanically constrained to sum to 1.\n"))
cat(sprintf("  Avg pairwise correlation of share*treatment: not directly estimable without listing data.\n"))
cat("  Implication: Borusyak et al. (2022) show that with few locations and a single shift,\n")
cat("  the Bartik IV is equivalent to a 2SLS with a single instrument. Validity rests on\n")
cat("  the exogeneity of shares, not on 'many shifts' diversification.\n\n")

# ── ISSUE 3: Conley spatial SEs ──────────────────────────────────────────────
cat("=== ISSUE 3: Spatial autocorrelation between constituencies ===\n")
cat("  fixest does not natively support Conley spatial SEs.\n")
cat("  With 17 constituencies on a small island, geographic spillovers are plausible.\n")
cat("  Partial fix: cluster-robust SEs (already computed above) implicitly allow\n")
cat("  arbitrary within-constituency correlation; between-constituency spatial\n")
cat("  correlation remains unaddressed.\n")
cat("  Recommendation: acknowledge as a limitation; Conley SEs require a distance matrix\n")
cat("  and the 'conleySE' or 'spatialreg' package.\n\n")

cat("=== SUMMARY ===\n")
cat("Primary fix applied: cluster-robust SEs at constituency level.\n")
cat("Key finding: SE inflation factors indicate how much current SEs are understated.\n")
cat("Results written to lpm_cluster_results.csv and rent_cluster_results.csv\n")
