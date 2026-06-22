setwd("C:/Users/mavek/OneDrive/Desktop/IMBA Douments/semester 4/Thesis/Thesis Data")

suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(labelled)
  library(fixest)
})

cons_name_map <- c(
  "100"="Gros Islet","200"="Babonneau","300"="Castries North",
  "400"="Castries East","500"="Castries Central","600"="Castries South",
  "700"="Anse-la-Raye/Canaries","800"="Soufriere","900"="Choiseul",
  "1000"="Laborie","1100"="Vieux-Fort South","1200"="Vieux-Fort North",
  "1300"="Micoud South","1400"="Micoud North","1500"="Dennery South",
  "1600"="Dennery North","1700"="Castries South-East"
)

# ── 2022 census ───────────────────────────────────────────────────────────────
cat("Loading 2022 census...\n")
raw_2022 <- read_sav("PersonHHoldMerge 2022 Annon.sav")
hh_2022 <- raw_2022 |>
  distinct(CompositeKey, .keep_all = TRUE) |>
  select(CompositeKey, CONSTITUENCY, Npersons, h2_3a, h2_3b1, h2_15, HHLD_WEIGHT) |>
  rename(household_id=CompositeKey, constituency_id=CONSTITUENCY,
         household_size=Npersons, tenure=h2_3a, monthly_rent=h2_3b1,
         bedrooms=h2_15, hh_weight=HHLD_WEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure,monthly_rent,bedrooms,household_size,constituency_id),
           ~if_else(.%in%c(-999999999,999999999),NA_real_,as.numeric(.))),
    constituency = cons_name_map[as.character(as.integer(constituency_id))],
    owner    = if_else(tenure %in% 1:6,1,0),
    renter   = if_else(tenure %in% 7:8,1,0),
    rentfree = if_else(tenure==9,1,0),
    persons_per_bedroom = if_else(bedrooms>0,household_size/bedrooms,NA_real_)
  ) |>
  filter(!is.na(constituency))

# ── 2010 census ───────────────────────────────────────────────────────────────
cat("Loading 2010 census...\n")
raw_2010 <- read_sav("person_house_merged.sav")
hh_2010 <- raw_2010 |>
  mutate(household_id_2010=paste(DISTRICT,ED,HH,sep="_")) |>
  group_by(household_id_2010) |>
  mutate(HWEIGHT=suppressWarnings(max(HWEIGHT,na.rm=TRUE)),
         HWEIGHT=if_else(is.infinite(HWEIGHT),NA_real_,HWEIGHT)) |>
  ungroup() |>
  distinct(household_id_2010,.keep_all=TRUE) |>
  select(household_id_2010,poldist,NPERS,H13_OWN,H24_BEDROOMS,HWEIGHT) |>
  rename(household_id=household_id_2010, constituency_id=poldist,
         household_size=NPERS, tenure=H13_OWN, bedrooms=H24_BEDROOMS, hh_weight=HWEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure,bedrooms,household_size,constituency_id),
           ~if_else(.%in%c(-999999999,999999999),NA_real_,as.numeric(.))),
    persons_per_bedroom=if_else(bedrooms>0,household_size/bedrooms,NA_real_),
    constituency=cons_name_map[as.character(as.integer(constituency_id))],
    owner    = if_else(tenure %in% c(1,2),1,0),
    renter   = if_else(tenure %in% c(3,4),1,0),
    rentfree = if_else(tenure==5,1,0)
  ) |>
  filter(!is.na(constituency))

# ── TPI ───────────────────────────────────────────────────────────────────────
tpi_data <- read.csv("TPI_constituency.csv") |> select(constituency,tpi,tourism_tier)

# ── shifts ────────────────────────────────────────────────────────────────────
tourism_raw <- read.csv("Selected-Tourism-Statistics.csv",header=FALSE,stringsAsFactors=FALSE)
stay_annual <- tourism_raw |>
  setNames(paste0("V",seq_len(ncol(tourism_raw)))) |>
  filter(grepl("Stay-Over Arrivals",V2,fixed=TRUE)) |>
  mutate(year=as.integer(substr(trimws(V5),1,4)),
         amount=as.numeric(gsub(",","",trimws(V6)))) |>
  filter(!is.na(year),!is.na(amount)) |>
  group_by(year) |> summarise(arrivals=sum(amount),.groups="drop")

pre_avg        <- mean(stay_annual$arrivals[stay_annual$year %in% 2010:2014])
post_avg       <- mean(stay_annual$arrivals[stay_annual$year %in% c(2015:2019,2022)])
national_shift <- (post_avg-pre_avg)/pre_avg

gt_raw   <- read.csv("google_trends_Airbnb.csv",header=TRUE,stringsAsFactors=FALSE)
colnames(gt_raw) <- c("date","index")
gt_raw$year  <- as.integer(substr(trimws(gt_raw$date),1,4))
gt_raw$index <- suppressWarnings(as.numeric(gt_raw$index))
gt_annual <- aggregate(index~year,data=gt_raw[!is.na(gt_raw$year)&!is.na(gt_raw$index),],FUN=mean)
gt_pre   <- mean(gt_annual$index[gt_annual$year %in% 2010:2014])
gt_post  <- mean(gt_annual$index[gt_annual$year %in% c(2015:2019,2022)])
gt_shift <- (gt_post-gt_pre)/gt_pre
combined_shift <- sqrt(gt_shift*national_shift)

cat(sprintf("Shifts: arrivals=%.4f  google=%.4f  combined=%.4f\n",
            national_shift, gt_shift, combined_shift))

# ── stacked panel ─────────────────────────────────────────────────────────────
hh_panel <- bind_rows(
  hh_2010 |> mutate(year=2010L,post=0L),
  hh_2022 |> mutate(year=2022L,post=1L)
) |>
  left_join(tpi_data |> select(constituency,tpi),by="constituency") |>
  mutate(
    tpi_post          = tpi*national_shift*post,
    tpi_post_gt       = tpi*gt_shift*post,
    tpi_post_combined = tpi*combined_shift*post
  )

# ── LPM regressions ───────────────────────────────────────────────────────────
cat("Running LPM regressions...\n")
lpm1 <- feols(renter~tpi_post_gt       |constituency+year, data=hh_panel, weights=~hh_weight, vcov="hetero")
lpm2 <- feols(renter~tpi_post          |constituency+year, data=hh_panel, weights=~hh_weight, vcov="hetero")
lpm3 <- feols(renter~tpi_post_combined |constituency+year, data=hh_panel, weights=~hh_weight, vcov="hetero")

extract_lpm <- function(m, label) {
  cf <- coef(m); se <- se(m); pv <- pvalue(m)
  tibble(
    model       = label,
    term        = names(cf),
    estimate    = round(cf, 6),
    std_error   = round(se, 6),
    p_value     = round(pv, 4),
    nobs        = nobs(m),
    r2          = round(r2(m,"r2"), 4),
    adj_r2      = round(r2(m,"ar2"), 4)
  )
}

lpm_results <- bind_rows(
  extract_lpm(lpm1,"(1) GT Shift"),
  extract_lpm(lpm2,"(2) Arrivals Shift"),
  extract_lpm(lpm3,"(3) Combined Shift")
)
write.csv(lpm_results, "lpm_results.csv", row.names=FALSE)
cat("LPM results written to lpm_results.csv\n")
print(lpm_results)

# ── Rent regressions ──────────────────────────────────────────────────────────
cat("\nRunning rent regressions...\n")
hh_rent_22 <- hh_panel |>
  filter(year==2022, tenure %in% 7:8,
         !is.na(monthly_rent), monthly_rent>0, monthly_rent<99999,
         !is.na(bedrooms), bedrooms>0, !is.na(household_size)) |>
  mutate(log_rent=log(monthly_rent))

rent1 <- feols(log_rent~tpi,                          data=hh_rent_22, weights=~hh_weight, vcov="hetero")
rent2 <- feols(log_rent~tpi+bedrooms+household_size,  data=hh_rent_22, weights=~hh_weight, vcov="hetero")

extract_rent <- function(m, label) {
  cf <- coef(m); se <- se(m); pv <- pvalue(m)
  tibble(
    model     = label,
    term      = names(cf),
    estimate  = round(cf, 6),
    std_error = round(se, 6),
    p_value   = round(pv, 4),
    nobs      = nobs(m),
    r2        = round(r2(m,"r2"), 4),
    adj_r2    = round(r2(m,"ar2"), 4)
  )
}

rent_results <- bind_rows(
  extract_rent(rent1,"(4) No controls"),
  extract_rent(rent2,"(5) Unit controls")
)
write.csv(rent_results, "rent_results.csv", row.names=FALSE)
cat("Rent results written to rent_results.csv\n")
print(rent_results)

# ── constituency descriptive stats ────────────────────────────────────────────
cat("\nBuilding constituency summary...\n")
cons_2022 <- hh_2022 |>
  filter(!is.na(hh_weight)) |>
  group_by(constituency) |>
  summarise(
    renter_rate_22   = weighted.mean(renter, hh_weight, na.rm=TRUE),
    owner_rate_22    = weighted.mean(owner,  hh_weight, na.rm=TRUE),
    .groups="drop"
  )

cons_2010 <- hh_2010 |>
  filter(!is.na(hh_weight)) |>
  group_by(constituency) |>
  summarise(
    renter_rate_10   = weighted.mean(renter, hh_weight, na.rm=TRUE),
    owner_rate_10    = weighted.mean(owner,  hh_weight, na.rm=TRUE),
    .groups="drop"
  )

cons_wide <- cons_2010 |>
  left_join(cons_2022, by="constituency") |>
  left_join(tpi_data,  by="constituency") |>
  mutate(
    d_renter = (renter_rate_22 - renter_rate_10) * 100,
    d_owner  = (owner_rate_22  - owner_rate_10)  * 100
  ) |>
  arrange(desc(tpi))

# median rent by constituency
med_rent <- hh_2022 |>
  filter(tenure %in% 7:8, !is.na(monthly_rent), monthly_rent>0, monthly_rent<99999,
         !is.na(hh_weight)) |>
  group_by(constituency) |>
  summarise(
    median_rent = median(rep(monthly_rent, times=pmax(round(hh_weight),1))),
    n_renters   = n(),
    .groups="drop"
  )

cons_desc <- cons_wide |>
  left_join(med_rent, by="constituency") |>
  mutate(
    renter_rate_10_pct = round(renter_rate_10*100,1),
    renter_rate_22_pct = round(renter_rate_22*100,1),
    owner_rate_10_pct  = round(owner_rate_10*100,1),
    owner_rate_22_pct  = round(owner_rate_22*100,1),
    d_renter           = round(d_renter,1),
    d_owner            = round(d_owner,1),
    tpi_pct            = round(tpi*100,1),
    median_rent        = round(median_rent)
  )
write.csv(cons_desc, "cons_desc.csv", row.names=FALSE)
cat("Constituency descriptive stats written to cons_desc.csv\n")
print(cons_desc |> select(constituency, tpi_pct, renter_rate_10_pct, renter_rate_22_pct,
                           d_renter, owner_rate_10_pct, owner_rate_22_pct, d_owner, median_rent))

cat("\nDone.\n")
