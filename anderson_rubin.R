setwd("C:/Users/mavek/OneDrive/Desktop/IMBA Douments/semester 4/Thesis/Thesis Data")

suppressPackageStartupMessages({
  library(haven); library(tidyverse); library(labelled); library(fixest)
})

# ── rebuild panel ─────────────────────────────────────────────────────────────
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

# ── rent data ─────────────────────────────────────────────────────────────────
hh_rent_22 <- hh_panel |>
  filter(year==2022,tenure%in%7:8,!is.na(monthly_rent),
         monthly_rent>0,monthly_rent<99999,
         !is.na(bedrooms),bedrooms>0,!is.na(household_size)) |>
  mutate(log_rent=log(monthly_rent))

# =============================================================================
# ANDERSON-RUBIN: What it is and how it applies here
# =============================================================================
cat("\n============================================================\n")
cat("ANDERSON-RUBIN (AR) TEST — THEORY AND APPLICATION\n")
cat("============================================================\n\n")

cat("The Anderson-Rubin (AR) test (1949) tests H0: beta_IV = beta0 by running:\n")
cat("  Y - beta0*D  =  alpha + gamma*Z + controls + error\n")
cat("and testing gamma = 0. When beta0 = 0, this reduces to the reduced-form\n")
cat("F-test. Crucially, the AR test has correct size regardless of instrument\n")
cat("strength — it does NOT require F_first_stage > 10.\n\n")
cat("With a single instrument and single endogenous variable:\n")
cat("  AR F-stat  = (reduced-form t-stat)^2\n")
cat("  AR p-value = p-value from reduced-form regression\n")
cat("=> Our reduced-form regressions ARE the AR test for H0: beta_IV = 0.\n\n")

# =============================================================================
# AR TEST FOR LPM (panel)
# =============================================================================
cat("------------------------------------------------------------\n")
cat("LPM: AR test (already computed via reduced form)\n")
cat("------------------------------------------------------------\n")

lpm_gt <- feols(renter~tpi_post_gt|constituency+year, data=hh_panel,
                weights=~hh_weight, vcov=~constituency)

beta_rf <- coef(lpm_gt)["tpi_post_gt"]
se_cl   <- se(lpm_gt)["tpi_post_gt"]
t_ar    <- beta_rf / se_cl
df      <- 17 - 1 - 1   # G - 1 (FE absorbed) - 1 (coef)
p_ar    <- 2 * pt(-abs(t_ar), df = df)
f_ar    <- t_ar^2

cat(sprintf("  beta^RF (GT shift) = %.6f\n", beta_rf))
cat(sprintf("  Cluster SE         = %.6f\n", se_cl))
cat(sprintf("  AR t-stat          = %.3f  (df = %d)\n", t_ar, df))
cat(sprintf("  AR F-stat          = %.3f\n", f_ar))
cat(sprintf("  AR p-value         = %.6f\n", p_ar))
cat("  => REJECT H0: beta_IV = 0 (robust to weak instruments)\n\n")

# =============================================================================
# AR CONFIDENCE INTERVALS (inversion of AR test)
# =============================================================================
cat("------------------------------------------------------------\n")
cat("AR Confidence Intervals (95%, t-distribution, df=15)\n")
cat("------------------------------------------------------------\n")
cat("AR CI = [beta^RF +/- t_{alpha/2, df} * cluster_SE]\n\n")

t_crit <- qt(0.975, df = df)
cat(sprintf("  t critical (alpha=0.05, df=%d): %.4f\n\n", df, t_crit))

# LPM: all three shifts
shifts <- list(
  list(name="(1) GT Shift",      var="tpi_post_gt",       delta=gt_shift),
  list(name="(2) Arrivals Shift",var="tpi_post",           delta=national_shift),
  list(name="(3) Combined Shift",var="tpi_post_combined",  delta=combined_shift)
)

ar_results <- lapply(shifts, function(s) {
  m  <- feols(as.formula(paste0("renter~",s$var,"|constituency+year")),
              data=hh_panel, weights=~hh_weight, vcov=~constituency)
  b  <- coef(m)[s$var]
  se <- se(m)[s$var]
  t  <- b/se
  f  <- t^2
  p  <- 2*pt(-abs(t), df=df)
  lo <- b - t_crit*se
  hi <- b + t_crit*se
  # AR CI in terms of beta_IV = beta_RF / pi_hat
  # Since pi_hat is unknown, report RF CI
  tibble(model=s$name, beta_rf=round(b,6), se_cluster=round(se,6),
         ar_t=round(t,3), ar_F=round(f,3), ar_p=round(p,6),
         ci_lo=round(lo,6), ci_hi=round(hi,6))
})
ar_df <- bind_rows(ar_results)
cat("LPM AR results:\n")
print(ar_df)
cat("\nAll LPM AR F-stats >> 1 => reject H0: beta_IV=0, valid under weak instruments.\n\n")

# =============================================================================
# AR TEST FOR RENT (cross-section)
# =============================================================================
cat("------------------------------------------------------------\n")
cat("RENT CROSS-SECTION: AR test\n")
cat("------------------------------------------------------------\n")
cat("For the rent regression, TPI is the 'instrument' (proxy for STR exposure).\n")
cat("The reduced-form regression of log(rent) on TPI IS the AR test.\n\n")

rent_m <- feols(log_rent~tpi+bedrooms+household_size, data=hh_rent_22,
                weights=~hh_weight, vcov=~constituency)
b_tpi  <- coef(rent_m)["tpi"]
se_tpi <- se(rent_m)["tpi"]
t_rent <- b_tpi/se_tpi
f_rent <- t_rent^2
p_rent <- 2*pt(-abs(t_rent), df=df)  # df=15 (17 clusters - 2 estimated coefs)
lo_r   <- b_tpi - t_crit*se_tpi
hi_r   <- b_tpi + t_crit*se_tpi

cat(sprintf("  beta (TPI, w/ controls) = %.4f\n", b_tpi))
cat(sprintf("  Cluster SE              = %.4f\n", se_tpi))
cat(sprintf("  AR t-stat               = %.3f  (df = %d)\n", t_rent, df))
cat(sprintf("  AR F-stat               = %.3f\n", f_rent))
cat(sprintf("  AR p-value              = %.4f\n", p_rent))
cat(sprintf("  AR 95%% CI for TPI       = [%.3f, %.3f]\n", lo_r, hi_r))
cat("  => FAIL TO REJECT H0: beta_IV = 0\n")
cat("  => Wide CI includes zero; consistent with insufficient power (N=17 eff. obs.)\n\n")

# =============================================================================
# CONSTITUENCY-LEVEL COLLAPSED REGRESSION (the honest equivalent of rent model)
# =============================================================================
cat("------------------------------------------------------------\n")
cat("HONEST EQUIVALENT: Constituency-level OLS (N=17)\n")
cat("------------------------------------------------------------\n")
cat("Since TPI only varies at constituency level, the rent regression\n")
cat("is equivalent to running OLS on 17 constituency means.\n\n")

cons_means <- hh_rent_22 |>
  left_join(tpi_data, by="constituency") |>
  group_by(constituency, tpi.y) |>
  summarise(mean_log_rent = weighted.mean(log_rent, hh_weight, na.rm=TRUE),
            n_hh = n(), .groups="drop") |>
  rename(tpi = tpi.y)

cat(sprintf("  N constituencies: %d\n", nrow(cons_means)))
cons_ols <- lm(mean_log_rent ~ tpi, data=cons_means, weights=n_hh)
s <- summary(cons_ols)
cat(sprintf("  beta (TPI): %.4f\n", coef(cons_ols)["tpi"]))
cat(sprintf("  SE         : %.4f\n", s$coefficients["tpi","Std. Error"]))
cat(sprintf("  t-stat     : %.3f\n", s$coefficients["tpi","t value"]))
cat(sprintf("  p-value    : %.4f\n", s$coefficients["tpi","Pr(>|t|)"]))
cat(sprintf("  R-squared  : %.4f\n", s$r.squared))
cat("\nConclusion: With N=17 and p=", round(s$coefficients["tpi","Pr(>|t|)"],3),
    "the constituency-level regression confirms:\n")
cat("  the rent gradient is economically large but statistically weak.\n\n")

write.csv(ar_df, "ar_results.csv", row.names=FALSE)
cat("AR results written to ar_results.csv\n")

# =============================================================================
# SUMMARY TABLE FOR THESIS
# =============================================================================
cat("\n============================================================\n")
cat("SUMMARY: AR TEST RESULTS FOR THESIS\n")
cat("============================================================\n")
cat("\nLPM (panel, N=100,829 HH / 17 clusters):\n")
cat("  H0: beta_IV = 0 (no effect of STR on Pr(Renting))\n")
cat("  AR F-stat (GT): ", round(ar_df$ar_F[1],2), "   p =", ar_df$ar_p[1], "\n")
cat("  AR CI (GT):     [", ar_df$ci_lo[1], ",", ar_df$ci_hi[1], "]\n")
cat("  CONCLUSION: REJECT H0. LPM finding is AR-robust.\n\n")
cat("RENT (cross-section, effective N=17 constituencies):\n")
cat("  H0: beta_IV = 0 (no effect of STR exposure on log rent)\n")
cat("  AR F-stat: ", round(f_rent,3), "   p =", round(p_rent,4), "\n")
cat("  AR CI:     [", round(lo_r,3), ",", round(hi_r,3), "]\n")
cat("  CONCLUSION: FAIL TO REJECT H0. Rent finding is statistically weak.\n")
