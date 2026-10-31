# Nonsecret specification. Credentials and licensed extracts stay outside this folder.
project_config <- list(
  project_version = "FIN494-project1-v1",
  product = "CRSP US Stock, CIZ via WRDS (verify individual entitlement)",
  schema = "crsp",
  monthly_table = "msf_v2",
  daily_table = "dsf_v2",
  month_end = as.Date("2025-12-31"),
  lookback_start = as.Date("2025-11-01"),
  lookback_sessions = 20L,
  min_valid_days = 15L,
  cap_min_m = 1000,       # Retain the Class 05 $1 billion baseline.
  adv_min_m = 1,          # Teaching choice, USD millions/day; justify before running.
  source_cap_unit = "USD thousands",
  price_unit = "USD/share",
  volume_unit = "shares",
  documentation_confirmed = FALSE,
  source_release = "UNVERIFIED: record the subscribed release before live retrieval",
  documentation = c(
    wrds_mapping = "https://wrds-www.wharton.upenn.edu/pages/wrds-research/macros/crsp-ciztosiz-macro/",
    wrds_dictionary = "https://wrds-www.wharton.upenn.edu/data-dictionary/crsp_a_stock/",
    crsp_guide = "https://indexes.morningstar.com/docs/guide/crsp-us-stock-databases-guide-for-flat-file-format-2-0?isRdp=true"
  )
)
