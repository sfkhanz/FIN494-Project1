# FIN494-project1-synthetic-v1: invented records for testing the workflow.
# This is separate from the Class 05 and Class 06 fixtures, and is not a WRDS extract.
# source() defines the generator only. Students predict results before screening.
synthetic_project_data <- function(config) {
  if (!identical(config$month_end, as.Date("2025-12-31")) ||
      config$lookback_sessions != 20L)
    stop("The supplied synthetic fixture uses 2025-12-31 and 20 sessions.")
  # Invented teaching calendar; not certification of a vendor market calendar.
  sessions <- as.Date(c("2025-12-03", "2025-12-04", "2025-12-05", "2025-12-08",
    "2025-12-09", "2025-12-10", "2025-12-11", "2025-12-12", "2025-12-15",
    "2025-12-16", "2025-12-17", "2025-12-18", "2025-12-19", "2025-12-22",
    "2025-12-23", "2025-12-24", "2025-12-26", "2025-12-29", "2025-12-30",
    "2025-12-31"))
  monthly <- data.frame(
    permno = 990001:990016,
    permco = c(880001, 880001, 880002:880015),
    mthcaldt = config$month_end,
    mthcap = c(1000, 1800, 2200, 3000, 800, NA, 1500, 1600,
               2500, 2000, 1300, 1700, 0, 1200, 1400, 1900) * 1000,
    sharetype = "NS", securitytype = "EQTY", securitysubtype = "COM",
    usincflg = "Y", issuertype = "CORP", primaryexch = "N",
    conditionaltype = "RW", tradingstatusflg = "A",
    stringsAsFactors = FALSE
  )
  monthly$securitytype[3] <- "FUND"
  monthly$securitysubtype[3] <- "ETF"
  monthly$securitysubtype[4] <- "PREF"
  monthly$primaryexch[9] <- "R"
  monthly$tradingstatusflg[10] <- "H"
  monthly$usincflg[11] <- NA_character_
  monthly$permco[12] <- NA_real_
  volume <- c(100000, 80000, 500000, 500000, 400000, 200000, 150000, 200000,
              600000, 200000, 200000, 300000, 100000, 0, 200000, 250000)
  daily <- do.call(rbind, lapply(seq_len(nrow(monthly)), function(i) {
    data.frame(permno=monthly$permno[i], dlycaldt=sessions,
               dlyprc=10, dlyvol=volume[i])
  }))
  # Partial coverage, an absent security, and invalid versus missing inputs.
  daily <- daily[!(daily$permno == 990007 & daily$dlycaldt %in% sessions[1:6]) &
                 daily$permno != 990008, ]
  daily$dlyprc[daily$permno == 990015 & daily$dlycaldt %in% sessions[1:5]] <- NA_real_
  daily$dlyvol[daily$permno == 990016 & daily$dlycaldt == sessions[1]] <- -1
  rownames(daily) <- NULL
  list(monthly=monthly, daily=daily, sessions=sessions)
}
