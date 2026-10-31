# Project 1: inspect the broad download, calculate liquidity, and audit screens.
# These functions use base R. Prices are USD/share, volume is shares, and
# CRSP CIZ mthcap is USD thousands. Dollar-volume estimates are a proxy:
# daily closing price times daily share volume, not transaction-level turnover.

require_columns <- function(data, columns, label) {
  if (!is.data.frame(data)) stop(label, " must be a data frame.")
  if (anyDuplicated(names(data))) stop(label, " has duplicate column names.")
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(label, " is missing: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

require_numeric <- function(x, label) {
  if (!is.numeric(x)) stop(label, " must be numeric; do not silently coerce source data.")
  invisible(TRUE)
}

require_identifier <- function(x, label, allow_missing = FALSE) {
  require_numeric(x, label)
  invalid <- !is.na(x) & (!is.finite(x) | x <= 0 | x != floor(x))
  if (any(invalid) || (!allow_missing && anyNA(x))) {
    stop(label, " must contain positive integer identifiers",
         if (allow_missing) " or NA." else " without missing values.")
  }
  invisible(TRUE)
}

source_dates <- function(x, label) {
  if (inherits(x, "Date")) {
    result <- x
  } else if (is.character(x) && all(!is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))) {
    result <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (anyNA(result) || any(format(result, "%Y-%m-%d") != x)) {
      stop(label, " contains malformed dates.")
    }
  } else {
    stop(label, " must be Date values or unambiguous YYYY-MM-DD strings.")
  }
  if (anyNA(result) || any(!is.finite(as.numeric(result)))) stop(label, " contains missing or invalid dates.")
  result
}

classification_present <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

validate_snapshot <- function(monthly, config) {
  classifiers <- c("sharetype", "securitytype", "securitysubtype", "usincflg",
                   "issuertype", "primaryexch", "conditionaltype", "tradingstatusflg")
  require_columns(monthly, c("permno", "permco", "mthcaldt", "mthcap", classifiers), "monthly")
  if (!nrow(monthly)) stop("The monthly download is empty; check the source and date.")
  require_identifier(monthly$permno, "monthly$permno")
  require_identifier(monthly$permco, "monthly$permco", allow_missing = TRUE)
  require_numeric(monthly$mthcap, "monthly$mthcap")
  monthly$mthcaldt <- source_dates(monthly$mthcaldt, "monthly$mthcaldt")
  if (any(monthly$mthcaldt != config$month_end)) stop("Monthly rows do not all match the configured snapshot date.")
  if (anyDuplicated(monthly$permno)) stop("Monthly permno/month keys are duplicated.")
  for (name in classifiers) {
    if (!is.character(monthly[[name]]) && !is.factor(monthly[[name]])) {
      stop("monthly$", name, " must be a character classification field.")
    }
    monthly[[name]] <- as.character(monthly[[name]])
  }
  # Retain missing, negative, or infinite cap for the exclusion audit.
  monthly$cap_m <- monthly$mthcap / 1000
  monthly
}

validate_sessions <- function(sessions, config) {
  sessions <- source_dates(sessions, "sessions")
  if (length(sessions) != config$lookback_sessions || anyDuplicated(sessions) ||
      is.unsorted(sessions, strictly = TRUE)) {
    stop("sessions must contain exactly lookback_sessions unique, increasing common market dates.")
  }
  if (any(sessions < config$lookback_start | sessions > config$month_end)) {
    stop("Market sessions fall outside the configured retrieval dates.")
  }
  sessions
}

summarize_liquidity <- function(daily, sessions, config) {
  require_columns(daily, c("permno", "dlycaldt", "dlyprc", "dlyvol"), "daily")
  require_identifier(daily$permno, "daily$permno")
  require_numeric(daily$dlyprc, "daily$dlyprc")
  require_numeric(daily$dlyvol, "daily$dlyvol")
  daily$dlycaldt <- source_dates(daily$dlycaldt, "daily$dlycaldt")
  sessions <- validate_sessions(sessions, config)
  if (any(daily$dlycaldt < config$lookback_start | daily$dlycaldt > config$month_end)) {
    stop("Daily rows fall outside the configured retrieval dates.")
  }
  if (anyDuplicated(daily[c("permno", "dlycaldt")])) stop("Daily permno/date keys are duplicated.")
  # Every security uses the SAME sessions. Never take each security's last 20 rows.
  daily <- daily[daily$dlycaldt %in% sessions, , drop = FALSE]
  empty <- data.frame(permno = numeric(), daily_rows = integer(), valid_days = integer(),
                      invalid_days = integer(), zero_volume_days = integer(),
                      avg_dollar_volume_m = numeric(), adv20_m = numeric())
  if (!nrow(daily)) return(empty)
  dollar_volume_m <- daily$dlyprc * daily$dlyvol / 1e6
  valid <- is.finite(daily$dlyprc) & daily$dlyprc > 0 &
    is.finite(daily$dlyvol) & daily$dlyvol >= 0 & is.finite(dollar_volume_m)
  groups <- split(seq_len(nrow(daily)), daily$permno)
  result <- lapply(groups, function(rows) {
    good_rows <- rows[valid[rows]]
    observed_mean <- if (length(good_rows)) mean(dollar_volume_m[good_rows]) else NA_real_
    data.frame(permno = daily$permno[rows[1L]], daily_rows = length(rows),
               valid_days = length(good_rows), invalid_days = sum(!valid[rows]),
               zero_volume_days = sum(daily$dlyvol[good_rows] == 0),
               avg_dollar_volume_m = observed_mean,
               adv20_m = if (length(good_rows) >= config$min_valid_days) observed_mean else NA_real_)
  })
  result <- do.call(rbind, result)
  rownames(result) <- NULL
  if (anyDuplicated(result$permno)) stop("Liquidity summary is not unique by permno.")
  result
}

join_liquidity <- function(monthly, liquidity, config) {
  require_columns(liquidity, c("permno", "daily_rows", "valid_days", "invalid_days",
                               "zero_volume_days", "avg_dollar_volume_m", "adv20_m"), "liquidity")
  if (anyDuplicated(monthly$permno) || anyDuplicated(liquidity$permno)) {
    stop("A join input has duplicate permno keys; a one-to-one join is required.")
  }
  if (any(!liquidity$permno %in% monthly$permno)) {
    stop("Daily summaries contain permno keys outside the monthly snapshot cohort.")
  }
  collisions <- intersect(setdiff(names(liquidity), "permno"), names(monthly))
  if (length(collisions)) stop("Monthly and liquidity columns collide: ", paste(collisions, collapse = ", "))
  location <- match(monthly$permno, liquidity$permno)
  audit <- monthly
  audit$daily_summary_matched <- !is.na(location)
  for (name in setdiff(names(liquidity), "permno")) audit[[name]] <- liquidity[[name]][location]
  for (name in c("daily_rows", "valid_days", "invalid_days", "zero_volume_days")) {
    audit[[name]][is.na(location)] <- 0L
  }
  audit$missing_session_days <- config$lookback_sessions - audit$daily_rows
  audit$coverage_fraction <- audit$valid_days / config$lookback_sessions
  if (any(audit$missing_session_days < 0) || any(audit$valid_days > audit$daily_rows)) {
    stop("Daily summary exceeds the common window or its available rows.")
  }
  if (nrow(audit) != nrow(monthly) || !identical(audit$permno, monthly$permno)) {
    stop("The left join changed the broad monthly population or row order.")
  }
  unmatched <- audit[!audit$daily_summary_matched, , drop = FALSE]
  join_audit <- data.frame(
    metric = c("monthly_rows_before", "monthly_rows_after", "monthly_keys_duplicated",
               "liquidity_keys_duplicated", "monthly_with_daily_summary",
               "monthly_without_daily_summary", "monthly_with_zero_valid_days",
               "daily_summary_without_monthly", "monthly_order_preserved"),
    value = c(nrow(monthly), nrow(audit), 0, 0, sum(audit$daily_summary_matched),
              sum(!audit$daily_summary_matched), sum(audit$valid_days == 0), 0, 1))
  list(audit = audit, join_audit = join_audit, unmatched_daily = unmatched)
}

screen_failure_flags <- function(audit, config) {
  common_fields <- c("sharetype", "securitytype", "securitysubtype", "usincflg", "issuertype")
  common_known <- Reduce(`&`, lapply(audit[common_fields], classification_present))
  common_ok <- audit$sharetype %in% "NS" & audit$securitytype %in% "EQTY" &
    audit$securitysubtype %in% "COM" & audit$usincflg %in% "Y" & audit$issuertype %in% c("ACOR", "CORP")
  exchange_known <- classification_present(audit$primaryexch)
  trading_known <- classification_present(audit$conditionaltype) & classification_present(audit$tradingstatusflg)
  cap_missing <- is.na(audit$cap_m)
  cap_valid <- is.finite(audit$cap_m) & audit$cap_m >= 0
  coverage_ok <- audit$valid_days >= config$min_valid_days
  adv_valid <- is.finite(audit$adv20_m) & audit$adv20_m >= 0
  baseline <- list(
    missing_security_classification = !common_known,
    ineligible_security_type = common_known & !common_ok,
    missing_exchange_classification = !exchange_known,
    ineligible_exchange = exchange_known & !audit$primaryexch %in% c("N", "A", "Q"),
    missing_trading_classification = !trading_known,
    ineligible_trading_status = trading_known & !(audit$conditionaltype %in% "RW" & audit$tradingstatusflg %in% "A"),
    missing_market_cap = cap_missing,
    invalid_market_cap = !cap_missing & !cap_valid,
    below_cap_threshold = cap_valid & audit$cap_m < config$cap_min_m)
  final <- c(baseline, list(
    insufficient_daily_coverage = !coverage_ok,
    missing_liquidity_measure = coverage_ok & is.na(audit$adv20_m),
    invalid_liquidity_measure = coverage_ok & !is.na(audit$adv20_m) & !adv_valid,
    below_liquidity_threshold = coverage_ok & adv_valid & audit$adv20_m < config$adv_min_m))
  list(baseline = baseline, final = final)
}

failure_text <- function(flags, first_only = FALSE) {
  if (!length(flags)) return(character())
  vapply(seq_along(flags[[1L]]), function(i) {
    reasons <- names(flags)[vapply(flags, `[`, logical(1), i)]
    if (!length(reasons)) return(if (first_only) NA_character_ else "")
    if (first_only) reasons[1L] else paste(reasons, collapse = " | ")
  }, character(1))
}

apply_screens <- function(audit, config) {
  rules <- screen_failure_flags(audit, config)
  for (name in names(rules$final)) audit[[paste0("fails_", name)]] <- rules$final[[name]]
  for (screen in names(rules)) {
    audit[[paste0(screen, "_included")]] <- !Reduce(`|`, rules[[screen]])
    audit[[paste0(screen, "_all_failures")]] <- failure_text(rules[[screen]])
    audit[[paste0(screen, "_first_failure")]] <- failure_text(rules[[screen]], first_only = TRUE)
  }
  if (any(audit$final_included & !audit$baseline_included)) stop("The final universe is not a subset of the baseline.")
  audit
}

exclusion_funnel <- function(audit, config) {
  rules <- screen_failure_flags(audit, config)
  result <- lapply(names(rules), function(screen) {
    kept <- rep(TRUE, nrow(audit))
    stages <- list(data.frame(screen = screen, stage = "broad_download",
                              before_n = nrow(audit), excluded_n = 0L, remaining_n = nrow(audit)))
    for (name in names(rules[[screen]])) {
      before <- sum(kept)
      removed <- kept & rules[[screen]][[name]]
      kept <- kept & !rules[[screen]][[name]]
      stages[[length(stages) + 1L]] <- data.frame(screen = screen, stage = name,
          before_n = before, excluded_n = sum(removed), remaining_n = sum(kept))
    }
    do.call(rbind, stages)
  })
  do.call(rbind, result)
}

observed_median <- function(x) {
  observed <- x[is.finite(x) & x >= 0]
  if (length(observed)) median(observed) else NA_real_
}

compare_universes <- function(audit) {
  selections <- list(broad_download = rep(TRUE, nrow(audit)),
                     baseline_size = audit$baseline_included,
                     final_size_and_liquidity = audit$final_included)
  result <- lapply(names(selections), function(screen) {
    selected <- audit[selections[[screen]], , drop = FALSE]
    data.frame(screen = screen, security_count = nrow(selected),
      company_count_known = length(unique(selected$permco[!is.na(selected$permco)])),
      missing_company_count = sum(is.na(selected$permco)),
      cap_observed_n = sum(is.finite(selected$cap_m) & selected$cap_m >= 0),
      cap_median_m = observed_median(selected$cap_m),
      insufficient_daily_coverage_n = sum(selected$fails_insufficient_daily_coverage),
      liquidity_observed_n = sum(is.finite(selected$adv20_m) & selected$adv20_m >= 0),
      adv20_m_median = observed_median(selected$adv20_m),
      raw_liquidity_observed_n = sum(is.finite(selected$avg_dollar_volume_m) & selected$avg_dollar_volume_m >= 0),
      raw_avg_dollar_volume_m_median = observed_median(selected$avg_dollar_volume_m))
  })
  do.call(rbind, result)
}

summarize_eda <- function(audit) {
  total <- nrow(audit)
  result <- list()
  for (name in c("cap_m", "avg_dollar_volume_m", "adv20_m", "valid_days")) {
    values <- audit[[name]]
    valid <- is.finite(values) & values >= 0
    counts <- c(rows = total, missing = sum(is.na(values)),
                invalid = sum(!is.na(values) & !valid), observed = sum(valid))
    result[[length(result) + 1L]] <- data.frame(variable = name, measure = names(counts),
        category = NA_character_, value = unname(counts), denominator = total)
    quantiles <- if (any(valid)) quantile(values[valid], c(0, .25, .5, .75, 1), names = FALSE) else rep(NA_real_, 5)
    result[[length(result) + 1L]] <- data.frame(variable = name,
        measure = c("minimum", "p25", "median", "p75", "maximum"), category = NA_character_,
        value = quantiles, denominator = sum(valid))
  }
  for (name in c("sharetype", "securitytype", "securitysubtype", "usincflg", "issuertype",
                 "primaryexch", "conditionaltype", "tradingstatusflg")) {
    values <- audit[[name]]
    values[!classification_present(values)] <- "<missing>"
    counts <- table(values)
    result[[length(result) + 1L]] <- data.frame(variable = name, measure = "category_count",
        category = names(counts), value = as.numeric(counts), denominator = total)
  }
  do.call(rbind, result)
}

plot_comparison <- function(audit, config, path) {
  grDevices::png(path, width = 1500, height = 650, res = 130)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 1), oma = c(3, 0, 0, 0))
  counts <- c("Broad download" = nrow(audit), "Size baseline" = sum(audit$baseline_included),
              "Size + liquidity" = sum(audit$final_included))
  bars <- barplot(counts, col = c("#9BA3AA", "#DDA342", "#237A63"),
                   ylim = c(0, max(1, counts) * 1.15), ylab = "Number of securities", main = "Universe sizes", cex.names = .85)
  text(bars, counts, labels = counts, pos = 3)
  visible <- is.finite(audit$cap_m) & audit$cap_m > 0 &
    is.finite(audit$avg_dollar_volume_m) & audit$avg_dollar_volume_m > 0
  x <- audit$cap_m[visible]
  y <- audit$avg_dollar_volume_m[visible]
  x_anchor <- max(config$cap_min_m, 1)
  y_anchor <- max(config$adv_min_m, .01)
  colours <- ifelse(audit$final_included, "#237A63", ifelse(audit$baseline_included, "#DDA342", "#9BA3AA"))
  point_colours <- if (any(visible)) grDevices::adjustcolor(colours[visible], alpha.f = .7) else "#9BA3AA"
  plot(x, y, log = "xy", xlim = range(c(x, x_anchor / 10, x_anchor * 10)),
       ylim = range(c(y, y_anchor / 10, y_anchor * 10)), pch = 16,
       col = point_colours,
       xlab = "Security market cap (USD millions)",
       ylab = "Mean daily dollar-volume proxy (USD m)",
       main = "Size and observed trading activity")
  if (config$cap_min_m > 0) abline(v = config$cap_min_m, lty = 2)
  if (config$adv_min_m > 0) abline(h = config$adv_min_m, lty = 2)
  legend("topleft", c("Final universe", "Baseline only", "Outside baseline"),
         col = c("#237A63", "#DDA342", "#9BA3AA"), pch = 16, bty = "n", cex = .8)
  mtext(paste0("Common ", config$lookback_sessions, "-session window; final screen requires ",
               config$min_valid_days, " valid days. Log plot omits ", sum(!visible),
               " securities with zero, invalid, or missing values."), outer = TRUE, side = 1, line = 1, cex = .8)
  invisible(path)
}
