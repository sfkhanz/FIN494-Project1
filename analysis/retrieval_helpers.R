# Retrieval preserves every security in the source's assigned monthly snapshot.
# Size, security eligibility, and liquidity rules belong in the local analysis.
project_monthly_fields <- c("permno", "permco", "mthcaldt", "mthcap", "sharetype",
  "securitytype", "securitysubtype", "usincflg", "issuertype", "primaryexch",
  "conditionaltype", "tradingstatusflg")
project_daily_fields <- c("permno", "dlycaldt", "dlyprc", "dlyvol")

check_config <- function(config) {
  if (!is.list(config) || !identical(config$schema, "crsp") ||
      !identical(config$monthly_table, "msf_v2") ||
      !identical(config$daily_table, "dsf_v2"))
    stop("This scaffold supports the documented CIZ candidates crsp.msf_v2 and crsp.dsf_v2 only.")
  one_date <- function(x) inherits(x, "Date") && length(x) == 1L && !is.na(x)
  if (!one_date(config$month_end) || format(config$month_end + 1, "%d") != "01")
    stop("month_end must be one calendar month-end Date.")
  if (!one_date(config$lookback_start) || config$lookback_start > config$month_end)
    stop("lookback_start must be a Date on or before month_end.")
  positive_integer <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 1 && x == floor(x)
  if (!positive_integer(config$lookback_sessions) ||
      !positive_integer(config$min_valid_days) ||
      config$min_valid_days > config$lookback_sessions)
    stop("Use positive whole-number session counts, with min_valid_days <= lookback_sessions.")
  nonnegative <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 0
  if (!nonnegative(config$cap_min_m) || !nonnegative(config$adv_min_m) || config$adv_min_m == 0)
    stop("Size threshold must be nonnegative and dollar-volume threshold positive, in USD millions.")
  if (!identical(config$source_cap_unit, "USD thousands") ||
      !identical(config$price_unit, "USD/share") ||
      !identical(config$volume_unit, "shares"))
    stop("Unverified units: confirm mthcap in USD thousands, dlyprc in USD/share, and dlyvol in shares.")
  if (!is.logical(config$documentation_confirmed) ||
      length(config$documentation_confirmed) != 1L || is.na(config$documentation_confirmed))
    stop("documentation_confirmed must be TRUE or FALSE after instructor review.")
  invisible(TRUE)
}

extraction_signature <- function(config) {
  check_config(config)
  # Threshold changes are allowed without downloading again. Source/scope changes
  # require fresh preflight and retrieval. This structured signature is not a hash.
  keys <- c("schema", "monthly_table", "daily_table", "month_end", "lookback_start",
    "lookback_sessions", "source_cap_unit", "price_unit", "volume_unit")
  documentation_keys <- sort(setdiff(grep("doc|release|product", names(config),
    value = TRUE), "documentation_confirmed"))
  list(version = 1L, extraction = config[keys],
    monthly_fields = project_monthly_fields, daily_fields = project_daily_fields,
    documentation = config[documentation_keys])
}

project_output_dir <- function(mode, root = getwd()) {
  if (length(mode) != 1L || !mode %in% c("synthetic", "preflight", "live"))
    stop("Choose synthetic, preflight, or live mode.")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (mode == "synthetic") {
    path <- file.path(root, "output", "synthetic")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }
  path <- Sys.getenv("FIN494_PROJECT1_DATA_DIR", unset = "")
  if (!nzchar(path) || !dir.exists(path))
    stop("Set FIN494_PROJECT1_DATA_DIR to an existing private folder outside the project and Git repository.")
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  roots <- root
  gitroot <- suppressWarnings(system2("git", c("-C", shQuote(root),
    "rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE))
  if (is.null(attr(gitroot, "status")) && length(gitroot) == 1L)
    roots <- c(roots, normalizePath(gitroot, winslash = "/", mustWork = TRUE))
  if (any(vapply(roots, function(p) identical(path, p) ||
      startsWith(path, paste0(p, "/")), logical(1))))
    stop("Licensed data folder must be outside the project and its enclosing Git repository.")
  path
}

project_private_call <- function(action, expr) {
  # Never echo driver errors or warnings that can contain connection details.
  tryCatch(suppressWarnings(force(expr)), error = function(e)
    stop(paste("WRDS", action, "failed. Check access privately with the instructor."), call. = FALSE))
}

connect_project_wrds <- function() {
  needed <- c("DBI", "RPostgres")
  if (!all(vapply(needed, requireNamespace, logical(1), quietly = TRUE)))
    stop("Live access requires preconfigured DBI and RPostgres. Use synthetic mode until setup is complete.")
  # Reuse Class 05's external libpq service/PGUSER and password-file setup.
  # No passwords, credential-file reads, or credentials in project scripts.
  service <- Sys.getenv("PGSERVICE", unset = "")
  if (nzchar(service)) return(project_private_call("connection",
    DBI::dbConnect(RPostgres::Postgres(), service = service, sslmode = "require")))
  if (!nzchar(Sys.getenv("PGUSER", unset = "")))
    stop("Instructor-led external WRDS authentication setup is incomplete; use synthetic mode.")
  project_private_call("connection", DBI::dbConnect(RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu", port = 9737L,
    dbname = "wrds", sslmode = "require"))
}

project_monthly_query <- function(config, count_only = FALSE) {
  check_config(config)
  fields <- if (count_only) "COUNT(*) AS n" else paste(project_monthly_fields, collapse = ", ")
  paste0("SELECT ", fields, "\nFROM ", config$schema, ".", config$monthly_table,
    "\nWHERE mthcaldt = DATE '", config$month_end, "'",
    if (count_only) "" else "\nORDER BY permno")
}

project_calendar_query <- function(config, count_only = FALSE) {
  check_config(config)
  paste0("SELECT ", if (count_only) "COUNT(DISTINCT dlycaldt) AS n" else "DISTINCT dlycaldt",
    "\nFROM ", config$schema, ".", config$daily_table,
    "\nWHERE dlycaldt BETWEEN DATE '", config$lookback_start,
    "' AND DATE '", config$month_end, "'",
    if (count_only) "" else "\nORDER BY dlycaldt")
}

project_select_sessions <- function(calendar, config, expected_count = length(calendar)) {
  check_config(config)
  if (!inherits(calendar, "Date") || anyNA(calendar) || anyDuplicated(calendar) ||
      length(calendar) != expected_count ||
      any(calendar < config$lookback_start | calendar > config$month_end) ||
      length(calendar) < config$lookback_sessions)
    stop("Daily source calendar must contain enough distinct valid dates and match its independent date count.")
  sessions <- tail(sort(calendar), config$lookback_sessions)
  if (format(max(sessions), "%Y-%m") != format(config$month_end, "%Y-%m"))
    stop("The daily calendar does not reach the assigned snapshot month.")
  sessions
}

project_daily_query <- function(config, sessions, count_only = FALSE) {
  sessions <- project_select_sessions(sessions, config)
  fields <- if (count_only) "COUNT(*) AS n" else paste(project_daily_fields, collapse = ", ")
  paste0("SELECT ", fields, "\nFROM ", config$schema, ".", config$daily_table,
    "\nWHERE dlycaldt BETWEEN DATE '", min(sessions), "' AND DATE '", max(sessions), "'",
    "\n  AND permno IN (SELECT permno FROM ", config$schema, ".", config$monthly_table,
    "\n                 WHERE mthcaldt = DATE '", config$month_end, "')",
    if (count_only) "" else "\nORDER BY permno, dlycaldt")
}

project_queries <- function(config, sessions) {
  list(monthly_count = project_monthly_query(config, TRUE),
    monthly = project_monthly_query(config),
    calendar_count = project_calendar_query(config, TRUE),
    calendar = project_calendar_query(config),
    daily_count = project_daily_query(config, sessions, TRUE),
    daily = project_daily_query(config, sessions))
}

project_count <- function(result, allow_zero = FALSE) {
  n <- suppressWarnings(as.numeric(result$n))
  if (length(n) != 1L || !is.finite(n) || n != floor(n) ||
      n < if (allow_zero) 0 else 1)
    stop("Source count is missing, invalid, or outside the required coverage.")
  n
}

validate_project_inputs <- function(monthly, daily, sessions, config,
                                    monthly_count = nrow(monthly), daily_count = nrow(daily)) {
  check_config(config)
  if (!is.data.frame(monthly) || !is.data.frame(daily) ||
      !all(project_monthly_fields %in% names(monthly)) ||
      !all(project_daily_fields %in% names(daily)) ||
      anyDuplicated(names(monthly)) || anyDuplicated(names(daily)))
    stop("Required monthly or daily fields are missing or duplicated.")
  sessions_checked <- project_select_sessions(sessions, config)
  if (!identical(sessions, sessions_checked))
    stop("The common session vector must contain exactly the requested dates in chronological order.")
  if (length(monthly_count) != 1L || !is.finite(monthly_count) || monthly_count < 1 ||
      nrow(monthly) != monthly_count || length(daily_count) != 1L ||
      !is.finite(daily_count) || daily_count < 0 || nrow(daily) != daily_count)
    stop("Fetched rows do not match independent monthly/daily population counts, or the monthly snapshot is empty.")
  valid_id <- function(x) is.numeric(x) && !anyNA(x) &&
    all(is.finite(x) & x > 0 & x == floor(x))
  if (!valid_id(monthly$permno) || !valid_id(daily$permno))
    stop("Missing or invalid security identifiers in the retrieval.")
  if (!inherits(monthly$mthcaldt, "Date") || anyNA(monthly$mthcaldt) ||
      any(monthly$mthcaldt != config$month_end) ||
      !inherits(daily$dlycaldt, "Date") || anyNA(daily$dlycaldt) ||
      any(!daily$dlycaldt %in% sessions))
    stop("Missing or unexpected source observation dates.")
  if (anyDuplicated(monthly[, c("permno", "mthcaldt")]) ||
      anyDuplicated(daily[, c("permno", "dlycaldt")]))
    stop("Duplicate security/date keys; investigate before screening.")
  if (any(!daily$permno %in% monthly$permno))
    stop("Daily retrieval includes a security outside the full monthly snapshot.")
  if (!is.numeric(monthly$permco) || !is.numeric(monthly$mthcap) ||
      !is.numeric(daily$dlyprc) || !is.numeric(daily$dlyvol))
    stop("Company identifier, cap, price, and volume fields must have numeric storage types.")
  # Do not remove or stop on missing/negative/nonfinite economic values or unusual
  # classifications. Students inspect and explain those observations locally.
  # Securities without daily rows remain in monthly and receive explicit exclusions.
  invisible(TRUE)
}

project_write_query_metadata <- function(metadata, destination, filename = "query_metadata.txt") {
  query_lines <- unlist(lapply(names(metadata$queries), function(name)
    c("", paste0(name, ":"), metadata$queries[[name]])), use.names = FALSE)
  docs <- unlist(metadata$documentation, recursive = TRUE, use.names = TRUE)
  documentation_lines <- if (length(docs)) paste(names(docs), docs, sep = ": ") else
    "No release/documentation references recorded."
  writeLines(c(metadata$source, paste("Mode:", metadata$mode),
    paste("Snapshot:", metadata$month_end),
    paste("Common source sessions:", length(metadata$sessions)),
    paste("Window:", min(metadata$sessions), "through", max(metadata$sessions)),
    paste("Exact session dates:", paste(as.character(metadata$sessions), collapse = ", ")),
    paste("Cap unit:", metadata$units$cap), paste("Price unit:", metadata$units$price),
    paste("Volume unit:", metadata$units$volume),
    paste("Monthly rows independently counted:", metadata$counts$monthly),
    paste("Daily rows independently counted:", metadata$counts$daily),
    paste("Source calendar distinct dates:", metadata$counts$calendar),
    paste("Obtained UTC:", metadata$obtained_utc),
    "Source product, release, and documentation:", documentation_lines,
    "Units are documentation assumptions; SQL counts do not establish definitions or entitlements.",
    if (identical(metadata$mode, "preflight"))
      "PREFLIGHT ONLY: table/field and count/calendar checks; full monthly and daily input queries were not executed.",
    if (identical(metadata$mode, "synthetic"))
      "SYNTHETIC ONLY: queries are specifications, not executed WRDS queries; counts describe invented fixture.",
    "Monthly extraction has only a snapshot-date filter; all screens occur locally.", query_lines),
    file.path(destination, filename))
}
