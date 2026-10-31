# Run from the extracted student project: Rscript analysis/retrieve.R synthetic
# preflight/live use existing private WRDS authentication and external data storage.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 1L) {
  script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
  setwd(dirname(dirname(script_path)))
}
if (!file.exists("config/project_config.R") || !file.exists("analysis/retrieval_helpers.R"))
  stop("Run this script from the extracted Project 1 student starter.")
source("config/project_config.R")
source("analysis/retrieval_helpers.R")

retrieve_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L || !args %in% c("synthetic", "preflight", "live"))
    stop("Choose one mode: Rscript analysis/retrieve.R synthetic|preflight|live")
  mode <- args[1L]
  check_config(project_config)
  signature <- extraction_signature(project_config)
  destination <- project_output_dir(mode)
  receipt_file <- file.path(destination, "preflight_receipt.rds")
  receipt <- NULL
  if (mode == "live") {
    if (!isTRUE(project_config$documentation_confirmed) || !file.exists(receipt_file))
      stop("Live retrieval requires documented instructor confirmation and a matching private preflight receipt.")
    receipt <- readRDS(receipt_file)
    if (!identical(receipt$signature, signature) ||
        !identical(receipt$monthly_fields, project_monthly_fields) ||
        !identical(receipt$daily_fields, project_daily_fields))
      stop("Extraction settings changed since preflight; rerun preflight for this configuration.")
  }
  if (mode == "synthetic") {
    source("analysis/fixture.R")
    fixture <- synthetic_project_data(project_config)
    monthly <- fixture$monthly
    daily <- fixture$daily
    sessions <- fixture$sessions
    queries <- project_queries(project_config, sessions)
    counts <- list(monthly = nrow(monthly), daily = nrow(daily), calendar = length(sessions))
    source_label <- "SYNTHETIC: entirely invented teaching fixture; no WRDS download or live-count claim"
  } else {
    con <- connect_project_wrds()
    transaction_open <- FALSE
    on.exit({
      if (transaction_open) try(project_private_call("rollback", DBI::dbRollback(con)), silent = TRUE)
      try(project_private_call("disconnect", DBI::dbDisconnect(con)), silent = TRUE)
    }, add = TRUE)
    project_private_call("transaction start", DBI::dbBegin(con))
    transaction_open <- TRUE
    project_private_call("transaction isolation", DBI::dbExecute(con,
      "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"))
    monthly_fields <- project_private_call("monthly field check", DBI::dbListFields(con,
      DBI::Id(schema = project_config$schema, table = project_config$monthly_table)))
    daily_fields <- project_private_call("daily field check", DBI::dbListFields(con,
      DBI::Id(schema = project_config$schema, table = project_config$daily_table)))
    if (!all(project_monthly_fields %in% monthly_fields) ||
        !all(project_daily_fields %in% daily_fields))
      stop("Candidate CIZ source lacks required fields. Verify documentation and entitlement before proceeding.")
    get_query <- function(sql) project_private_call("query", DBI::dbGetQuery(con, sql))
    monthly_count <- project_count(get_query(project_monthly_query(project_config, TRUE)))
    calendar_count <- project_count(get_query(project_calendar_query(project_config, TRUE)))
    calendar <- get_query(project_calendar_query(project_config))$dlycaldt
    sessions <- project_select_sessions(calendar, project_config, calendar_count)
    queries <- project_queries(project_config, sessions)
    if (mode == "live" && !identical(receipt$queries, queries))
      stop("The source date window or queries changed since preflight; rerun preflight and review coverage.")
    daily_count <- project_count(get_query(queries$daily_count), allow_zero = TRUE)
    counts <- list(monthly = monthly_count, daily = daily_count, calendar = calendar_count)
    if (mode == "preflight") {
      receipt <- list(signature = signature, monthly_fields = project_monthly_fields,
        daily_fields = project_daily_fields, available_monthly_fields = monthly_fields,
        available_daily_fields = daily_fields, sessions = sessions, counts = counts,
        queries = queries, checked_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
        scope = "Read-only availability/count check; no full monthly/daily download and no units validation")
      project_private_call("transaction completion", DBI::dbCommit(con))
      transaction_open <- FALSE
      saveRDS(receipt, receipt_file)
      preflight_metadata <- list(mode = mode,
        source = "PREFLIGHT: source availability checks, not a complete retrieval",
        month_end = project_config$month_end, sessions = sessions, queries = queries,
        counts = counts, obtained_utc = receipt$checked_utc,
        units = list(cap = project_config$source_cap_unit, price = project_config$price_unit,
          volume = project_config$volume_unit), documentation = signature$documentation)
      project_write_query_metadata(preflight_metadata, destination, "preflight_metadata.txt")
      cat("PREFLIGHT: table/field access, snapshot count, and daily calendar checked. Receipt saved privately.\n")
      cat("This does not validate definitions, units, entitlement, or a complete securities download.\n")
      return(invisible(NULL))
    }
    monthly <- get_query(queries$monthly)
    daily <- get_query(queries$daily)
    validate_project_inputs(monthly, daily, sessions, project_config, monthly_count, daily_count)
    project_private_call("transaction completion", DBI::dbCommit(con))
    transaction_open <- FALSE
    source_label <- "LIVE WRDS: full assigned monthly source snapshot plus daily inputs; licensed/private"
  }
  validate_project_inputs(monthly, daily, sessions, project_config, counts$monthly, counts$daily)
  metadata <- list(signature = signature, mode = mode, source = source_label,
    month_end = project_config$month_end, sessions = sessions, queries = queries,
    fields = list(monthly = project_monthly_fields, daily = project_daily_fields),
    tables = list(monthly = paste(project_config$schema, project_config$monthly_table, sep = "."),
      daily = paste(project_config$schema, project_config$daily_table, sep = ".")),
    units = list(cap = project_config$source_cap_unit, price = project_config$price_unit,
      volume = project_config$volume_unit),
    counts = counts, fetched_rows = list(monthly = nrow(monthly), daily = nrow(daily)),
    documentation = signature$documentation,
    obtained_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  # Save the unchanged source frames, including missing classifications/measurements.
  saveRDS(list(monthly = monthly, daily = daily, sessions = sessions, metadata = metadata),
    file.path(destination, "input.rds"))
  project_write_query_metadata(metadata, destination)
  cat(source_label, "\n")
  cat("Saved input.rds and query_metadata.txt. No security, size, or liquidity screen was applied.\n")
  cat("PASS: source row counts, security/date keys, numeric field types, and common session window.\n")
}

tryCatch(retrieve_main(), error = function(e) {
  # Driver calls are wrapped above; this final guard also suppresses driver details.
  if (inherits(e, c("Rcpp::exception", "PqException"))) {
    message("WRDS connection/query failed. Check access privately with the instructor.")
  } else message(conditionMessage(e))
  quit(status = 1L)
})
