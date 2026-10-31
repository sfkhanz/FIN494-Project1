# Run from a terminal: Rscript analysis/screen.R synthetic
# Live mode writes only to the external directory configured for WRDS data.

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1L]), mustWork = TRUE)
  setwd(dirname(dirname(script_path)))
}
source("config/project_config.R")
source("analysis/retrieval_helpers.R")
source("analysis/universe_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args[1L] %in% c("synthetic", "live")) {
  stop("Usage: Rscript analysis/screen.R synthetic|live")
}
mode <- args[1L]
check_config(project_config)
output_dir <- project_output_dir(mode)
input_file <- file.path(output_dir, "input.rds")
if (!file.exists(input_file)) stop("No input.rds. Run analysis/retrieve.R in the same mode first.")
bundle <- readRDS(input_file)
if (!is.list(bundle) || !all(c("monthly", "daily", "sessions", "metadata") %in% names(bundle))) {
  stop("input.rds is not a complete retrieval bundle.")
}
if (!identical(bundle$metadata$signature, extraction_signature(project_config))) {
  stop("Retrieval configuration changed. Re-run retrieve.R before screening this download.")
}
if (!identical(bundle$metadata$mode, mode)) stop("Input source mode does not match the requested analysis mode.")
validate_project_inputs(bundle$monthly, bundle$daily, bundle$sessions, project_config,
                        bundle$metadata$counts$monthly, bundle$metadata$counts$daily)

# Each stage is visible and separately inspectable; no security is pre-screened.
monthly <- validate_snapshot(bundle$monthly, project_config)
liquidity <- summarize_liquidity(bundle$daily, bundle$sessions, project_config)
joined <- join_liquidity(monthly, liquidity, project_config)
audit <- apply_screens(joined$audit, project_config)
funnel <- exclusion_funnel(audit, project_config)
comparison <- compare_universes(audit)
eda <- summarize_eda(audit)

outputs <- list(all_security_audit = audit, exclusion_funnel = funnel,
  baseline_universe = audit[audit$baseline_included, , drop = FALSE],
  final_universe = audit[audit$final_included, , drop = FALSE],
  comparison = comparison, eda = eda, join_audit = joined$join_audit,
  unmatched_daily = joined$unmatched_daily)
for (name in names(outputs)) {
  write.csv(outputs[[name]], file.path(output_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
}
plot_comparison(audit, project_config, file.path(output_dir, "comparison.png"))

source_files <- c("config/project_config.R", "analysis/retrieve.R", "analysis/retrieval_helpers.R",
                  "analysis/screen.R", "analysis/universe_helpers.R")
if (mode == "synthetic") source_files <- c(source_files, "analysis/fixture.R")
source_files <- source_files[file.exists(source_files)]
source_dir <- file.path(output_dir, "source")
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
for (path in source_files) {
  target <- file.path(source_dir, path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(path, target, overwrite = TRUE)) stop("Could not preserve source file: ", path)
}
metadata <- list(mode = mode, analyzed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  config = project_config, retrieval = bundle$metadata, sessions = bundle$sessions,
  input_md5 = unname(tools::md5sum(input_file)), source_md5 = tools::md5sum(source_files),
  liquidity_definition = "Mean of positive finite CIZ daily price times nonnegative finite daily share volume / 1e6 on shared market sessions; missing or invalid days excluded; zero volume retained.",
  observed_mean = "avg_dollar_volume_m is retained at any positive coverage; adv20_m is missing below min_valid_days.",
  exclusions = "Ordered first failures produce the funnel; all failures are retained in each security's audit row.",
  session_info = capture.output(sessionInfo()))
saveRDS(metadata, file.path(output_dir, "analysis_metadata.rds"))
log_lines <- c(paste("Mode:", mode), paste("Analyzed:", metadata$analyzed_at_utc),
  paste("Snapshot:", project_config$month_end),
  paste("Shared session range:", min(bundle$sessions), "through", max(bundle$sessions)),
  paste("Valid-day minimum:", project_config$min_valid_days),
  paste("Rows in source daily extraction:", nrow(bundle$daily)),
  paste("Rows outside the shared session window:",
        sum(!source_dates(bundle$daily$dlycaldt, "daily$dlycaldt") %in% bundle$sessions)),
  "", "Universe comparison (medians use observed denominators):", capture.output(print(comparison, row.names = FALSE)),
  "", "Join audit:", capture.output(print(joined$join_audit, row.names = FALSE)),
  "", metadata$liquidity_definition, metadata$observed_mean, "", metadata$session_info)
writeLines(log_lines, file.path(output_dir, "analysis_run_log.txt"))
if (mode == "synthetic") {
  cat(paste(log_lines[seq_len(7L)], collapse = "\n"), "\n\n")
  print(comparison, row.names = FALSE)
}
cat("\nAnalysis saved to:", normalizePath(output_dir), "\n")
