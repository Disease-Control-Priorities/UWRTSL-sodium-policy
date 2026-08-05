
#...........................................................
# Clean inputs ----
#...........................................................

#...........................................................
# Task 3a: Euromonitor packaged-food trend (v002 workbook) ----
#...........................................................
# Matti's v002 workbook (Euromonitor_trends_RTSL_v002.xlsx) REPLACES the old
# leaf-category derivation. Each geography now carries a PRE-COMBINED row
#   series_category == "Combined selected sodium-sources categories"  (code 0)
# that already combines the sodium-relevant categories into ONE trend per
# geography, WITH confidence intervals. We consume that row directly instead of
# taking a median over hand-picked leaf categories.
#
# Output (data/processed/packaged_food_trends.rds): one row per model location
#   location : canonical model name (Vietnam -> "Viet Nam" via name_map)
#   g        : packaged-share growth rate, FRACTION per year (apc / 100)
#   g_lcl    : lower 95% bound (fraction/yr)
#   g_ucl    : upper 95% bound (fraction/yr)
#   window   : which sheet the trend came from
#
# Controls (resolved via get0 so 03 is robust when sourced by a bespoke driver
# that did not set them; canonical values live in 07 SECTION 0 / run_config):
#   EUROMONITOR_WINDOW           : "predictions" (2025-2030, default) |
#                                  "observed" (2011-2025) | "post_covid" (2023-2025)
#   EUROMONITOR_RECOMBINE_FROM_LEAVES : FALSE (default) uses Matti's combined row;
#                                  TRUE reproduces the OLD median-over-leaves method
#                                  (kept only as a fallback / cross-check).
# NB: v002 has NO sales-volume column, so volume weighting is NOT implemented
# here (see the clearly-labelled stub below). name_map comes from 01_utils.R
# (sourced before 03).

.euro_window   <- get0("EUROMONITOR_WINDOW",                ifnotfound = "predictions")
.euro_recomb   <- isTRUE(get0("EUROMONITOR_RECOMBINE_FROM_LEAVES", ifnotfound = FALSE))
.euro_file_v2  <- paste0(wd_raw, "Euromonitor_trends_RTSL_v002.xlsx")
.euro_file_v1  <- paste0(wd_raw, "Euromonitor_trends_RTSL.xlsx")

# window -> (sheet, apc-column stem). "observed" uses `aapc`; the others `apc`.
.euro_sheet <- switch(.euro_window,
  "predictions" = "Predictions (2025-2030)",
  "observed"    = "All observed (2011-2025)",
  "post_covid"  = "Post-COVID (2023-2025)",
  stop("EUROMONITOR_WINDOW must be 'predictions', 'observed', or 'post_covid'."))
.euro_stem  <- if (.euro_window == "observed") "aapc" else "apc"

if (file.exists(.euro_file_v2)) {

  if (!.euro_recomb) {
    # ---- PRIMARY path: consume the pre-combined per-geography row ------------
    eu <- as.data.table(read_excel(.euro_file_v2, sheet = .euro_sheet))
    comb <- eu[series_category == "Combined selected sodium-sources categories"]
    if (nrow(comb) == 0L) {
      stop("Task 3a: no 'Combined selected sodium-sources categories' row in sheet '",
           .euro_sheet, "' of ", basename(.euro_file_v2))
    }
    col_c <- .euro_stem
    col_l <- paste0(.euro_stem, "_lcl")
    col_u <- paste0(.euro_stem, "_ucl")
    packaged_food_trends <- comb[, .(
      geography,
      g     = as.numeric(get(col_c)) / 100,
      g_lcl = as.numeric(get(col_l)) / 100,
      g_ucl = as.numeric(get(col_u)) / 100
    )]
    packaged_food_trends[, window := .euro_window]
    # geography -> model location (only "Vietnam" -> "Viet Nam" among these 10).
    packaged_food_trends[, location := fcoalesce(name_map[geography], geography)]
    packaged_food_trends[, geography := NULL]
    setcolorder(packaged_food_trends, c("location", "g", "g_lcl", "g_ucl", "window"))

    saveRDS(packaged_food_trends, file = paste0(wd_data, "packaged_food_trends.rds"))
    cat(sprintf(
      "Task 3a: packaged_food_trends written from v002 COMBINED row for %d geographies (window = %s).\n",
      nrow(packaged_food_trends), .euro_window))

  } else {
    # ---- FALLBACK: OLD median-over-leaves method (cross-check only) ----------
    # Non-overlapping, sodium-relevant LEAF categories (exact labels). Parents
    # "Meals and Soups" and "Dairy" are EXCLUDED to avoid double-counting their
    # children. Retained so we can reproduce the pre-v002 number if ever needed.
    sodium_leaf_categories <- c(
      "Sauces, Dips and Condiments", "Soup", "Ready Meals",
      "Processed Meat, Seafood and Alternatives to Meat", "Savoury Snacks",
      "Cheese", "Baked Goods", "Rice, Pasta and Noodles")
    eu <- as.data.table(read_excel(.euro_file_v2, sheet = .euro_sheet))
    eu <- eu[series_category %in% sodium_leaf_categories]
    packaged_food_trends <- eu[, .(
      g     = median(as.numeric(get(.euro_stem)), na.rm = TRUE) / 100,
      g_lcl = NA_real_, g_ucl = NA_real_
    ), by = geography]
    packaged_food_trends[, window := paste0(.euro_window, "_recombine_leaves")]
    packaged_food_trends[, location := fcoalesce(name_map[geography], geography)]
    packaged_food_trends[, geography := NULL]
    setcolorder(packaged_food_trends, c("location", "g", "g_lcl", "g_ucl", "window"))
    saveRDS(packaged_food_trends, file = paste0(wd_data, "packaged_food_trends.rds"))
    cat(sprintf(
      "Task 3a: packaged_food_trends REBUILT via legacy median-over-leaves (%d leaves, window = %s).\n",
      length(sodium_leaf_categories), .euro_window))
  }

  # ---- STUB (do NOT implement now): volume-weighted recombine ---------------
  # v002 has no sales-volume column. If a future workbook adds per-category sales
  # volumes, a "volume_weighted" recombine could weight each leaf category's APC
  # by its sodium-relevant sales volume before combining. Until then we rely on
  # Matti's pre-combined row above.

  rm(eu)

} else if (file.exists(.euro_file_v1)) {
  stop("Task 3a: only the OLD Euromonitor_trends_RTSL.xlsx is present; v002 ",
       "(Euromonitor_trends_RTSL_v002.xlsx) is required for the combined-row trend.")
} else {
  cat("Task 3a: no Euromonitor workbook found in wd_raw; packaged trend input NOT built.\n")
}

rm(.euro_window, .euro_recomb, .euro_file_v1, .euro_file_v2, .euro_sheet, .euro_stem)
