
#...........................................................
# Clean inputs ----
#...........................................................

#...........................................................
# Task 4a: Euromonitor packaged-food consumption trend ----
#...........................................................
# Ingest the "Predictions (2025-2030)" APC (annual % change) forecasts and
# collapse to ONE growth rate per country. We use the MEDIAN APC over
# hand-picked, NON-OVERLAPPING, sodium-relevant LEAF categories. The workbook's
# `code` column encodes a parent/child hierarchy, e.g.
#   "Meals and Soups" (3.2) > "Ready Meals" (3.2.1), "Soup" (3.2.2)
#   "Dairy"           (4.2) > "Cheese"      (4.2.1b)
# so averaging every row would double-count. The two PARENTS (3.2, 4.2) are
# therefore EXCLUDED; their sodium-relevant children are kept. Labels below were
# confirmed verbatim against the workbook (2026-07-22).
#
# NB: geography -> model-location mapping (only "Vietnam" -> "Viet Nam" among
# these 10 countries) is NOT done here: name_map is defined in
# 05_build_baseline.R, which is sourced AFTER this file. The mapping AND the
# application to source shares happen in 07_run_interventions.R (Task 4b), which
# reads the per-geography table written below.
if (file.exists(paste0(wd_raw, "Euromonitor_trends_RTSL.xlsx"))) {

  # Non-overlapping, sodium-relevant LEAF categories (exact labels from file).
  # Excluded parents (avoid double-counting): "Meals and Soups" (3.2), "Dairy" (4.2).
  sodium_leaf_categories <- c(
    "Sauces, Dips and Condiments",                      # 3.3
    "Soup",                                             # 3.2.2  (leaf of Meals and Soups)
    "Ready Meals",                                      # 3.2.1  (leaf of Meals and Soups)
    "Processed Meat, Seafood and Alternatives to Meat", # 5.4
    "Savoury Snacks",                                   # 6.3
    "Cheese",                                           # 4.2.1b (leaf of Dairy)
    "Baked Goods",                                      # 5.1
    "Rice, Pasta and Noodles"                           # 5.5
  )

  eu_pkg <- as.data.table(read_excel(
    paste0(wd_raw, "Euromonitor_trends_RTSL.xlsx"),
    sheet = "Predictions (2025-2030)"
  ))
  eu_pkg <- eu_pkg[series_category %in% sodium_leaf_categories]

  # One growth rate per geography = MEDIAN APC (robust to category heterogeneity)
  # over the leaf categories, expressed as a fraction per year.
  packaged_food_trends <- eu_pkg[
    , .(g = median(as.numeric(apc), na.rm = TRUE) / 100), by = geography
  ]

  saveRDS(packaged_food_trends,
          file = paste0(wd_data, "packaged_food_trends.rds"))
  cat(sprintf(
    "Task 4a: packaged_food_trends written for %d geographies (median APC over %d leaf categories).\n",
    nrow(packaged_food_trends), length(sodium_leaf_categories)))
  rm(eu_pkg)
} else {
  cat("Task 4a: Euromonitor_trends_RTSL.xlsx not found in wd_raw; packaged trend input NOT built.\n")
}
