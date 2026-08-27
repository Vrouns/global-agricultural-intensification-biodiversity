# ============================================================================
# Extend national irrigated (blue) crop water footprint to 2020-2024
# ============================================================================
# Source data:
#   - data/01_raw/Mialyk_water_footprint/national_wf_all_crops_1990_2019.csv
#     Per crop-country-year: harvested area, production, yield, and unit
#     water footprints (green / blue-capillary-rise / blue-irrigation /
#     total), 1990-2019, 175 crops. From:
#     Mialyk, O. et al. (2024) "Water footprints and crop water use of 175
#     individual crops for 1990-2019 simulated with a global crop model."
#     Scientific Data 11:206. https://doi.org/10.1038/s41597-024-03051-3
#   - data/05_crop_types/CG/FAOSTAT/Production_Crops_Livestock_E_All_Data/
#     Production_Crops_Livestock_E_All_Data_NOFLAG.csv
#     FAOSTAT crop + livestock production, 1961-2024.
#
# Method (per-crop intensity extrapolation):
#   1. For 1990-2019, compute irrigation water use per crop-country-year as
#      wfb_i_m3_t (blue unit WF from irrigation, m3/t) x production_t
#      (already in the CSV, FAOSTAT-scaled)
#   2. For each crop-country, average wfb_i_m3_t over 2015-2019 as the
#      "recent" irrigation-water intensity.
#   3. Multiply that intensity by FAOSTAT's actual 2020-2024 production
#      (same crop_code = Item Code, country_code = Area Code) to
#      extrapolate irrigation water use for those years, crop by crop.
#   4. Sum across crops per country-year -> total national irrigation
#      water use, no crop breakdown in the output (per your request).
#   5. Append to 1990-2019 -> one continuous 1990-2024 series, plus a
#      global total.
#
# Why this is defensible:
#   - Tamea et al. (2021, Earth Syst. Sci. Data, the CWASI dataset)
#     extended Mekonnen & Hoekstra's single-year (2000) water footprint
#     estimates into a 1961-2016 series using this exact logic: scale to
#     historical production while holding water-use intensity fixed,
#     because unit water use responds to climate/cropland dynamics far
#     more slowly than production does.
#   - Mialyk et al. (2024) -- the source of this CSV -- make the identical
#     argument in their own "Post-processing" section to justify why they
#     rescale simulated *yields* to FAOSTAT but leave crop water use (and
#     hence unit WF) unscaled: it "is much less affected by agricultural
#     developments compared to yields."
#   - Doing this crop-by-crop (rather than on an all-crops-combined total)
#     avoids the composition-shift bias flagged in the earlier version of
#     this script: a shift toward more/less water-intensive crops in
#     2020-2024 is captured via each crop's own extrapolated production,
#     even though the final output is aggregated across crops.
#
# Remaining caveat (state wherever these numbers are used):
#   wfb_i_m3_t reflects a *potential* net irrigation requirement
#   (unconstrained by water availability), not actual withdrawal
#   (Hoekstra et al. 2011, Water Footprint Assessment Manual). Holding it
#   at its 2015-2019 average also assumes no major change in irrigation
#   technology/efficiency or water stress within a given crop-country
#   during 2020-2024.
# ============================================================================

library(data.table)

# ---- 1. Paths ------------------------------------------------------------
proj_root  <- "H:/02_Projekte/03_Intensification-fragmentation-CFs"
acea_file  <- file.path(proj_root,
  "data/01_raw/Mialyk_water_footprint/national_wf_all_crops_1990_2019.csv")
fao_file   <- file.path(proj_root,
  "data/05_crop_types/CG/FAOSTAT/Production_Crops_Livestock_E_All_Data",
  "Production_Crops_Livestock_E_All_Data_NOFLAG.csv")
out_dir    <- file.path(proj_root, "data/02_b_extention_datasets/")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

baseline_years <- 2015:2019   # window used to estimate "recent" intensity
target_years   <- 2020:2024   # years to extrapolate

# The 175 FAOSTAT item codes ACEA modelled (from readme.pdf, "List of
# crops"). Filtering FAOSTAT to exactly this set -- rather than inferring
# "which items are crops" -- guarantees the two datasets cover the same
# 175 crops.
acea_crop_codes <- c(15,27,44,56,71,75,79,83,89,92,94,97,101,103,108,116,122,
  125,135,136,137,149,156,157,161,176,181,187,191,195,197,201,203,205,210,
  211,216,217,220,221,222,223,224,225,226,234,236,242,249,254,260,263,265,
  267,270,275,277,280,289,292,296,299,305,310,328,333,336,339,358,366,367,
  372,373,388,393,394,397,399,401,402,403,406,407,414,417,420,423,426,430,
  446,449,459,461,463,486,489,490,495,497,507,512,515,521,523,526,530,531,
  534,536,541,542,544,547,549,550,552,554,558,560,567,568,569,571,572,574,
  577,587,591,592,600,603,619,636,637,638,639,640,641,642,643,644,645,646,
  647,648,649,651,655,656,661,667,671,677,687,689,692,693,698,702,711,720,
  723,748,754,773,777,780,782,788,789,800,809,821,826,836)
stopifnot(length(acea_crop_codes) == 175)

# ---- 2. ACEA national data, 1990-2019 -------------------------------------
acea <- fread(acea_file, showProgress = FALSE)

acea[, wf_irrig_blue_m3 := wfb_i_m3_t * production_t]

observed <- acea[, .(wf_irrig_blue_m3 = sum(wf_irrig_blue_m3, na.rm = TRUE),
                      production_t     = sum(production_t, na.rm = TRUE)),
                  by = .(country_code, country_name, year)]
observed[, source := "ACEA_simulated_1990_2019"]

# Recent (2015-2019) irrigation-water intensity per crop x country
intensity <- acea[year %in% baseline_years,
                   .(intensity_m3_per_t = mean(wfb_i_m3_t, na.rm = TRUE)),
                   by = .(crop_code, country_code, country_name)]

# ---- 3. FAOSTAT production for the same 175 crops, 2020-2024 -------------
fao <- fread(fao_file, showProgress = FALSE)
year_cols <- grep("^Y[0-9]{4}$", names(fao), value = TRUE)

fao_crops <- fao[Element == "Production" & Unit == "t" &
                    `Item Code` %in% acea_crop_codes,
                  c("Area Code", "Area", "Item Code","Item", ..year_cols), with = FALSE]

fao_long <- melt(fao_crops, id.vars = c("Area Code", "Area", "Item Code","Item"),
                  variable.name = "year", value.name = "production_t")
fao_long[, year := as.integer(sub("Y", "", year))]
fao_long <- fao_long[year %in% target_years & !is.na(production_t)]
setnames(fao_long, c("Area Code", "Area", "Item Code","Item"),
         c("country_code", "country_name", "crop_code", "crop_name"))
fao_long <- fao_long[country_code < 5000]

# ---- 4. Extrapolate 2020-2024, crop by crop, then sum ---------------------
extrap_crop <- merge(fao_long,
                      intensity[, .(crop_code, country_code, intensity_m3_per_t)],
                      by = c("crop_code", "country_code"), all.x = TRUE)

no_intensity <- extrap_crop[is.na(intensity_m3_per_t),
                             uniqueN(paste(crop_code, country_code))]

no_intensity_table <- extrap_crop[is.na(intensity_m3_per_t)]
unique(no_intensity_table$crop_name)
if (no_intensity > 0) {
  message("NOTE: ", no_intensity, " crop-country combinations in 2020-2024 ",
          "have no 2015-2019 ACEA baseline (e.g. new/rare crop-country ",
          "records) and are excluded from the extrapolated total.")
}

extrap_crop[, wf_irrig_blue_m3 := intensity_m3_per_t * production_t]

extrapolated <- extrap_crop[!is.na(wf_irrig_blue_m3),
  .(wf_irrig_blue_m3 = sum(wf_irrig_blue_m3, na.rm = TRUE),
    production_t     = sum(production_t, na.rm = TRUE)),
  by = .(country_code, country_name, year)]
extrapolated[, source := "extrapolated_2020_2024"]

# ---- 5. Combine into one continuous 1990-2024 series ----------------------
national_wf_1990_2024 <- rbindlist(
  list(observed, extrapolated), use.names = TRUE, fill = TRUE)
setorder(national_wf_1990_2024, country_code, year)

# ---- 6. QA checks (inspect before trusting the output) --------------------
message("\n--- QA: spot-check major producers, last observed vs first extrapolated year ---")
print(national_wf_1990_2024[country_name %in%
  c("China, mainland", "India", "United States of America", "Brazil", "Pakistan") &
  year %in% c(2018, 2019, 2020, 2024)])

# ---- 7. Save ---------------------------------------------------------------
fwrite(national_wf_1990_2024,
       file.path(out_dir, "national_irrigated_blue_wf_1990_2024.csv"))

global_total <- national_wf_1990_2024[,
  .(wf_irrig_blue_m3 = sum(wf_irrig_blue_m3, na.rm = TRUE)), by = .(year, source)]
fwrite(global_total, file.path(out_dir, "global_irrigated_blue_wf_1990_2024.csv"))

message("\nDone. Wrote:\n - ",
        file.path(out_dir, "national_irrigated_blue_wf_1990_2024.csv"), "\n - ",
        file.path(out_dir, "global_irrigated_blue_wf_1990_2024.csv"))
