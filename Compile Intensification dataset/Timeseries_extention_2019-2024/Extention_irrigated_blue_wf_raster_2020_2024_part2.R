# ============================================================================
# Build a gridded (raster) irrigated blue water footprint for 2020-2024
# ============================================================================
# Purpose: produce a raster output (GeoTIFF + NetCDF) of the extrapolated
# total irrigated blue water footprint of crop production for 2020-2024,
# consistent with the national-level extrapolation already computed in
# extend_irrigated_blue_wf_2024.R.
#
# PREREQUISITE: run extend_irrigated_blue_wf_2024.R first -- this script
# reads its output (data/04_bia_inputs/national_irrigated_blue_wf_1990_2024.csv).
#
# Why a spatial downscaling step is needed at all:
#   The national extrapolation (per-crop uWF intensity x FAOSTAT production,
#   summed nationally) only produces one number per country-year -- it has
#   no spatial information of its own. To turn that into a raster, this
#   script redistributes each country's extrapolated national total across
#   its own grid cells using the SPATIAL PATTERN of the existing ACEA
#   raster (wf_prod_irrigated_blue_1990_2019.nc), averaged over 2015-2019,
#   as a fixed spatial template. Concretely, for each country c and target
#   year t:
#       growth_factor(c,t) = extrapolated_national_total(c,t) /
#                            mean_national_total(c, 2015-2019)
#       new_raster(cell,t) = base_pattern(cell) * growth_factor(country_of(cell),t)
#
# This is the standard "uniform within-unit scaling" approach used whenever
# a gridded product must be extended in time using only unit-level (here:
# national) driver data without a newer spatial survey -- e.g. how gridded
# emissions inventories (EDGAR) and gridded socioeconomic projections
# (e.g. SSP-based gridded population/GDP, Gao 2017; Murakami & Yamagata
# 2019) extend a base-year spatial pattern forward using national-level
# growth rates when no finer-resolution update exists. The tradeoff is the
# same here: within-country spatial redistribution during 2020-2024 (e.g.
# new irrigation schemes appearing in a previously rainfed area) is NOT
# captured -- only the national-level change in magnitude is. State this
# explicitly wherever the raster is used.
# ============================================================================

library(terra)
library(sf)
library(rnaturalearth)
library(data.table)

# ---- 1. Paths -------------------------------------------------------------
proj_root     <- "H:/02_Projekte/03_Intensification-fragmentation-CFs"
nc_file       <- file.path(proj_root, "data/01_raw/Mialyk_water_footprint/wf_prod_irrigated_blue_1990_2019.nc")
national_csv  <- file.path(proj_root, "data/02_b_extention_datasets/national_irrigated_blue_wf_1990_2024.csv")
lookup_file   <- file.path(proj_root,
  "data/01_raw/Mialyk_water_footprint/country_lookup_faostat_iso3.csv")
out_dir       <- file.path(proj_root, "data/02_b_extention_datasets/")
# if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

baseline_years <- 2015:2019   # must match extend_irrigated_blue_wf_2024.R
target_years   <- 2020:2024

if (!file.exists(national_csv)) {
  stop("Missing ", national_csv, " -- run extend_irrigated_blue_wf_2024.R first.")
}

# ---- 2. Spatial base pattern: mean 2015-2019 gridded irrigated blue WF ---
wf_rast <- rast(nc_file)
names(wf_rast) <- 1990:2019
crs(wf_rast) <- "EPSG:4326"
base_pattern <- mean(wf_rast[[as.character(baseline_years)]], na.rm = TRUE)

# ---- 3. Country-code raster on the same grid ------------------------------
# country_lookup_faostat_iso3.csv is derived from the dataset's own
# readme.pdf ("List of countries" table), matching each FAOSTAT country
# code used in national_wf_all_crops_1990_2019.csv to its ISO3 code, so
# the country polygons line up exactly with the codes used elsewhere in
# this pipeline (one manual fix applied: "China, mainland" is listed in
# the readme with a placeholder ISO3 -- corrected here to CHN).
lookup <- fread(lookup_file)

world <- ne_countries(scale = "medium", returnclass = "sf") |> st_make_valid()
world <- merge(world, lookup, by.x = "iso_a3", by.y = "iso3", all.x = FALSE)
if (nrow(world) < nrow(lookup) * 0.9) {
  message("NOTE: only ", nrow(world), " of ", nrow(lookup),
          " lookup countries matched a polygon in rnaturalearth -- check for ",
          "name/code mismatches before trusting small-country totals.")
}
world <- st_transform(world, crs(wf_rast))
country_id_rast <- rasterize(vect(world), base_pattern, field = "fao_country_code")

# ---- 4. Growth factors per country, relative to the 2015-2019 baseline ---
national <- fread(national_csv)

baseline_mean <- national[year %in% baseline_years,
                           .(baseline_wf = mean(wf_irrig_blue_m3, na.rm = TRUE)),
                           by = country_code]

growth <- merge(national[year %in% target_years,
                          .(country_code, year, wf_irrig_blue_m3)],
                 baseline_mean, by = "country_code")
growth[, growth_factor := wf_irrig_blue_m3 / baseline_wf]
growth <- growth[is.finite(growth_factor)]

# ---- 5. Redistribute each year's national total onto the grid ------------
out_layers <- vector("list", length(target_years))
names(out_layers) <- as.character(target_years)

for (yr in target_years) {
  gf <- growth[year == yr, .(country_code, growth_factor)]
  rcl <- as.matrix(gf)
  gf_rast <- classify(country_id_rast, rcl, others = NA)
  layer <- base_pattern * gf_rast
  names(layer) <- as.character(yr)
  out_layers[[as.character(yr)]] <- layer
}
out_stack <- rast(out_layers)
units(out_stack) <- "m3 yr-1"
plot(out_stack)
# ---- 6. QA: compare raster-derived national totals vs the extrapolated table
message("\n--- QA: raster-derived vs. table-extrapolated national totals (m3/yr) ---")
zonal_check <- zonal(out_stack, country_id_rast, fun = "sum", na.rm = TRUE)
setnames(zonal_check, "fao_country_code", "country_code")
zonal_check <- as.data.table(zonal_check)
zonal_long <- melt(zonal_check, id.vars = "country_code",
                    variable.name = "year", value.name = "raster_wf_m3")
zonal_long[, year := as.integer(as.character(year))]
qa <- merge(zonal_long, national[, .(country_code, year, wf_irrig_blue_m3)],
            by = c("country_code", "year"))
qa[, pct_diff := 100 * (raster_wf_m3 - wf_irrig_blue_m3) / wf_irrig_blue_m3]
print(qa[country_code %in% c(2, 41, 100, 231, 21, 165)])  # Afghanistan, China, India, US, Brazil, Pakistan
message("Differences reflect rasterization edge effects (fractional coverage of ",
        "border cells is not weighted in this QA step) -- large systematic ",
        "differences would indicate a country-matching problem, not just noise.")

# ---- 7. Save ---------------------------------------------------------------
writeRaster(out_stack, file.path(out_dir, "wf_blue_irrigated_2020_2024.tif"),
            overwrite = TRUE)

message("\nDone. Wrote:\n - ",
        file.path(out_dir, "wf_blue_irrigated_2020_2024.tif"))
