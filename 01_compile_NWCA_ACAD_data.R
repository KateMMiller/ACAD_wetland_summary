# Download EPA data
library(rvest)
library(stringr)
library(tidyverse)

# EPA NWCA data were downloaded in 01_compile_NWCA_data.R. This script extracts and compiles the ACAD
# sentinal site data from EPA's datasets.
#dir.create(paste0("./data/epa_acad/"))
#dir.create(paste0('./data/epa_acad/comb_data/'))
export_path <- "C:/NETN/R_Dev/ACAD_wetland_summary/data/epa_acad/"
#--- Compile plots in NCE, Visit 1, PROB only ---
# 2011 Data
siteinfo_2011 <- read.csv("./data/epa_all/nwca11_siteinfo.csv")
nce_sites_2011 <- siteinfo_2011$UID[siteinfo_2011$SITE_ID %in%
  c("NWCA11-R301", "NWCA11-R302", "NWCA11-R303", "NWCA11-R304", "NWCA11-R305",
    "NWCA11-R306", "NWCA11-R307", "NWCA11-R308", "NWCA11-R309", "NWCA11-R310")]

nce_sites_2011
csv_list <- list.files(paste0("./data/epa_all/"))
csv2011a <- csv_list[grepl("nwca11|nwca2011", csv_list)]
csv2011 <- csv2011a[!grepl("planttaxa", csv2011a)]
csv2011_name1 <- gsub("nwca11", "nwca2011", csv2011)
csv2011_name <- gsub("nwca2011_siteinfo", "nwca2011_site_info", csv2011_name1)

invisible(
lapply(seq_along(csv2011), function(x){
  cat(csv2011[x], " = ", csv2011_name[x], "\n")
  dat <- read.csv(paste0("./data/epa_all/", csv2011[x]), fileEncoding = "latin1") |>
    dplyr::filter(UID %in% nce_sites_2011)
  write.csv(dat, paste0(export_path, csv2011_name[x]), row.names = F)
})
)

# 2016 Data
siteinfo_2016 <- read.csv("./data/epa_all/nwca-2016-site-information-data_0.csv")
nce_sites_2016 <- siteinfo_2016$UID[siteinfo_2016$SITE_ID %in%
  c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
    "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310")]

nce_sites_2016
csv_list <- list.files(paste0("./data/epa_all/"))
csv2016a <- csv_list[grepl("nwca-2016|nwca_2016|nwca16_", csv_list)]
csv2016 <- csv2016a[!grepl("plant_|condition_estimates|veg_mmi", csv2016a)]
csv2016_name1 <- gsub("nwca16|nwca-2016|nwca_2016", "nwca2016", csv2016)
csv2016_name2 <- gsub("_-_", "_", csv2016_name1)
csv2016_name3 <- gsub("-", "_", csv2016_name2)
csv2016_name4 <- gsub("_csv.", ".", csv2016_name3)
csv2016_name <- gsub("nwca2016_site_information_data_0", "nwca2016_site_info", csv2016_name4)

invisible(
  lapply(seq_along(csv2016), function(x){
    cat(csv2016[x], " = ", csv2016_name[x], "\n")
    dat <- read.csv(paste0("./data/epa_all/", csv2016[x])) |> #, fileEncoding = "latin1") |>
      dplyr::filter(UID %in% nce_sites_2016)
    write.csv(dat, paste0(export_path, csv2016_name[x]), row.names = F)
  })
)

# 2021 Data
siteinfo_2021 <- read.csv("./data/epa_all/nwca21_siteinfo-data.csv")
nce_sites_2021 <- siteinfo_2021$UID[siteinfo_2021$SITE_ID %in%
  c("NWC21-ME-HP301", "NWC21-ME-HP302", "NWC21-ME-HP303", "NWC21-ME-HP304", "NWC21-ME-HP305",
    "NWC21-ME-HP306", 'NWC21-ME-HP307', "NWC21-ME-HP308", "NWC21-ME-HP309", "NWC21-ME-HP310")]

nce_sites_2021
csv_list <- list.files(paste0("./data/epa_all/"))
csv2021a <- csv_list[grepl("nwca-2021|nwca_2021|nwca21_", csv_list)]
csv2021 <- csv2021a[!grepl("plantcval|plantnative|planttaxa|plantwis|condition_estimates|landscape_metrics", csv2021a)]
csv2021_name1 <- gsub("nwca21|nwca-2021|nwca_2021", "nwca2021", csv2021)
csv2021_name2 <- gsub("_-_", "_", csv2021_name1)
csv2021_name3 <- gsub("-", "_", csv2021_name2)
csv2021_name <- gsub("nwca2021_siteinfo_data", "nwca2021_site_info", csv2021_name3)

invisible(
  lapply(seq_along(csv2021), function(x){
    cat(csv2021[x], " = ", csv2021_name[x], "\n")
    dat <- read.csv(paste0("./data/epa_all/", csv2021[x])) |> #, fileEncoding = "latin1") |>
      dplyr::filter(UID %in% nce_sites_2021)
    write.csv(dat, paste0(export_path, csv2021_name[x]), row.names = F)
  })
)
#--- DOWNLOAD NOTES ---
# The UNIQUE_ID column in siteinfo is what links the same sites across years.
#   - Need to compile plant taxa tables separately
#   - Dropped nwca_2016_veg_mmi.csv from initial compile, because doesn't include UID (only SITE_ID)
#   - Dropped ncwa21_landscape_metrics-data.csv because no UID column (only SiteID)

#--- Combining data across years ----
# For the following datasets, I'm row binding data across visits by intersecting the columns each dataset has
# in common to start with. There may be columns in later datasets that need adding, but I'll deal
# with that later.

#---- Site Info ----
site11 <- read.csv(paste0(export_path, "nwca2011_site_info.csv"))
site16 <- read.csv(paste0(export_path, "nwca2016_site_info.csv"))
site21 <- read.csv(paste0(export_path, "nwca2021_site_info.csv"))

comm_site_names <- intersect(intersect(names(site11), names(site16)), names(site21))

site_all <- rbind(site11[,comm_site_names], site16[,comm_site_names], site21[,comm_site_names]) |>
  arrange(UNIQUE_ID, DATE_COL, VISIT_NO)

site_all$Date <- as.POSIXct(site_all$DATE_COL, format = "%m/%d/%Y")
site_all$Year <- format(site_all$Date, format = "%Y")

site_freq <- data.frame(table(site_all$UNIQUE_ID))

table(site_all$RPT_UNIT) #30
table(site_all$VISIT_NO) #301
table(site_all$Year) # 10 per 2011, 2016, 2021

write.csv(site_all, paste0(export_path, "comb_data/Site_Information_2011-2021.csv"), row.names = F)

#---- Plant data ----
# 1. Compile # of plots sampled per site x visit (should be 5 for most)
# 2. Link C and Native status to species data by GEOD_ID/NWC_CREG code.
    # Do each visit separately?
    # Move plant taxa csvs from epa_all into a taxa specific folder for easier compiling
# 3. Compile Bryophyte cover using the vegetation type data

# Veg plot data
plot11 <- read.csv(paste0(export_path, 'nwca2011_vegplotloc.csv'))
plot16 <- read.csv(paste0(export_path, 'nwca2016_veg_plot_location_data.csv'))
plot21 <- read.csv(paste0(export_path, 'nwca2021_vegplotloc_wide_data.csv'))

# Dates are different formats across years
plot11$Date <- as.Date(plot11$DATE_COL, format = "%d-%b-%y")
plot11$Year <- format(plot11$Date, "%Y")
plot16$Date <- as.Date(plot16$DATE_COL, format = "%m/%d/%Y")
plot16$Year <- format(plot16$Date, "%Y")
plot21$Date <- as.Date(plot21$DATE_COL, format = "%d-%b-%y")
plot21$Year <- format(plot21$Date, "%Y")

comm_p_names <- intersect(intersect(names(plot11), names(plot16)), names(plot21))

plot_all <- rbind(plot11[,comm_p_names], plot16[,comm_p_names], plot21[,comm_p_names]) |>
  arrange(SITE_ID, Year, VISIT_NO)

plot_sum <- plot_all |> group_by(UID, SITE_ID, VISIT_NO, Date, Year) |>
  summarize(num_plots = sum(!is.na(PLOT)),
            miss_plots = sum(is.na(PLOT)),
            .groups = "drop")

table(plot_sum$Year) # 2011: 5; 2016: 10; 2021: 10 # Missing data in 2011
table(complete.cases(plot_all$PLOT)) #7 FALSE; 78 TRUE;
write.csv(plot_all, "./data/comb_data/Vegetation_Plot_Location_2011-2021.csv", row.names = F)

# Bryophyte cover
bryo11 <- read.csv(paste0(export_path, "nwca2011_vegtype_grndsurf.csv")) |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo16 <- read.csv(paste0(export_path, 'nwca2016_vegetation_type_data.csv')) |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo21 <- read.csv(paste0(export_path, 'nwca2021_vegtype_wide_data.csv')) |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)

bryo_all <- rbind(bryo11, bryo16, bryo21)

write.csv(bryo_all, paste0(export_path, "comb_data/Bryophyte_Cover_2011-2021.csv"), row.names = F)

# Plant Cover Data
# 2011 plant data
cov11a <- read.csv(paste0(export_path, "nwca2011_plant_pres_cvr.csv")) |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)
site11a <- read.csv(paste0(export_path, "nwca2011_site_info.csv")) |>
  select(UID, SITE_ID, UNIQUE_ID, COE_REG_ID, NWC_CREG11, NWC_CREG16)
cov11b <- left_join(cov11a, site11a, by = c("UID", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
head(cov11b)

# Bring in C, native, wetland indicator status
taxa11 <- read.csv("./data/taxa_lists/nwca2011_planttaxa.csv", fileEncoding = "latin1")
cnat11 <- read.csv('./data/taxa_lists/nwca2011_planttaxa_cc_natstat.csv', fileEncoding = "latin1")
wis11 <- read.csv('./data/taxa_lists/nwca2011_planttaxa_wis.csv', fileEncoding = 'latin1')

cov11t <- left_join(cov11b, taxa11 |> select(SPECIES_NAME_ID, USDA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION),
                    by = c("SPECIES_NAME_ID"))

cov11cnat <- left_join(cov11t, cnat11 |> select(-PUBLICATION_DATE),
                       by = c("SPECIES_NAME_ID", "USDA_NAME", "NWC_CREG11" = "GEOG_ID"))
cov11wis <- left_join(cov11cnat, wis11 |> select(-PUBLICATION_DATE),
                      by = c("SPECIES_NAME_ID", "USDA_NAME", "COE_REG_ID" = "GEOG_ID")) |>
  rename(NWCA_NAME = USDA_NAME)

head(cov11wis) # future years use different name.

# cov11wis ready to join with remaining years.

# 2016 plant data
cov16a <- read.csv(paste0(export_path, 'nwca2016_plant_species_cover_height_data.csv')) |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)

site16a <- read.csv(paste0(export_path, "nwca2016_site_info.csv")) |>
  select(UID, SITE_ID, UNIQUE_ID, COE_REG_ID, NWC_CREG11, NWC_CREG16)
cov16b <- left_join(cov16a, site16a, by = c("UID", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
head(cov16b)

# Bring in C, native, wetland indicator status
taxa16 <- read.csv("./data/taxa_lists/nwca_2016_plant_taxa.csv")
c16 <- read.csv("./data/taxa_lists/nwca_2016_plant_cvalues.csv")
nat16 <- read.csv('./data/taxa_lists/nwca_2016_plant_native_status.csv')
wis16 <- read.csv('./data/taxa_lists/nwca_2016_plant_wis.csv')

cov16t <- left_join(cov16b, taxa16 |> select(SPECIES_NAME_ID, NWCA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION),
                    by = c("SPECIES_NAME_ID"))
cov16nat <- left_join(cov16t, nat16 |> select(-PUBLICATION_DATE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG11" = "GEOG_ID"))
cov16c <- left_join(cov16nat, c16 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG16" = "GEOG_ID"))
cov16wis <- left_join(cov16c, wis16 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "COE_REG_ID" = "GEOG_ID"))

head(cov16wis) # 2016 data ready to join with other years (some columns )

# 2021 plant data
cov21a <- read.csv(paste0(export_path, 'nwca2021_plant_wide_data.csv'))
cov21a$YEAR <- format(as.Date(cov21a$DATE_COL, format = "%d-%b-%y"), "%Y")

site21a <- read.csv(paste0(export_path, "nwca2021_site_info.csv")) |>
  select(UID, SITE_ID, UNIQUE_ID, COE_REG_ID, PSTL_CODE, COE_REG_ID,
         NWC_CREG16)

cov21b <- left_join(cov21a, site21a, by = c("UID", "SITE_ID", "UNIQUE_ID", "PSTL_CODE"))

# Bring in C, native, wetland indicator status
taxa21 <- read.csv("./data/taxa_lists/nwca21_planttaxa-data.csv")
c21 <- read.csv("./data/taxa_lists/nwca21_plantcval-data.csv")
nat21 <- read.csv('./data/taxa_lists/nwca21_plantnative-data.csv')
wis21 <- read.csv('./data/taxa_lists/nwca21_plantwis-data.csv')

cov21t <- left_join(cov21b, taxa21 |> select(SPECIES_NAME_ID, NWCA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION),
                    by = c("SPECIES_NAME_ID"))
cov21nat <- left_join(cov21t, nat21 |> select(-PUBLICATION_DATE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "PSTL_CODE" = "GEOG_ID"))
cov21c <- left_join(cov21nat, c21 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG16" = "GEOG_ID"))
cov21wis <- left_join(cov21c, wis21 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "COE_REG_ID" = "GEOG_ID"))

head(cov21wis) # 2021 data ready to join with other years (some columns )

# column name updates before combining
cov11 <- cov11wis |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, PLOT, SPECIES_NAME_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CC, NATSTAT = NWCA_NATSTAT, WIS, ECOIND, ALIEN)

cov16 <- cov16wis |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, PLOT, SPECIES_NAME_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

cov21 <- cov21wis |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, PLOT, SPECIES_NAME_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

covcomb <- rbind(cov11, cov16, cov21)
head(covcomb)

write.csv(covcomb, paste0(export_path, "comb_data/Plant_Cover_2011-2021.csv", row.names = F))
