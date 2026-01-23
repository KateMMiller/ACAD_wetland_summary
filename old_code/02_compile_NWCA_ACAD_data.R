#------------------------------------------------------------
# Compile ACAD Sentinel data collected as part of EPA NWCA
#------------------------------------------------------------

# EPA NWCA data were downloaded in 01_download_NWCA_NCE_data.R. This script extracts and compiles the ACAD
# sentinal site data from EPA's datasets.

#dir.create(paste0("./data/epa_acad/"))
#dir.create(paste0('./data/epa_acad/comb_data/'))

library(tidyverse)
library(readxl)
export_path <- "C:/NETN/R_Dev/ACAD_wetland_summary/data/epa_acad/"

#--- Compile ACAD sentinal plots (R301-R310) ---
# 2011 Data
siteinfo_2011 <- read.csv("./data/epa_all/nwca11_siteinfo.csv")
acad_sites_2011 <- siteinfo_2011$UID[siteinfo_2011$SITE_ID %in%
  c("NWCA11-R301", "NWCA11-R302", "NWCA11-R303", "NWCA11-R304", "NWCA11-R305",
    "NWCA11-R306", "NWCA11-R307", "NWCA11-R308", "NWCA11-R309", "NWCA11-R310")]

acad_sites_2011
csv_list <- list.files(paste0("./data/epa_all/"))
csv2011a <- csv_list[grepl("nwca11|nwca2011", csv_list)]
csv2011 <- csv2011a[!grepl("planttaxa", csv2011a)]
csv2011_name1 <- gsub("nwca11", "nwca2011", csv2011)
csv2011_name <- gsub("nwca2011_siteinfo", "nwca2011_site_info", csv2011_name1)

invisible(
lapply(seq_along(csv2011), function(x){
  cat(csv2011[x], " = ", csv2011_name[x], "\n")
  dat <- read.csv(paste0("./data/epa_all/", csv2011[x]), fileEncoding = "latin1") |>
    dplyr::filter(UID %in% acad_sites_2011) |>
    mutate(local_code = case_when(grepl("301", SITE_ID) ~ "DUCK",
                                  grepl("302", SITE_ID) ~ "WMTN",
                                  grepl("303", SITE_ID) ~ "BIGH",
                                  grepl("304", SITE_ID) ~ "GILM",
                                  grepl("305", SITE_ID) ~ "LITH",
                                  grepl("306", SITE_ID) ~ "NEMI",
                                  grepl("307", SITE_ID) ~ "GRME",
                                  grepl("308", SITE_ID) ~ "HEBR",
                                  grepl("309", SITE_ID) ~ "HODG",
                                  grepl("310", SITE_ID) ~ "FRAZ"))
  write.csv(dat, paste0(export_path, csv2011_name[x]), row.names = F)
})
)

# 2016 Data
siteinfo_2016 <- read.csv("./data/epa_all/nwca-2016-site-information-data_0.csv")
acad_sites_2016 <- siteinfo_2016$UID[siteinfo_2016$SITE_ID %in%
  c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
    "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310")]

acad_sites_2016
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
      dplyr::filter(UID %in% acad_sites_2016) |>
      mutate(local_code = case_when(grepl("301", SITE_ID) ~ "DUCK",
                                    grepl("302", SITE_ID) ~ "WMTN",
                                    grepl("303", SITE_ID) ~ "BIGH",
                                    grepl("304", SITE_ID) ~ "GILM",
                                    grepl("305", SITE_ID) ~ "LITH",
                                    grepl("306", SITE_ID) ~ "NEMI",
                                    grepl("307", SITE_ID) ~ "GRME",
                                    grepl("308", SITE_ID) ~ "HEBR",
                                    grepl("309", SITE_ID) ~ "HODG",
                                    grepl("310", SITE_ID) ~ "FRAZ"))
    write.csv(dat, paste0(export_path, csv2016_name[x]), row.names = F)
  })
)

# 2021 Data
siteinfo_2021 <- read.csv("./data/epa_all/nwca21_siteinfo-data.csv")
acad_sites_2021 <- siteinfo_2021$UID[siteinfo_2021$SITE_ID %in%
  c("NWC21-ME-HP301", "NWC21-ME-HP302", "NWC21-ME-HP303", "NWC21-ME-HP304", "NWC21-ME-HP305",
    "NWC21-ME-HP306", 'NWC21-ME-HP307', "NWC21-ME-HP308", "NWC21-ME-HP309", "NWC21-ME-HP310")]

acad_sites_2021
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
      dplyr::filter(UID %in% acad_sites_2021) |>
      mutate(local_code = case_when(grepl("301", SITE_ID) ~ "DUCK",
                                    grepl("302", SITE_ID) ~ "WMTN",
                                    grepl("303", SITE_ID) ~ "BIGH",
                                    grepl("304", SITE_ID) ~ "GILM",
                                    grepl("305", SITE_ID) ~ "LITH",
                                    grepl("306", SITE_ID) ~ "NEMI",
                                    grepl("307", SITE_ID) ~ "GRME",
                                    grepl("308", SITE_ID) ~ "HEBR",
                                    grepl("309", SITE_ID) ~ "HODG",
                                    grepl("310", SITE_ID) ~ "FRAZ"))
    write.csv(dat, paste0(export_path, csv2021_name[x]), row.names = F)
  })
)

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
table(site_all$local_code)
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

plot_sum <- plot_all |> group_by(UID, local_code, SITE_ID, VISIT_NO, Date, Year) |>
  summarize(num_plots = sum(!is.na(PLOT)),
            miss_plots = sum(is.na(PLOT)),
            .groups = "drop")

table(plot_sum$Year) # 2011: 5; 2016: 10; 2021: 10 # Missing data in 2011
table(complete.cases(plot_all$PLOT)) #7 FALSE; 78 TRUE;
write.csv(plot_all, paste0(export_path, "comb_data/Vegetation_Plot_Location_2011-2021.csv"), row.names = F)

# Bryophyte cover
bryo11 <- read.csv(paste0(export_path, "nwca2011_vegtype_grndsurf.csv")) |>
  select(UID, local_code, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo11$YEAR <- 2011

bryo16 <- read.csv(paste0(export_path, 'nwca2016_vegetation_type_data.csv')) |>
  select(UID, local_code, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES, DATE_COL)
bryo16$YEAR <- format(as.Date(bryo16$DATE_COL, format = "%m/%d/%Y"), "%Y")
table(bryo16$YEAR, useNA = 'always')

bryo21 <- read.csv(paste0(export_path, 'nwca2021_vegtype_wide_data.csv')) |>
  select(UID, local_code, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES, DATE_COL)

head(bryo21)
bryo21$YEAR <- format(as.Date(bryo21$DATE_COL, "%d-%b-%y"), "%Y")
table(bryo21$YEAR, useNA = 'always')

bryo_all <- rbind(bryo11, bryo16 |> select(-DATE_COL), bryo21 |> select(-DATE_COL))

write.csv(bryo_all, paste0(export_path, "comb_data/Bryophyte_Cover_2011-2021.csv"), row.names = F)

# Plant Cover Data
# 2011 plant data
cov11a <- read.csv(paste0(export_path, "nwca2011_plant_pres_cvr.csv")) |>
  select(UID, local_code, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)
site11a <- read.csv(paste0(export_path, "nwca2011_site_info.csv")) |>
  select(UID, local_code, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, COE_REG_ID, NWC_CREG11, NWC_CREG16)
cov11b <- left_join(cov11a, site11a, by = c("UID", "local_code", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
head(cov11b)

# Bring in C, native, wetland indicator status
taxa11a <- read.csv("./data/taxa_lists/nwca2011_planttaxa.csv", fileEncoding = "latin1")
# Aronia melanocarpa is treated as Photinia melanocarpa in 2011. Hack below adds it
# so the species shows up in later joins
aromel <- taxa11a[taxa11a$USDA_NAME == "PHOTINIA MELANOCARPA",]
aromel$SPECIES_NAME_ID <- 91516
aromel$USDA_NAME <- "ARONIA MELANOCARPA"
aromel$GENUS <- "ARONIA"
taxa11 <- rbind(taxa11a, aromel)

cnat11a <- read.csv('./data/taxa_lists/nwca2011_planttaxa_cc_natstat.csv', fileEncoding = "latin1")
aromel_cnat <- cnat11a[cnat11a$USDA_NAME == "PHOTINIA MELANOCARPA" & cnat11a$GEOG_ID == "ME",]
aromel_cnat$USDA_NAME <- "ARONIA MELANOCARPA"
aromel_cnat$SPECIES_NAME_ID <- 91516
cnat11 <- rbind(cnat11a, aromel_cnat)

wis11a <- read.csv('./data/taxa_lists/nwca2011_planttaxa_wis.csv', fileEncoding = 'latin1')
aromel_wis <- wis11a[wis11a$USDA_NAME == "PHOTINIA MELANOCARPA" & wis11a$GEOG_ID == "NE",]
aromel_wis$USDA_NAME <- "ARONIA MELANOCARPA"
aromel_wis$SPECIES_NAME_ID <- 91516
wis11 <- rbind(wis11a, aromel_wis)

cov11t <- left_join(cov11b, taxa11 |> select(SPECIES_NAME_ID, USDA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION),
                    by = c("SPECIES_NAME_ID"))

cov11cnat <- left_join(cov11t, cnat11 |> select(-PUBLICATION_DATE),
                       by = c("SPECIES_NAME_ID", "USDA_NAME", "NWC_CREG11" = "GEOG_ID"))
cov11wis <- left_join(cov11cnat, wis11 |> select(-PUBLICATION_DATE),
                      by = c("SPECIES_NAME_ID", "USDA_NAME", "COE_REG_ID" = "GEOG_ID")) |>
  rename(NWCA_NAME = USDA_NAME) |>
  mutate(SYMBOL = NA_character_)

head(cov11wis) # future years use different name.

# cov11wis ready to join with remaining years.

# 2016 plant data
cov16a <- read.csv(paste0(export_path, 'nwca2016_plant_species_cover_height_data.csv')) |>
  select(UID, local_code, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)

site16a <- read.csv(paste0(export_path, "nwca2016_site_info.csv")) |>
  select(UID, local_code, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, COE_REG_ID, NWC_CREG11, NWC_CREG16)

cov16b <- left_join(cov16a, site16a, by = c("UID", "local_code", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
head(cov16b)

# Bring in C, native, wetland indicator status
taxa16 <- read.csv("./data/taxa_lists/nwca_2016_plant_taxa.csv")
c16 <- read.csv("./data/taxa_lists/nwca_2016_plant_cvalues.csv")
nat16 <- read.csv('./data/taxa_lists/nwca_2016_plant_native_status.csv')
wis16 <- read.csv('./data/taxa_lists/nwca_2016_plant_wis.csv')

cov16t <- left_join(cov16b, taxa16 |> select(SPECIES_NAME_ID, NWCA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION, SYMBOL = ACCEPTED_SYMBOL),
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
  select(UID, local_code, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, COE_REG_ID, PSTL_CODE, COE_REG_ID,
         NWC_CREG16)

cov21b <- left_join(cov21a, site21a, by = c("UID", "local_code", "SITE_ID", "UNIQUE_ID", "PSTL_CODE"))

# Bring in C, native, wetland indicator status
taxa21 <- read.csv("./data/taxa_lists/nwca21_planttaxa-data.csv")
c21 <- read.csv("./data/taxa_lists/nwca21_plantcval-data.csv")
nat21 <- read.csv('./data/taxa_lists/nwca21_plantnative-data.csv')
wis21 <- read.csv('./data/taxa_lists/nwca21_plantwis-data.csv')

cov21t <- left_join(cov21b, taxa21 |> select(SPECIES_NAME_ID, NWCA_NAME, ORDER, FAMILY, GENUS,
                                             GROWTH_HABIT, DURATION, SYMBOL = ACCEPTED_SYMBOL),
                    by = c("SPECIES_NAME_ID"))
cov21nat <- left_join(cov21t, nat21 |> select(-PUBLICATION_DATE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "PSTL_CODE" = "GEOG_ID"))
cov21c <- left_join(cov21nat, c21 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                    by = c("SPECIES_NAME_ID", "NWCA_NAME", "NWC_CREG16" = "GEOG_ID"))
cov21wis <- left_join(cov21c, wis21 |> select(-PUBLICATION_DATE, -GEOG_TYPE),
                      by = c("SPECIES_NAME_ID", "NWCA_NAME", "COE_REG_ID" = "GEOG_ID"))

head(cov21wis) # 2021 data ready to join with other years (some columns )

# column name updates before combining
cov11 <- cov11wis |> select(UID, local_code, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CC, NATSTAT = NWCA_NATSTAT, WIS, ECOIND, ALIEN)

cov16 <- cov16wis |> select(UID, local_code, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            SPECIES_NAME_ID, SYMBOL,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

cov21 <- cov21wis |> select(UID, local_code, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            STATE = PSTL_CODE, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

covcomb <- rbind(cov11, cov16, cov21)
head(covcomb)

# write.csv(covcomb, paste0(export_path, "comb_data/Plant_Cover_2011-2021.csv"), row.names = F)

covcomb <- rbind(cov11, cov16, cov21)
covcomb$NWCA_NAME <- gsub("×", "x", covcomb$NWCA_NAME)
head(covcomb)

# Add invasive column to plant cover data
covcomb_final1 <- left_join(covcomb |> select(-SYMBOL),
                           taxa_wide |> select(SPECIES_NAME_ID, SYMBOL, INVASIVE),
                           by = c("SPECIES_NAME_ID")) |>
  filter(!is.na(SPECIES_NAME_ID)) # drops 2 empty records from 2021

head(covcomb_final1)
cov_na <- covcomb_final1 |> filter(is.na(NWCA_NAME)) |>
  select(local_code, YEAR, NWCA_NAME, SYMBOL) # empty df

# Making species names consistent (lots of ssp and vars in 2021 data)
spp_update <- function(df, nwca_name_orig, nwca_name_new, sppID, symbol){
  df$SPECIES_NAME_ID[df$NWCA_NAME == nwca_name_orig] <- sppID
  df$SYMBOL[df$NWCA_NAME == nwca_name_orig] <- symbol
  df$NWCA_NAME[df$NWCA_NAME == nwca_name_orig] <- nwca_name_new
  df
}

covcomb_final1 <- spp_update(covcomb_final1, "ALNUS INCANA SSP. RUGOSA", "ALNUS INCANA", 2989, "ALIN2")
covcomb_final1 <- spp_update(covcomb_final1, "ANDROMEDA POLIFOLIA VAR. GLAUCOPHYLLA", "ANDROMEDA POLIFOLIA", 4092, "ANPO")
covcomb_final1 <- spp_update(covcomb_final1, "CALOPOGON TUBEROSUS VAR. TUBEROSUS", "CALOPOGON TUBEROSUS", 14557, "CATU5")
covcomb_final1 <- spp_update(covcomb_final1, "CAREX LASIOCARPA VAR. AMERICANA", "CAREX LASIOCARPA", 16265, "CALA11")
covcomb_final1 <- spp_update(covcomb_final1, "ERIOPHORUM ANGUSTIFOLIUM SSP. ANGUSTIFOLIUM", "ERIOPHORUM ANGUSTIFOLIUM", 33528, "ERAN6")
covcomb_final1 <- spp_update(covcomb_final1, "LINNAEA BOREALIS SSP. LONGIFLORA", "LINNAEA BOREALIS", 49412, "LIBO3")
covcomb_final1 <- spp_update(covcomb_final1, "SARRACENIA PURPUREA SSP. PURPUREA VAR. PURPUREA", "SARRACENIA PURPUREA", 76786, "SAPU4")
covcomb_final1 <- spp_update(covcomb_final1, "SOLIDAGO ULIGINOSA VAR. LINOIDES", "SOLIDAGO ULIGINOSA", 80892, "SOUL")
covcomb_final1 <- spp_update(covcomb_final1, "THELYPTERIS PALUSTRIS VAR. PUBESCENS", "THELYPTERIS PALUSTRIS", 84920, "THPA")
covcomb_final1 <- spp_update(covcomb_final1, "VIBURNUM NUDUM VAR. CASSINOIDES", "VIBURNUM NUDUM", 88708, "VINU")
covcomb_final1 <- spp_update(covcomb_final1, "MORELLA CAROLINIENSIS", "MORELLA PENSYLVANICA", 55927, "MOPE6")
covcomb_final1$GROWTH_HABIT[covcomb_final1$NWCA_NAME == "SARRACENIA PURPUREA"] <- "FORB/HERB"

# Compile and add Invasive column to plant cover data.
# all species recorded in plant cover data.
spplist <- covcomb_final1 |> select(SPECIES_NAME_ID, SYMBOL, NWCA_NAME, NATSTAT, ALIEN) |> unique() |>
  group_by(SPECIES_NAME_ID, NWCA_NAME, NATSTAT, ALIEN) |>
  fill(SYMBOL, .direction = "downup") |> unique()

spp_numbers <- unique(spplist$SPECIES_NAME_ID)

taxa_comb <- rbind(taxa11 |> select(SPECIES_NAME_ID, NWCA_NAME = USDA_NAME) |> mutate(year = 2011, SYMBOL = NA_character_),
                   taxa16 |> select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL = ACCEPTED_SYMBOL) |> mutate(year = 2016),
                   taxa21 |> select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL = ACCEPTED_SYMBOL) |> mutate(year = 2021))

taxa_comb$NWCA_NAME <- gsub("×", "x", taxa_comb$NWCA_NAME)

taxa_wide <- taxa_comb |> pivot_wider(names_from = year, values_from = SYMBOL, names_prefix = "yr") |>
  mutate(SYMBOL = ifelse(!is.na(yr2021), yr2021, yr2016)) |>
  filter(SPECIES_NAME_ID %in% spp_numbers) |>
  select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL)

table(complete.cases(taxa_wide$SYMBOL)) # none missing a symbol
taxa_wide |> group_by(SPECIES_NAME_ID) |> summarize(num = n()) |> filter(num>1) # no duplicate IDs

# read in invasive species list downloaded from USDA Plants
# % Cover Invasive - Using USDA PLANTS database to establish invasive status.
#      https://plants.usda.gov/noxious-invasive-search
# Doesn't appear that that state-level designations are all that helpful. Going to go with,
# if it's invasive in any state in the reporting region, it's invasive in the analysis. I
# can't think of an example where that isn't true.
#
# Invasive list needs the USDA symbol to be joined to the species in the cover data. However,
# the 2011 taxa tables don't include the symbol. I'm using the taxa 2016 and 2021 tables to
# fill in as much of those holes, then will manually add the missing species.

inv_spp1 <- read.csv("./data/taxa_lists/USDA_PLANTS_Invasive_Species_By_State_20251217_clean.csv") |>
  select(SYMBOL = Accepted.Symbol)

# Pruning by hand - species to drop from invasive list
drop_spp <- c("BIDEN", "CALLI6", "CASE13", "CRATA", "EPILO", "GEUM", "IMPAT", "2UNK",
              "MALUS", "MEAR4", "OXALI", "POPR", "RUHI", "RUID", "THDA", "URDI", "VIOP", "VIRE7")

inv_spp <- inv_spp1 |> filter(!SYMBOL %in% drop_spp) |> unique()

taxa_wide$INVASIVE <- ifelse(taxa_wide$SYMBOL %in% inv_spp$SYMBOL, 1, 0)
write.csv(taxa_wide, paste0(export_path, "comb_data/Plant_Species_List_2011-2021.csv"), row.names = F)


#--- NEIWPCC CoC values ---
regc <- read_xlsx("./data/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx", sheet = 2) |>
  select(Symbol = `Accepted Symbol`, Accepted_Name = `Accepted Name`, cval82 = `82`)

taxa_wide2 <- left_join(taxa_wide, regc, by = c("SYMBOL" = "Symbol"))
head(taxa_wide2)

# Add in the species CoCs where var or ssp prevented join
coc_update <- function(df, species, coc){
  df$cval82[df$NWCA_NAME == species] <- coc
  df
}

taxa_wide2 <- coc_update(taxa_wide2, "ALNUS INCANA", 3)
taxa_wide2 <- coc_update(taxa_wide2, "AMELANCHIER", 5)
taxa_wide2 <- coc_update(taxa_wide2, "ARETHUSA BULBOSA", 9)
taxa_wide2 <- coc_update(taxa_wide2, "BETULA PAPYRIFERA", 3)
taxa_wide2 <- coc_update(taxa_wide2, "CAREX ECHINATA SSP. ECHINATA", 3) #MNAP differs from EPA
taxa_wide2 <- coc_update(taxa_wide2, "CAREX MAGELLANICA SSP. IRRIGUA", 7)
taxa_wide2 <- coc_update(taxa_wide2, "OCLEMENA xBLAKEI", 5)
taxa_wide2 <- coc_update(taxa_wide2, "RUBUS IDAEUS", 2)
taxa_wide2 <- coc_update(taxa_wide2, "SPARGANIUM EMERSUM", 6)
taxa_wide2 <- coc_update(taxa_wide2, "SOLIDAGO ULIGINOSA", 8)
taxa_wide2 <- coc_update(taxa_wide2, "VIBURNUM NUDUM", 5)
taxa_wide2 <- coc_update(taxa_wide2, "VIOLA", 4)

covcomb_final <- left_join(covcomb_final1, taxa_wide2 |> select(SPECIES_NAME_ID, NWCA_NAME, CVAL_final = cval82),
                           by = c("SPECIES_NAME_ID", "NWCA_NAME"))

write.csv(covcomb_final, paste0(export_path, "comb_data/Plant_Cover_2011-2021.csv"), row.names = F)

#---- Compiling VMMI from EPA data ----
# Compile plot list to left_join results with
plot_list1 <- covcomb_final |> select(UID, local_code, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83,
                                      YEAR, VISIT_NO, STATE, CREG, WISREG) |>
  unique() |> arrange(UNIQUE_ID)

# It appears that the plot tables don't always include data on number of plots, so I can't
# use that to sum up the number of plots per site. The cover data is pretty consistent, with only
# one site not having 5 veg plots. Going to just use that to sum up the number of veg plots for
# site level averaging.

# count plots from cover data
num_plots1 <- covcomb_final |> select(UID, local_code, UNIQUE_ID, SITE_ID, YEAR, PLOT) |> unique() |>
  group_by(UID, local_code, UNIQUE_ID, SITE_ID, YEAR) |>
  summarize(num_vplots = sum(!is.na(PLOT)), .groups = 'drop')

table(num_plots1$local_code, num_plots1$YEAR, num_plots1$num_vplots)

#fill_plots <- left_join(miss_plot_num, num_plots1, by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR"))

plot_list <- left_join(plot_list1, num_plots1, by = c("UID","local_code", "UNIQUE_ID", "SITE_ID", "YEAR"))

# It appears that the plot tables don't always include data on number of plots, so I can't
# use that to sum up the number of plots per site. The cover data is pretty consistent, with only
# one site not having 5 veg plots. Going to just use that to sum up the number of veg plots for
# site level averaging.


# % Bryophyte
bryo_sum <- bryo_all |> group_by(UID, local_code, SITE_ID, VISIT_NO, YEAR) |>
  summarize(bryo_sum = sum(BRYOPHYTES, na.rm = T), .groups = 'drop')

plot_bryo <- left_join(plot_list, bryo_sum, by = c("UID", "local_code", "SITE_ID", "VISIT_NO", "YEAR")) |>
  mutate(bryo_cov = bryo_sum/num_vplots) |> select(-bryo_sum)
head(plot_bryo)

# Mean C
head(covcomb_final)
table(covcomb_final$CVAL_final, useNA = 'always')

# Calculating meanC by making a list of all species on the plot, then calc. C,
# so species found in all 5 veg plots don't count more in the mean C than
# rare species that only occur once
cval <- covcomb_final |> mutate(CVAL_num = as.numeric(CVAL_final)) |>
  #filter(!is.na(CVAL_num)) |>
  filter(COVER > 0) |>
  select(UID, local_code, UNIQUE_ID, SITE_ID, YEAR, CVAL_num, SPECIES_NAME_ID, NWCA_NAME) |>
  unique() |>
  group_by(UID, local_code, UNIQUE_ID, SITE_ID, YEAR) |>
  summarize(#totalC = sum(CVAL_num, na.rm = T),
    # numSpp = sum(!is.na(CVAL_num)),
    # meanC2 = totalC/numSpp,
    meanC = mean(CVAL_num, na.rm = T),
    # Cdiff = meanC2-meanC,
    .groups = 'drop')
#ignore NAs warning. Used to intentionally drop non-numeric values
head(cval)
head(plot_bryo)

plot_br_cv <- left_join(plot_bryo, cval,
                        by = c("UID", "local_code", "UNIQUE_ID", "SITE_ID", "YEAR"))

# % Cover Disturbance Tolerant & Invasive
dist_inv_sum1 <- covcomb_final |> mutate(CVAL_num = as.numeric(CVAL_final)) |>
  filter(!is.na(CVAL_num)) |>
  mutate(cov_disttol = ifelse(CVAL_num <= 4, COVER, 0),
         cov_inv = ifelse(INVASIVE == 1, COVER, 0))

dist_inv_sum <- dist_inv_sum1 |>
  group_by(UID, local_code, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO) |>
  summarize(disttol_sum = sum(cov_disttol),
            invcov_sum = sum(cov_inv),
            .groups = 'drop')

head(dist_inv_sum1)
dist_inv_plot <- left_join(plot_list, dist_inv_sum,
                           by = c("UID", "local_code", "UNIQUE_ID", "SITE_ID", "YEAR", "VISIT_NO")) |>
  mutate(disttol_cov = disttol_sum/num_vplots,
         inv_cov = invcov_sum/num_vplots)

plot_vmmi <- left_join(plot_br_cv, dist_inv_plot |> select(UID, local_code, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO,
                                                           disttol_cov, inv_cov),
                       by = c("UID", "local_code", "UNIQUE_ID", "SITE_ID", "YEAR", "VISIT_NO")) |>
  mutate(meanC_adj1 = ifelse(meanC < 3.015, 3.015, ifelse(meanC > 7.346, 7.346, meanC)),
         meanC_adj2 = ((meanC_adj1 - 3.015)/(7.346 - 3.015)) * 10,

         covtol_adj1 = ifelse(disttol_cov < 0.386, 0, ifelse(disttol_cov > 136.645, 136.645, disttol_cov)),
         covtol_adj2 = ((((covtol_adj1 - 0.386)/(136.645 - 0.386))*10) - 10) * -1,

         invcov_adj1 = ifelse(inv_cov > 38.45, 38.45, inv_cov),
         invcov_adj2 = ((((invcov_adj1/38.45) * 10) - 10))*-1,

         bryo_adj1 = ifelse(bryo_cov > 98.48, 98.48, bryo_cov),
         bryo_adj2 = (bryo_adj1/98.48) * 10,

         vmmi1 = meanC_adj2 + covtol_adj2 + invcov_adj2 + bryo_adj2,
         vmmi2 = ifelse(vmmi1 < 0.389, 0.389, vmmi1),
         vmmi = ((vmmi2 - 0.389)/(40 - 0.389)) * 100,
         vmmi_rating = ifelse(vmmi > 65.22746, "Good", ifelse(vmmi < 52.785, "Poor", "Fair"))
  ) |> select(-vmmi1, -vmmi2)

head(plot_vmmi)
table(plot_vmmi$local_code, plot_vmmi$vmmi_rating)

plot_vmmi$vmmi_rating_fac <- factor(plot_vmmi$vmmi_rating, levels = c("Good", "Fair", "Poor"))

ggplot(plot_vmmi, aes(x = YEAR, y = vmmi, color = vmmi_rating_fac, group = local_code)) + theme_bw() +
  geom_point() + geom_line(color = 'grey') + #facet_wrap(~STATE) +
  scale_color_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2"),
                     name = "VMMI Rating")

ggplot(plot_vmmi, aes(x = YEAR, y = vmmi, color = vmmi_rating_fac, group = local_code)) + theme_bw() +
  geom_point() + geom_line(color = 'grey') + facet_wrap(~local_code) +
  labs(y = "Vegetation MMI", x = "Year") +
  scale_color_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2"),
                     name = "VMMI Rating")

write.csv(plot_vmmi, paste0(export_path, "comb_data/Vegetation_MMI_2011-2021.csv"), row.names = F)

vmmi_simp <- plot_vmmi[,c("SITE_ID", "local_code", "YEAR", "bryo_cov", "meanC", "disttol_cov", "inv_cov",
                          "meanC_adj2", "covtol_adj2", "invcov_adj2", "bryo_adj2",
                          "vmmi", "vmmi_rating")]

#---- Summarize cover data by site ----
head(covcomb_final)
table(plot_list$num_vplots)

spplist <- covcomb_final |> group_by(YEAR, SPECIES_NAME_ID, SYMBOL, NWCA_NAME,
                                     #ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                                     CVAL, NATSTAT, WIS, ECOIND, ALIEN, INVASIVE) |>
  summarize(num_recs = n(), .groups = 'drop') |>
  pivot_wider(names_from = YEAR, values_from = num_recs, values_fill = 0, names_prefix = "yr_") |>
  arrange(NWCA_NAME)

write.csv(spplist, paste0(export_path, "comb_data/Species_list_by_year.csv"), row.names = F)

covsum <- left_join(covcomb_final, plot_list |> select(UID, local_code, YEAR, num_vplots),
                    by = c("UID", "local_code", "YEAR")) |>
  group_by(UNIQUE_ID, local_code, YEAR, NWCA_NAME, SYMBOL, CVAL_final) |>
  summarize(sum_cov = sum(COVER, na.rm = T),
            mean_cov = sum_cov/first(num_vplots),
            .groups = 'drop') |> select(-sum_cov) |>
  pivot_wider(names_from = YEAR, values_from = mean_cov, values_fill = 0, names_prefix = "yr_") |>
  arrange(local_code, NWCA_NAME)

write.csv(covsum, paste0(export_path, "./comb_data/Plant_Cover_Sum_2011-2021.csv"), row.names = F)
head(covsum)

