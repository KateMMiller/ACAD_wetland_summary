#-------------------------------------------------------------------------
# Download and compile EPA NWCA data for sites in the NCE reporting unit
# Later change to Northeast Level 3 Ecoregion codes 58, 59, 82, 83, 84
# because the Coefficient of Conservation values were assigned during the
# same workshop/same botanists, and most closely resemble the values used
# to develop the vegetation MMI and thresholds. The CoC values in the EPA
# taxa tables change a lot among years, affecting the MMI calculations for
# mathematical and not ecological reasons. All MMI calculations will use
# The NEIWPCC CoCs assigned here:
#   https://neiwpcc.org/wp-content/uploads/2018/03/Northeast-FQA_NEWIPCC_-FINAL-REPORT_March-2018.pdf
#   http://neiwpcc.org/wp-content/uploads/2018/03/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx
#-------------------------------------------------------------------------

# EPA NWCA data were downloaded in 01_download_NWCA_NCE_data.R. This script extracts and compiles the EPA
# R1 data for sites within US_L3CODE 58, 59, 82, 83, and 84

#--- Params ----
library(tidyverse)
library(readxl)
export_path <- "C:/NETN/R_Dev/ACAD_wetland_summary/data/epa_r1/"
#dir.create(paste0(export_path, "comb_data/"))

#--- Combining data across years ----
# For the following datasets, I'm row binding data across visits by intersecting the columns each dataset has
# in common to start with. There may be columns in later datasets that need adding, but I'll deal
# with that later.

#---- Site Info ----
site11 <- read.csv("./data/epa_nce/nwca2011_site_info.csv")
site16 <- read.csv("./data/epa_nce/nwca2016_site_info.csv")
site21 <- read.csv("./data/epa_nce/nwca2021_site_info.csv")

comm_site_names <- intersect(intersect(names(site11), names(site16)), names(site21))

site_all <- rbind(site11[,comm_site_names], site16[,comm_site_names], site21[,comm_site_names]) |>
  arrange(UNIQUE_ID, DATE_COL, VISIT_NO)

site_all$Date <- as.POSIXct(site_all$DATE_COL, format = "%m/%d/%Y")
site_all$Year <- format(site_all$Date, format = "%Y")

site_freq <- data.frame(table(site_all$UNIQUE_ID))

table(site_all$RPT_UNIT) #318 NCE
table(site_all$VISIT_NO) #318 1
table(site_all$Year) # 12 in 2022?

head(site_all)

site_r1 <- site_all |> filter(US_L3CODE %in% c(58, 59, 82, 83, 84)) # 137
r1_uids <- sort(unique(site_r1$UID))
# table(site_r1$UID, site_r1$Year)

site_l3 <- site_r1 |> select(UID, US_L3CODE)

write.csv(site_r1, paste0(export_path, "comb_data/Site_Information_2011-2021.csv"), row.names = F)

#---- Plant data ----
# 1. Compile # of plots sampled per site x visit (should be 5 for most)
# 2. Link C and Native status to species data by GEOD_ID/NWC_CREG code.
    # Do each visit separately?
    # Move plant taxa csvs from epa_all into a taxa specific folder for easier compiling
# 3. Compile Bryophyte cover using the vegetation type data

# Veg plot data
plot11 <- read.csv('./data/epa_nce/nwca2011_vegplotloc.csv')
plot16 <- read.csv('./data/epa_nce/nwca2016_veg_plot_location_data.csv')
plot21 <- read.csv('./data/epa_nce/nwca2021_vegplotloc_wide_data.csv')

# Dates are different formats across years
plot11$Date <- as.Date(plot11$DATE_COL, format = "%d-%b-%y")
plot11$Year <- format(plot11$Date, "%Y")
plot16$Date <- as.Date(plot16$DATE_COL, format = "%m/%d/%Y")
plot16$Year <- format(plot16$Date, "%Y")
plot21$Date <- as.Date(plot21$DATE_COL, format = "%d-%b-%y")
plot21$Year <- format(plot21$Date, "%Y")

comm_p_names <- intersect(intersect(names(plot11), names(plot16)), names(plot21))

plot_all <- rbind(plot11[,comm_p_names], plot16[,comm_p_names], plot21[,comm_p_names]) |>
  arrange(SITE_ID, Year, VISIT_NO) |>
  filter(UID %in% r1_uids)

plot_sum1 <- plot_all |> group_by(UID, SITE_ID, VISIT_NO, Date, Year) |>
  summarize(num_plots = sum(!is.na(PLOT), na.rm = T),
            miss_plots = sum(is.na(PLOT)),
            .groups = "drop")

plot_sum <- left_join(plot_sum1, site_l3, by = "UID")

table(plot_sum$Year) # 2011: 19; 2016: 39; 2021: 42;
table(complete.cases(plot_all$PLOT)) #55 FALSE; 198 TRUE; Lots of PLOT blanks in 2021.
# Not clear what the blanks in 2021 are about yet - were they not sampled?

write.csv(plot_all, paste0(export_path, "comb_data/Vegetation_Plot_Location_2011-2021.csv"), row.names = F)

# Bryophyte cover
bryo11 <- read.csv("./data/epa_nce/nwca2011_vegtype_grndsurf.csv") |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo11$YEAR <- 2011

bryo16 <- read.csv('./data/epa_nce/nwca2016_vegetation_type_data.csv') |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES, DATE_COL)
bryo16$YEAR <- format(as.Date(bryo16$DATE_COL, format = "%m/%d/%Y"), "%Y")
table(bryo16$YEAR, useNA = 'always')

bryo21 <- read.csv('./data/epa_nce/nwca2021_vegtype_wide_data.csv') |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES, DATE_COL)
head(bryo21)
bryo21$YEAR <- format(as.Date(bryo21$DATE_COL, "%d-%b-%y"), "%Y")
table(bryo21$YEAR, useNA = 'always')

bryo_all1 <- rbind(bryo11, bryo16 |> select(-DATE_COL), bryo21 |> select(-DATE_COL)) |>
  filter(UID %in% r1_uids)

bryo_all <- left_join(bryo_all1, site_l3, by = "UID")

write.csv(bryo_all, paste0(export_path, "comb_data/Bryophyte_Cover_2011-2021.csv"), row.names = F)

# Plant Cover Data
# 2011 plant data
cov11a <- read.csv("./data/epa_nce/nwca2011_plant_pres_cvr.csv") |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)
site11a <- read.csv("./data/epa_nce/nwca2011_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, COE_REG_ID, NWC_CREG11, NWC_CREG16, US_L3CODE)
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
  rename(NWCA_NAME = USDA_NAME) |>
  mutate(SYMBOL = NA_character_)

head(cov11wis) # future years use different name.

# cov11wis ready to join with remaining years.

# 2016 plant data
cov16a <- read.csv('./data/epa_nce/nwca2016_plant_species_cover_height_data.csv') |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)

site16a <- read.csv("./data/epa_nce/nwca2016_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, COE_REG_ID, NWC_CREG11, NWC_CREG16, US_L3CODE)
cov16b <- left_join(cov16a, site16a, by = c("UID", "SITE_ID", "UNIQUE_ID", "NWC_CREG16"))
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
cov21a <- read.csv('./data/epa_nce/nwca2021_plant_wide_data.csv')
cov21a$YEAR <- format(as.Date(cov21a$DATE_COL, format = "%d-%b-%y"), "%Y")

site21a <- read.csv("./data/epa_nce/nwca2021_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, COE_REG_ID, PSTL_CODE, COE_REG_ID,
         NWC_CREG16, US_L3CODE)

cov21b <- left_join(cov21a, site21a, by = c("UID", "SITE_ID", "UNIQUE_ID", "PSTL_CODE"))

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
cov11 <- cov11wis |> select(UID, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            US_L3CODE,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CC, NATSTAT = NWCA_NATSTAT, WIS, ECOIND, ALIEN)

cov16 <- cov16wis |> select(UID, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            US_L3CODE,
                            SPECIES_NAME_ID, SYMBOL,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

cov21 <- cov21wis |> select(UID, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83, YEAR, VISIT_NO, PLOT,
                            US_L3CODE,
                            STATE = PSTL_CODE, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

covcomb1 <- rbind(cov11, cov16, cov21)
covcomb1$NWCA_NAME <- gsub("×", "x", covcomb1$NWCA_NAME)

covcomb <- covcomb1 |> filter(UID %in% r1_uids)

head(covcomb)

table(cov11$STATE)
table(cov11$CREG)
table(cov11$WISREG)

table(cov16$STATE)
table(cov16$CREG)
table(cov16$WISREG)

table(cov21$STATE)
table(cov21$CREG)
table(cov21$WISREG)

table(covcomb$STATE, covcomb$YEAR) # NE states only
table(covcomb$CREG, covcomb$YEAR)
table(covcomb$WISREG, covcomb$YEAR)
table(covcomb$US_L3CODE, covcomb$YEAR)

# There were 100 species names that didn't match b/t EPA and NEIWPCC b/c of use of var. or ssp. Taking
# taxa to species, and did so in a separate spreadsheet.
taxa_fix <- read.csv(paste0(export_path, "comb_data/EPA_NEIWPCC_taxa_matching_import.csv"), fileEncoding = "Latin1")
head(taxa_fix)

covcomb_join <- left_join(covcomb, taxa_fix, by = c("SPECIES_NAME_ID" = "SPECIES_NAME_ID_orig",
                                                    "NWCA_NAME" = "NWCA_NAME_orig")) |>
  mutate(SPECIES_NAME_ID_fix = ifelse(!is.na(SPECIES_NAME_ID_new), SPECIES_NAME_ID_new, SPECIES_NAME_ID),
         NWCA_NAME_fix = ifelse(!is.na(NWCA_NAME_new), NWCA_NAME_new, NWCA_NAME),
         SYMBOL_fix = ifelse(!is.na(SYMBOL_new), SYMBOL_new, SYMBOL)
  )

table(complete.cases(covcomb_join$SYMBOL_fix), covcomb_join$YEAR) # Most blank Symbols are from 2011

#++++ ENDED HERE +++++

# Compile and add Invasive column to plant cover data.
# all species recorded in plant cover data.
spplist <- covcomb_join |> select(SPECIES_NAME_ID = SPECIES_NAME_ID_fix,
                                  SYMBOL = SYMBOL_fix,
                                  NWCA_NAME = NWCA_NAME_fix,
                                  NATSTAT, ALIEN) |> unique() |>
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
taxa_wide |> group_by(SPECIES_NAME_ID) |> summarize(num = n()) |> filter(num > 1) # no duplicate IDs

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

#--- NEIWPCC CoC values ---
# download.file("http://neiwpcc.org/wp-content/uploads/2018/03/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx",
#               destfile = "./data/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx")

regc <- read_xlsx("./data/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx", sheet = 2) |>
  select(Symbol = `Accepted Symbol`, Accepted_Name = `Accepted Name`, eco58 = `58`,
         eco59 = `59`, eco82 = `82`, eco83 = `83`, eco84 = `84`)
# +++++ ENDED HERE ++++
# NEED TO ADD covcomb_join fixes to species, then join with regc

# Clean up ssp or vars are preventing joins with taxa.
regc$Symbol[regc$Accepted_Name == "Carex debilis var. rudgei"] <- "CADE5"

taxa_wide2 <- left_join(taxa_wide, regc, by = c("SYMBOL" = "Symbol"))

write.csv(taxa_wide2, paste0(export_path, "./comb_data/EPA_NEIWPCC_taxa_matching.csv"), row.names = F)

head(regc)
head(taxa_wide)

write.csv(taxa_wide, paste0(export_path, "comb_data/Plant_Species_List_2011-2021.csv"), row.names = F)

# Add invasive column to plant cover data
covcomb2 <- left_join(covcomb |> select(-SYMBOL),
                      taxa_wide |> select(SPECIES_NAME_ID, SYMBOL, INVASIVE),
                      by = c("SPECIES_NAME_ID")) |>
  filter(!is.na(SPECIES_NAME_ID)) # drops 2 empty records from 2021

head(covcomb2)

# EPA's CoC values change a lot among years, affecting the vegetation MMI calculations
# for mathematical and not ecological reasons, and also don't match the latest CoCs
# used in New England developed by NEIWPCC. This code imports the NEIWPCC CoCs and
# assigns them to the plot using their US_L3CODE



covcomb3 <- left_join(covcomb2, regc, by = c("SYMBOL" = "SYMBOL"))

write.csv(covcomb_final, "./data/comb_data/Plant_Cover_2011-2021.csv", row.names = F)


#---- Compiling VMMI from EPA data ----

# Compile plot list to left_join results with
plot_list1 <- covcomb_final |> select(UID, UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83,
                                      YEAR, VISIT_NO, STATE, CREG, WISREG) |>
  unique() |> arrange(UNIQUE_ID)

plot_list2 <- left_join(plot_list1, plot_sum |> select(-Date),
                       by = c("UID", "SITE_ID", "VISIT_NO", c("YEAR" = "Year")))
head(plot_list2)

# Fix plots_missing plot count from plot_sum
#miss_plot_num <- plot_list2 |> filter(is.na(num_plots)) |> select(UID, UNIQUE_ID, SITE_ID, YEAR)

# count plots from cover data
num_plots1 <- covcomb_final |> select(UID, UNIQUE_ID, SITE_ID, YEAR, PLOT) |> unique() |>
  group_by(UID, UNIQUE_ID, SITE_ID, YEAR) |>
  summarize(num_plots_cov = sum(!is.na(PLOT)), .groups = 'drop')

#fill_plots <- left_join(miss_plot_num, num_plots1, by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR"))

plot_list3 <- left_join(plot_list2, num_plots1, by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR")) |>
  mutate(num_plots = ifelse(is.na(num_plots), num_plots_cov, num_plots),
         plot_diff = ifelse(abs(num_plots - num_plots_cov) %in% c(2, 3, 4), "XX", NA_character_))

# It appears that the plot tables don't always include data on number of plots, so I can't
# use that to sum up the number of plots per site. The cover data is pretty consistent, with only
# one site not having 5 veg plots. Going to just use that to sum up the number of veg plots for
# site level averaging.

plot_list <- plot_list3 |> select(UID:WISREG, num_vplots = num_plots_cov)
head(plot_list) # df for left_join

# % Bryophyte
bryo_sum <- bryo_all |> group_by(UID, SITE_ID, VISIT_NO, YEAR) |>
  summarize(bryo_sum = sum(BRYOPHYTES, na.rm = T), .groups = 'drop')

plot_bryo <- left_join(plot_list, bryo_sum, by = c("UID", "SITE_ID", "VISIT_NO", "YEAR")) |>
  mutate(bryo_cov = bryo_sum/num_vplots) |> select(-bryo_sum)
head(plot_bryo)

# Mean C
head(covcomb_final)
table(covcomb_final$CVAL, useNA = 'always')

# Calculating meanC by making a list of all species on the plot, then calc. C,
# so species found in all 5 veg plots don't count more in the mean C than
# rare species that only occur once
cval <- covcomb_final |> mutate(CVAL_num = as.numeric(CVAL)) |>
  filter(!is.na(CVAL_num)) |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, CVAL_num) |> unique() |>
  group_by(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO) |>
  summarize(meanC = mean(CVAL_num), .groups = 'drop')

#ignore NAs warning. Used to intentionally drop non-numeric values
head(cval)
head(plot_bryo)

plot_br_cv <- left_join(plot_bryo, cval, by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR", "VISIT_NO"))

# % Cover Disturbance Tolerant & Invasive
dist_inv_sum1 <- covcomb_final |> mutate(CVAL_num = as.numeric(CVAL)) |>
  filter(!is.na(CVAL_num)) |>
  mutate(cov_disttol = ifelse(CVAL_num <= 4, COVER, 0),
         cov_inv = ifelse(INVASIVE == 1, COVER, 0))

dist_inv_sum <- dist_inv_sum1 |>
  group_by(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO) |>
  summarize(disttol_sum = sum(cov_disttol),
            invcov_sum = sum(cov_inv),
            .groups = 'drop')

head(dist_inv_sum1)
dist_inv_plot <- left_join(plot_list, dist_inv_sum, by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR", "VISIT_NO")) |>
  mutate(disttol_cov = disttol_sum/num_vplots,
         inv_cov = invcov_sum/num_vplots)

plot_vmmi <- left_join(plot_br_cv, dist_inv_plot |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO,
                                                           disttol_cov, inv_cov),
                       by = c("UID", "UNIQUE_ID", "SITE_ID", "YEAR", "VISIT_NO")) |>
  mutate(meanC_adj1 = ifelse(meanC < 3.015, 3.015, ifelse(meanC > 7.346, 7.346, meanC)),
         meanC_adj2 = ((meanC_adj1 - 3.015)/(7.346 - 3.015)) * 10,

         covtol_adj1 = ifelse(disttol_cov < 0.386, 0, ifelse(disttol_cov > 136.645, 136.645, disttol_cov)),
         covtol_adj2 = ((((covtol_adj1 - 0.386)/(136.645 - 0.386))*10) - 10) * -1,

         invcov_adj1 = ifelse(inv_cov > 38.45, 38.45, inv_cov),
         invcov_adj2 = ((((invcov_adj1/38.45) * 10) - 10))*-1,

         bryo_adj1 = ifelse(bryo_cov > 98.48, 98.48, bryo_cov),
         bryo_adj2 = (bryo_adj1/98.48) * 10,

         vmmi = (((meanC_adj2 + covtol_adj2 + invcov_adj2 + bryo_adj2) - 0.389)/(40 - 0.389)) * 100,
         vmmi_rating = ifelse(vmmi > 65.22746, "Good", ifelse(vmmi < 52.785, "Poor", "Fair"))
  )

head(plot_vmmi)

table(plot_vmmi$STATE, plot_vmmi$vmmi_rating)

ggplot(plot_vmmi, aes(x = YEAR, y = vmmi, color = vmmi_rating)) + theme_bw() +
  geom_point() + #facet_wrap(~STATE) +
  scale_color_manual(values = c("Poor" = "indianred", "Fair" = "gold", "Good" = "green2"))

write.csv(plot_vmmi, "./data/comb_data/Vegetation_MMI_2011-2021.csv", row.names = F)
