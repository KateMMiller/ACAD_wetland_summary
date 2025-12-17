# Download EPA data
library(rvest)
library(stringr)
library(tidyverse)
# library(tidycensus)
download_path <- "C:/NETN/R_Dev/ACAD_wetland_summary/data/epa_all/"

#--- Read html on website ---
page_orig <- "https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys"
page_read <- read_html(page_orig)

#--- Download data files ---
# Detect hyperlinks ending in .csv
csv_list <- page_read |> html_nodes("a") |> html_attr("href") |> str_subset(regex("\\.csv", ignore_case = T))
nwca_data <- str_subset(csv_list, "nwca")
nwca_data_names <- sub(".*\\/", "", nwca_data)

# Download all hyperlinks with nwca and .csv in their name
lapply(seq_along(nwca_data_names), function(x){
  link = nwca_data[x]
  filename = nwca_data_names[x]
  download.file(link, paste0(download_path, filename))
})

#--- Download metadata files ---
txt_list <- page_read |> html_nodes("a") |> html_attr("href") |> str_subset("\\.txt")
nwca_meta <- str_subset(txt_list, "nwca")
nwca_meta_names <- sub(".*\\/", "", nwca_meta)

#dir.create("./data/metadata/") # added folder for metadata
lapply(seq_along(nwca_meta_names), function(x){
  link = nwca_meta[x]
  filename = nwca_meta_names[x]
  download.file(link, paste0(download_path, 'metadata/', filename))
})

#--- Compile plots in NCE, Visit 1, PROB only ---
# 2011 Data
siteinfo_2011 <- read.csv("./data/epa_all/nwca11_siteinfo.csv")
nce_sites_2011 <- siteinfo_2011$UID[siteinfo_2011$RPT_UNIT == "NCE" &
                                    siteinfo_2011$VISIT_NO == 1 &
                                    siteinfo_2011$SITETYPE == "PROB"] # dropped ACAD sites
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
  write.csv(dat, paste0("./data/epa_nce/", csv2011_name[x]), row.names = F)
})
)

# 2016 Data
siteinfo_2016 <- read.csv("./data/epa_all/nwca-2016-site-information-data_0.csv")
nce_sites_2016 <- siteinfo_2016$UID[siteinfo_2016$RPT_UNIT == "NCE" &
                                      siteinfo_2016$VISIT_NO == 1 &
                                      siteinfo_2016$SITETYPE == "PROB" &
                                      !is.na(siteinfo_2016$UID)]
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
    write.csv(dat, paste0("./data/epa_nce/", csv2016_name[x]), row.names = F)
  })
)

# 2021 Data
siteinfo_2021 <- read.csv("./data/epa_all/nwca21_siteinfo-data.csv")
nce_sites_2021 <- siteinfo_2021$UID[siteinfo_2021$RPT_UNIT == "NCE" &
                                      siteinfo_2021$VISIT_NO == 1 &
                                      siteinfo_2021$SITETYPE == "PROB" &
                                      !is.na(siteinfo_2021$UID)]
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
    write.csv(dat, paste0("./data/epa_nce/", csv2021_name[x]), row.names = F)
  })
)
#--- DOWNLOAD NOTES ---
# 2021 Site info includes records for sites that weren't sampled. Have to drop anything where is.na(UID)
# Consider using weights set by EPA for each site (TNT_CAT) for small area estimation approach?
# SITE_ID is the same across years, except the NWCA## changes.
# The UNIQUE_ID column in siteinfo is what links the same sites across years. It appears that sites are
# only sampled twice, so if they were sampled in 2011 and 2021, they were dropped in 2021.
#   - Need to compile plant taxa tables separately
#   - Dropped nwca_2016_veg_mmi.csv from initial compile, because doesn't include UID (only SITE_ID)
#   - Dropped ncwa21_landscape_metrics-data.csv because no UID column (only SiteID)


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

write.csv(site_all, "./data/comb_data/Site_Information_2011-2021.csv", row.names = F)

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
  arrange(SITE_ID, Year, VISIT_NO)

plot_sum <- plot_all |> group_by(UID, SITE_ID, VISIT_NO, Date, Year) |>
  summarize(num_plots = sum(!is.na(PLOT)),
            miss_plots = sum(is.na(PLOT)),
            .groups = "drop")

table(plot_sum$Year) # 2011: 27; 2016: 94; 2021: 96; 2022: 12
table(complete.cases(plot_all$PLOT)) #159 FALSE; 313 TRUE; Lots of PLOT blanks in 2021.
# Not clear what the blanks in 2021 are about yet - were they not sampled?

write.csv(plot_all, "./data/comb_data/Vegetation_Plot_Location_2011-2021.csv", row.names = F)

# Bryophyte cover
bryo11 <- read.csv("./data/epa_nce/nwca2011_vegtype_grndsurf.csv") |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo16 <- read.csv('./data/epa_nce/nwca2016_vegetation_type_data.csv') |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)
bryo21 <- read.csv('./data/epa_nce/nwca2021_vegtype_wide_data.csv') |>
  select(UID, SITE_ID, VISIT_NO, PLOT, BRYOPHYTES)

bryo_all <- rbind(bryo11, bryo16, bryo21)

write.csv(bryo_all, "./data/comb_data/Bryophyte_Cover_2011-2021.csv", row.names = F)

# Plant Cover Data
# 2011 plant data
cov11a <- read.csv("./data/epa_nce/nwca2011_plant_pres_cvr.csv") |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)
site11a <- read.csv("./data/epa_nce/nwca2011_site_info.csv") |>
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
  rename(NWCA_NAME = USDA_NAME) |>
  mutate(SYMBOL = NA_character_)

head(cov11wis) # future years use different name.

# cov11wis ready to join with remaining years.

# 2016 plant data
cov16a <- read.csv('./data/epa_nce/nwca2016_plant_species_cover_height_data.csv') |>
  select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, LINE, PLOT,
         NWC_CREG16, SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)

site16a <- read.csv("./data/epa_nce/nwca2016_site_info.csv") |>
  select(UID, SITE_ID, UNIQUE_ID, COE_REG_ID, NWC_CREG11, NWC_CREG16)
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
  select(UID, SITE_ID, UNIQUE_ID, COE_REG_ID, PSTL_CODE, COE_REG_ID,
         NWC_CREG16)

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
cov11 <- cov11wis |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, PLOT,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CC, NATSTAT = NWCA_NATSTAT, WIS, ECOIND, ALIEN)

cov16 <- cov16wis |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, PLOT,
                            SPECIES_NAME_ID, SYMBOL,
                            STATE = NWC_CREG11, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

cov21 <- cov21wis |> select(UID, UNIQUE_ID, SITE_ID, YEAR, VISIT_NO, PLOT,
                            STATE = PSTL_CODE, CREG = NWC_CREG16, WISREG = COE_REG_ID,
                            SPECIES_NAME_ID, SYMBOL,
                            NWCA_NAME, COVER, HEIGHT, NE, SW,
                            ORDER, FAMILY, GENUS, GROWTH_HABIT, DURATION,
                            CVAL = NWCA_CVAL, NATSTAT = NWCA_NATSTAT, WIS, ECOIND = ECOIND1, ALIEN)

covcomb <- rbind(cov11, cov16, cov21)
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

table(covcomb$STATE, covcomb$YEAR) # Confirmed in site info that 2011 only had target sites in IA and NJ
table(covcomb$CREG, covcomb$YEAR)
table(covcomb$WISREG, covcomb$YEAR)


write.csv(covcomb, "./data/comb_data/Plant_Cover_2011-2021.csv", row.names = F)


# Compiling VMMI from EPA data
# % Bryophyte : Done
# Mean C
# % Cover Disturbance Tolerante
# % Cover Invasive


# % Cover Invasive - Using USDA PLANTS database to establish invasive status.
#      https://plants.usda.gov/noxious-invasive-search
# Doesn't appear that that state-level designations are all that helpful. Going to go with,
# if it's invasive in any state in the reporting region, it's invasive in the analysis. I
# can't think of an example where that isn't true.

spplist <- covcomb |> select(SPECIES_NAME_ID, SYMBOL, NWCA_NAME, NATSTAT, ALIEN) |> unique() |>
  group_by(SPECIES_NAME_ID, NWCA_NAME, NATSTAT, ALIEN) |>
  fill(SYMBOL, .direction = "downup") |> unique()

dup_spp <- data.frame(table(spplist$SYMBOL)) |> filter(Freq > 1) |>
  mutate(duplicate = 1)

spplist <- left_join(spplist, dup_spp, by = c("SYMBOL" = "Var1"))

# Pruning by hand - species to drop from invasive list
drop_spp <- c("BIDEN", "CALLI6", "CASE13", "CRATA", "EPILO", "GEUM", "IMPAT", "2UNK",
              "MALUS", "MEAR4", "OXALI", "POPR", "RUHI", "RUID", "THDA", "URDI", "VIOP", "VIRE7")

inv_spp <- read.csv("./data/taxa_lists/USDA_PLANTS_Invasive_Species_By_State_20251217_clean.csv") |>
  select(Accepted.Symbol) |> filter(!Accepted.Symbol %in% drop_spp) |> unique()

head(inv_spp)
# ENDED HERE ++++ Add a column to the covcomb on whether it's invasive based on the symbol.
# but have to relate to the species name b/c 2011 doesn't have symbols attached.

# data(fips_codes) # from tidycensus
# inv_spp <- left_join(inv_spp1, fips_codes |> select(state, state_code) |> unique(),
#                      by = c("FIPS" = "state_code"))
# head(inv_spp)

