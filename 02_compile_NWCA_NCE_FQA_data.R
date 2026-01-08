#------------------------------------------------------------------------------------
# Updating Coefficient of Conservatism scores for sites within the NCE reporting unit
# to then update VegMMI thresholds for distinguishing Good, Fair, and Poor
#------------------------------------------------------------------------------------

library(tidyverse)

export_path = "C:/NETN/R_Dev/ACAD_wetland_summary/data/epa_nce/"

# Ecoregions to include
ecoreg = c(49, 50, 51, 56, 58, 59, 60, 61, 62, 82, 83, 84)

#--- Combine the CoC lists ---
c_mn <- read.csv("./data/COCs/MN_FQA_Milburn_2007.csv") |> mutate(COC_reg = "MN")
c_wi <- read.csv("./data/COCs/WI_FQA_N_SC_Chung_2017.csv") |> mutate(COC_reg = "WI") # NCE only includes L3: 50 and 51 (N and SC)
c_mi <- read.csv("./data/COCs/MI_FQA_Merjent_2024.csv") |> mutate(COC_reg = "MI")
c_in <- read.csv("./data/COCs/IN_FQA_Roth_2019.csv") |> mutate(COC_reg = "IN")
c_ne_ma <- read.csv("./data/COCs/NE_MidAtlantic_Ecoreg_FQA_Faber_2021.csv", fileEncoding = "Latin1") |>
  select(SYMBOL = USDA.Accepted.Taxa..Symbol,
         Accepted.Scientific.Name,
         C_82 = Acadian.Plains...Hlils..82..CoC,
         C_58 = Northeastern.Highlands..58..CoC,
         C_59 = Northeast.Coastal..59..CoC,
         C_83 = Eastern.Great.Lakes..83..CoC,
         C_84 = Atlantic.Coastal.Pine.Barrens..84..CoC,
         C_60_61 = Allegheny.Plateau..Glaciated..60.61..CoC,
         C_62_69_70 = Allegheny.Plateau..UNglaciated..62.69.70..CoC,
         C_66_67_68 = Ridge...Valley..66.67.68..CoC,
         C_45_64 = Piedmont..45..64..CoC,
         C_63_65 = Mid.Atlantic.Coastal.Plain..63.65..CoC) |>
  pivot_longer(C_82:C_63_65, names_to = "COC_reg", values_to = "CVal") |>
  filter(!is.na(CVal)) |>
  mutate(SCIENTIFIC_NAME = toupper(Accepted.Scientific.Name)) |>
  select(SYMBOL, SCIENTIFIC_NAME, CVal, COC_reg)

head(c_ne_ma)

mw <- rbind(c_mn, c_wi, c_mi, c_in) |>
  mutate(SCIENTIFIC_NAME = toupper(Scientific.Name),
         SYMBOL = NA_character_) |>
  select(SYMBOL, SCIENTIFIC_NAME, CVal = C, COC_reg)
head(mw)

coc_list <- rbind(c_ne_ma, mw) |>
  arrange(SCIENTIFIC_NAME, COC_reg) |>
  group_by(SCIENTIFIC_NAME) |> fill(SYMBOL, .direction = "updown")

table(complete.cases(coc_list$SYMBOL)) # 3146 missing SYMBOLS, but going to wait to fix, to see which ones
# show up in the epa data.

#--- Combining data across years ----
# For the following datasets, I'm row binding data across visits by intersecting the columns each dataset has
# in common to start with. There may be columns in later datasets that need adding, but I'll deal
# with that later. Allowing REF and PROB sites to be in the dataset. Will split them later for the
# threshold revision (REF) and the regional assessment (PROB)

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
site_eco <- site_all |> filter(US_L3CODE %in% ecoreg)

mmi_uids <- sort(unique(site_eco$UID))

table(site_eco$RPT_UNIT) #422 NCE
table(site_eco$VISIT_NO) #422 1
table(site_eco$Year) # 11 in 2022?
table(site_eco$Year, site_eco$SITETYPE) # Most HAND are from 2011, but include some
table(site_eco$STATE_NM)
table(site_eco$US_L3CODE)

# in 2016 and 2021.
# Check out handpicked data. Only the 10 ACAD sites (NWC_ME-10024: NWC_ME-10033) have been
# sampled more than 1 time.
hand <- site_eco |> filter(SITETYPE == "HAND") |>
  select(UID, SITE_ID, UNIQUE_ID, US_L3CODE,
         STATE_NM, SITETYPE, Year)
table(hand$STATE_NM, hand$Year)

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

plot_eco <- plot_all |> filter(UID %in% mmi_uids)

plot_sum <- plot_eco |> group_by(UID, SITE_ID, VISIT_NO, Date, Year) |>
  summarize(num_plots = sum(!is.na(PLOT), na.rm = T),
            miss_plots = sum(is.na(PLOT)),
            .groups = "drop")

#plot_sum <- left_join(plot_sum1, site_l3, by = "UID")

table(plot_sum$Year) # 2011: 45; 2016: 128; 2021: 113;
table(complete.cases(plot_eco$PLOT)) #189 FALSE; 483 TRUE; Lots of PLOT blanks in 2021.
# Not clear what the blanks in 2021 are about yet - were they not sampled?

write.csv(plot_eco, "./data/comb_data/Vegetation_Plot_Location_2011-2021.csv", row.names = F)

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

bryo_all <- rbind(bryo11, bryo16 |> select(-DATE_COL), bryo21 |> select(-DATE_COL))
bryo_eco <- bryo_all |> filter(UID %in% mmi_uids)

write.csv(bryo_eco, "./data/comb_data/Bryophyte_Cover_2011-2021.csv", row.names = F)

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

covcomb <- rbind(cov11, cov16, cov21) |> filter(UID %in% mmi_uids)
covcomb$NWCA_NAME <- gsub("×", "x", covcomb$NWCA_NAME)

head(covcomb)

spplist1 <- covcomb |> select(SPECIES_NAME_ID, SYMBOL, NWCA_NAME, US_L3CODE, CREG, STATE) |>
  arrange(NWCA_NAME, SYMBOL) |>
  unique() |>
  group_by(SPECIES_NAME_ID, NWCA_NAME) |> fill(SYMBOL, .direction = "downup") |>
  mutate(COC_reg = paste0(STATE, "_", US_L3CODE),
         present = 1) |>
  arrange(COC_reg, NWCA_NAME) |> ungroup()#|>
  # select(-US_L3CODE, -STATE) |> unique() |>
  # pivot_wider(names_from = COC_reg, values_from = present, values_fill = 0) |>
  # arrange(NWCA_NAME)

head(spplist1)

cregs <- spplist1 |> select(CREG, COC_reg, US_L3CODE) |> unique()
head(cregs)
# spp_na <- spplist1 |> filter(is.na(SYMBOL))
table(complete.cases(spplist1$SYMBOL)) #359 missing symbols b/c 2011 data didn't include
# symbols and there must be differences in nomenclature or different species detected.

# Bring in Symbols from 2021 taxa list, which is the most complete, to have to update
# fewer symbols manually.

taxa21 <- read.csv("./data/taxa_lists/nwca21_planttaxa-data.csv") |>
  select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL21 = ACCEPTED_SYMBOL)

spplist2 <- left_join(spplist1, taxa21, by = c("SPECIES_NAME_ID", "NWCA_NAME")) |>
  select(-SYMBOL) |>
  select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL = SYMBOL21, everything()) |>
  filter(!is.na(SPECIES_NAME_ID))   # blank row came in from 2021 data

table(complete.cases(spplist2$SYMBOL)) # only 30; 4 unique symbols missing in 2021 data.

# Fixing the missing symbols manually (from 2011 and 2016 data)
spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91513] <- "ARONIA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91513] <- "ARONI2"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91514] <- "ARONIA xPRUNIFOLIA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91514] <- "ARPR2"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91516] <- "ARONIA MELANOCARPA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91516] <- "ARME6"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91538] <- "BOLBOSCHOENUS FLUVIATILIS"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91538] <- "BOFL3"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91577] <- "CAREX BILLINGSII"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91577] <- "CABI22"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 91707] <- "ERECHTITES HIERACIIFOLIUS"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 91707] <- "ERHI12"

spplist2$NWCA_NAME[spplist2$SPECIES_NAME_ID == 131571] <- "OENOTHERA GAURA"
spplist2$SYMBOL[spplist2$SPECIES_NAME_ID == 131571] <- "OEGA"

table(complete.cases(spplist2$SYMBOL)) # Only FALSE are SHRUB, which I'll delete.

spplist3 <- spplist2 |> filter(!is.na(NWCA_NAME))
table(complete.cases(spplist3$SYMBOL)) # all TRUE

head(spplist3)

#--- Combine NWCA species list with COC_list
head(coc_list)
sort(unique(spplist3$COC_reg))
head(spplist3)

spplist4 <- spplist3 |>
  mutate(COC_reg2 = case_when(STATE == "IN" ~ "IN",
                              STATE == "MI" ~ "MI",
                              STATE == "MN" ~ "MN",
                              STATE == "WI" ~ "WI",
                              TRUE ~ substr(COC_reg, 4, 5))) |>
  select(SPECIES_NAME_ID, NWCA_NAME, SYMBOL, COC_reg = COC_reg2, CREG) |>
  unique() |> arrange(NWCA_NAME, COC_reg)

head(spplist4)

coc_list2 <- coc_list |>
  mutate(COC_reg2 = sub("C_", "", COC_reg)) |>
  separate_wider_delim(cols = COC_reg2, delim = "_",
                       names = c("X1", "X2", "X3"),
                       too_few = "align_start") |>
  select(-COC_reg) |>
  pivot_longer(X1:X3, names_to = "junk", values_to = "COC_reg") |>
  filter(!is.na(COC_reg)) |> select(-junk) |>
  unique()

coc_list_miss_sym <- coc_list2 |> filter(is.na(SYMBOL))

sppcoc <- left_join(spplist4, coc_list2, by = c("SYMBOL", "COC_reg"))
sppcoc_miss <- sppcoc |> filter(is.na(SCIENTIFIC_NAME))
write.csv(sppcoc_miss, "./data/COCs/Species_missing_COCs.csv", row.names = F)

# Where I don't have CVal from newer sources, use values EPA assigned from 2021
# The values closely match COCs for MN, WI, MI, IN Coefs., but that are hard
# to match because the state FQA sources have wierd nomenclature.

taxa21_cval <- read.csv("./data/taxa_lists/nwca21_plantcval-data.csv") |>
  select(SPECIES_NAME_ID, GEOG_ID, NWCA_CVAL)

sppcoc2 <- left_join(sppcoc, taxa21_cval, by = c("SPECIES_NAME_ID",
                                                 "CREG" = "GEOG_ID")) |>
  mutate(CVAL_final = ifelse(is.na(CVal), NWCA_CVAL, CVal),
         COC_diff = as.numeric(CVal) - as.numeric(NWCA_CVAL))

head(sppcoc2)
table(sppcoc2$CVal, sppcoc2$NWCA_CVAL, useNA = 'always')

write.csv(sppcoc2, "./data/COCs/Full_COC_List.csv", row.names = F)

# Now import the updated list that was visually checked for species that required updated based
# on the reference lists. Tedius AF, I know.

coc_list <- read.csv("./data/COCs/Full_COC_List_final.csv")
