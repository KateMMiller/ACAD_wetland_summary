#-------------------------------------------------------------------
# Compiling RAM data for freshwater wetland trend analysis in ACAD
#-------------------------------------------------------------------
library(tidyverse)
library(wetlandACAD)
library(lme4)
library(sf)

# Combine RAM data from database and SENT data compiled from EPA NWCA sources
# RAM data
importRAM(export_protected = T)

vmmi1 <- sumVegMMI()
loc <- VIEWS_RAM$locations
vmmi <- left_join(vmmi1,
                  loc |> select(Code, FWS_Class_Code, HGM_Class, HGM_Sub_Class),
                  by = "Code") |>
  select(Code, Panel, X = xCoordinate, Y = yCoordinate, Year, meanC, Bryophyte_Cover,
         Invasive_Cover, Cover_Tolerant, vmmi, vmmi_rating, vmmi_rating_orig, FWS_Class_Code, HGM_Class)

# Great Meadow data
loc_grme <- read.csv("./data/grme/locations.csv") |> select(Code, FWS_Class_Code, HGM_Class)
vmmi_grme1 <- read.csv("./data/grme/ACAD_Wetland_VegMMI_20260126.csv")

vmmi_grme <- left_join(vmmi_grme1,
                       loc_grme |> select(Code, FWS_Class_Code, HGM_Class),
                       by = "Code") |>
  select(Code, Panel, X = xCoordinate, Y = yCoordinate, Year, meanC, Bryophyte_Cover,
         Invasive_Cover, Cover_Tolerant, vmmi, vmmi_rating, vmmi_rating_orig, FWS_Class_Code, HGM_Class)

# Sentinel data
vmmi_sent1 <- read.csv("./results/Vegetation_MMI_2011-2021_ACAD_REF.csv")

# Bring in FWS Code and HGM code
epa_site <- read.csv("./data/comb_data/Site_Information_2011-2021.csv") |>
  filter(UID %in% unique(vmmi_sent1$UID)) |>
  select(UID, WETCLS_EVL, WETCLS_HGM)

vmmi_sent2 <- left_join(vmmi_sent1, epa_site, by = "UID")

vmmi_sent_sf <- st_as_sf(vmmi_sent2, coords = c("LON_DD83", "LAT_DD83"), crs = 4269)
vmmi_sent_utm <- st_transform(vmmi_sent_sf, crs = 26919)
head(vmmi_sent_utm)
vmmi_sent_xy <- st_coordinates(vmmi_sent_utm)
head(vmmi_sent_xy)[,1]
vmmi_sent2 <- cbind(vmmi_sent2,
                   X = vmmi_sent_xy[,1], Y = vmmi_sent_xy[,2])

vmmi_sent3 <- vmmi_sent2 |>
  mutate(Panel = 0,
         HGM_Class = case_when(WETCLS_HGM %in% c("DEPRESSION", "DPRSS") ~ "Depression",
                               WETCLS_HGM == "FLATS" ~ "Flats",
                               WETCLS_HGM == "RIVERINE" ~ "Riverine",
                               WETCLS_HGM == "SLOPE" ~ "Slope")) |>
  select(Code = local_code, Panel, X, Y, Year = YEAR, meanC, Bryophyte_Cover = bryo_cov,
         Invasive_Cover = inv_cov, Cover_Tolerant = disttol_cov, vmmi, vmmi_rating,
         vmmi_rating_orig, FWS_Class_Code = WETCLS_EVL, HGM_Class)

# combine sites
vmmi_comb <- rbind(vmmi, vmmi_grme, vmmi_sent3)

# Check that COCs match between NETN and EPA analyses
# Only interested in species that have been found in ACAD.
ramspp <- VIEWS_RAM$species_list |> select(Latin_Name, PLANTS_Code, CoC_ME_ACAD) |> distinct()
spplist <- VIEWS_RAM$tlu_Plant |> select(Latin_Name, PLANTS_Code, CoC_ME_ACAD)

sent_veg <- read.csv("./data/comb_data/Plant_Cover_2011-2021.csv")

sentspp <- sent_veg |>
  filter(UID %in% unique(vmmi_sent1$UID)) |>
  select(SYMBOL, NWCA_NAME, CVAL_final, NWCA_CVAL, CVal, Updated) |> distinct()

setdiff(unique(sentspp$SYMBOL), unique(ramspp$PLANTS_Code)) # 30 species on epa list on in ram data
setdiff(unique(sentspp$SYMBOL), unique(spplist$PLANTS_Code)) # 15 on epa list not on ram tlu_plants; mostly synonyms

epaspp <- read.csv("./data/COCs/Full_COC_List_Final.csv") |>
  filter(COC_reg == "82") |>
  select(SYMBOL, NWCA_NAME, CVal:Updated)

spplist_check <- left_join(spplist, epaspp, by = c("PLANTS_Code" = "SYMBOL")) |>
  filter(PLANTS_Code %in% c(ramspp$PLANTS_Code, sentspp$SYMBOL)) |>
  arrange(Latin_Name) |>
  mutate(COC_diff = abs(CoC_ME_ACAD - as.numeric(CVAL_final))) |>
  filter(COC_diff > 0)

spplist_check # no species differences in COCs between NETN database and EPA CoC list

write.csv(vmmi, "./results/Vegetation_MMI_2011-2021_ACAD_RAM.csv", row.names = F)
write.csv(vmmi_comb, "./results/Vegetation_MMI_2011-2021_ACAD_RAM_SENT_GRME.csv", row.names = F)

# Coefficient of Wetness
# RAM
spp_ram <- left_join(VIEWS_RAM$species_list,
                     VIEWS_RAM$tlu_Plant[,c("TSN", "Coef_wetness")],
                     by = "TSN")

cow_ram <- spp_ram |> group_by(Code, Year) |>
  summarize(mean_wet = mean(Coef_wetness, na.rm = T),
            .groups = "drop")

# Great Meadow
cow_grme <- read.csv("./data/grme/ACAD_Wetland_Species_List_20260126.csv") |>
  group_by(Code, Year) |>
  summarize(mean_wet = mean(Coef_wetness, na.rm = T),
            .groups = "drop")

# SENT
spp_sent <- sent_veg |>
  filter(UID %in% unique(vmmi_sent1$UID)) |>
  left_join(vmmi_sent2 |> select(Code = local_code, UID) |> distinct(),
            by = "UID") |>
  mutate(COW = case_when(WIS == "UPL" ~ 5,
                         WIS == "FACU" ~ 3,
                         WIS == "FAC" ~ 0,
                         WIS == "FACW" ~ -3,
                         WIS == "OBL" ~ -5,
                         TRUE ~ NA_real_)) |>
  select(SYMBOL, SPECIES_NAME_ID, NWCA_NAME, UID, UNIQUE_ID, Code, YEAR, COVER, COW, PLOT)

spp_sent2 <- spp_sent |> group_by(SYMBOL, SPECIES_NAME_ID, NWCA_NAME, UID, UNIQUE_ID, Code, YEAR, COW) |>
  summarize(sum_cov = sum(COVER),
            mean_cov = sum_cov/5,
            .groups = "drop")

cow_sent <- spp_sent2 |> group_by(Code, Year = YEAR) |>
  summarize(mean_wet = mean(COW, na.rm = T),
            .groups = 'drop')

cow_comb <- rbind(cow_ram, cow_grme, cow_sent)

vmmi_cow_comb <- left_join(vmmi_comb, cow_comb, by = c("Code", "Year")) |>
  mutate(site_type = case_when(grepl("R-", Code) ~ "ACAD RAM",
                               grepl("GRME0|GRME10", Code) ~ "ACAD GRME",
                               Code %in% c("BIGH", "DUCK", "FRAZ", "GILM", "GRME", "HEBR",
                                           "HODG", "LITH", "NEMI", "WMTN") ~ "ACAD Sent.",
                               grepl("GIME", Code) ~ "ACAD GILM",
                               TRUE ~ "UNK"))
head(vmmi_cow_comb)

write.csv(vmmi_cow_comb, "./results/Vegetation_MMI_COW_2011-2021_ACAD_RAM_SENT_GRME.csv", row.names = F)
vmmi_sf <- st_as_sf(vmmi_cow_comb, coords = c("X", "Y"), crs = 26919)
st_write(vmmi_sf, "./results/Vegetation_MMI_COW_2011-2021_ACAD_RAM_SENT_GRME.shp")


#+++++ ENDED HERE +++++
# Stressors
locev <- left_join(VIEWS_RAM$locations |> select(Code, FWS_Class_Code, HGM_Class, HGM_Sub_Class),
                   VIEWS_RAM$visits |> select(Code, Year, Visit_Type, Buffer_Width_Avg, Buffer_Perim_Percent),
                   by = "Code") |>
  filter(Visit_Type == "VS")

# Need to adjust stressors a bit based on how we use them.
# -- 1. Use presence of invasives from visits for invasive stressor in AA and
#       drop the one from the BZ which we don't consistently assess.
# -- 2. Clean up deer impacts. There are 2 places they show up and we haven't
#       been consistent on when we use either or across years.

visits_inv <- VIEWS_RAM$visits |>
  filter(Visit_Type == "VS") |>
  filter(Invasive_Cover > 0) |>
  mutate(Location_Level = "AA",
         Stressor_Category = "Invasive Vegetation",
         Stressor = "Cover of non-native or invasive species",
         Severity_Indiv = case_when(Invasive_Cover < 1 ~ 1,
                                    between(Invasive_Cover, 1, 5) ~ 2,
                                    TRUE ~ 3)) |>
  select(Code, Year, Location_Level, Stressor_Category, Stressor, Severity_Indiv)

stress1 <- VIEWS_RAM$RAM_stressors |>
  filter(Visit_Type == "VS") |>
  select(Code, Year, Location_Level, Stressor_Category, Stressor, Severity_Indiv) |>
  filter(!(Location_Level == "BZ" & Stressor == "Cover of non-native or invasive species"))

browse <- stress1 |> filter(Stressor == "Grazing by native ungulates" |
                            Stressor_Category == "Excessive Grazing or Herbivory" |
                            Stressor == "Animal Trampling") |>
          mutate(Location_Level = "AA",
                 Stressor_Category = "Deer Browse Impacts",
                 Stressor = "Excessive Grazing and/or trampling by native ungulates") |>
  select(Code, Year, Location_Level, Stressor_Category, Stressor, Severity_Indiv) |>
  group_by(Code, Year, Location_Level, Stressor_Category, Stressor) |>
  summarize(Severity_Indiv = max(Severity_Indiv, na.rm = T), .groups = 'drop')

stress_bind <- stress1 |> filter(!(Stressor %in% c("Grazing by native ungulates",
                                                   "Animal Trampling"))) |>
  filter(!(Stressor_Category == "Excessive Grazing or Herbivory"))


# Simplify hydrological alteration to either be related to pipes, ditching/channelization, or dams/berm
# Simplify roads to paved, gravel, vs trail. by buffer vs AA.
roads <- stress_bind |> filter(grepl("road|Road", Stressor))

stress_pre <- rbind(stress_bind, visits_inv, browse)

# turn stressor into binary
stress <- left_join(locev,
                    stress_pre,
                    by = c("Code", "Year")) |>
mutate(stress = ifelse(!is.na(Severity_Indiv), 1, 0))

table(stress$Severity_Indiv, stress$stress, useNA = 'always')

stress_wide <- stress |> group_by(Code, HGM_Class, Year, Location_Level) |>
  summarize(num_stressors = sum(stress),
            .groups = 'drop') |>
  pivot_wider(names_from = Location_Level, values_from = num_stressors, values_fill = 0) |>
  select(Code, HGM_Class, Year, AA, BUFF = BZ) |>
  mutate(site_type = "ACAD RAM")

head(stress_wide)

