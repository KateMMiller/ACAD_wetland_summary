#-------------------------------------------------------------------
# Compiling RAM data for freshwater wetland trend analysis in ACAD
#-------------------------------------------------------------------

library(tidyverse)
library(wetlandACAD)
library(lme4)
library(sf)

# RAM data
importRAM(export_protected = T)

vmmi1 <- sumVegMMI()
loc <- VIEWS_RAM$locations
vmmi <- left_join(vmmi1,
                  loc |> select(Code, FWS_Class_Code, HGM_Class, HGM_Sub_Class),
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

head(vmmi_sent2)

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
vmmi_comb <- rbind(vmmi, vmmi_sent3)

# Check that COCs match between NETN and EPA analyses
# Only interested in species that have been found in ACAD.
ramspp <- VIEWS_RAM$species_list |> select(Latin_Name, PLANTS_Code, CoC_ME_ACAD) |> distinct()
spplist <- VIEWS_RAM$tlu_Plant |> select(Latin_Name, PLANTS_Code, CoC_ME_ACAD)

sentspp <- read.csv("./data/comb_data/Plant_Cover_2011-2021.csv") |>
  filter(UID %in% unique(vmmi_sent1$UID)) |>
  select(SYMBOL, NWCA_NAME, CVAL_final, NWCA_CVAL, CVal, Updated) |> distinct()
head(sentspp)
head(ramspp)
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
write.csv(vmmi_comb, "./results/Vegetation_MMI_2011-2021_ACAD_RAM_SENT.csv", row.names = F)
head(vmmi_comb)

ggplot(vmmi, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 20, ymax = 41.48136),
            fill = "#CC6666", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 41.48136, ymax = 60.94853),
            fill = "#FFF394", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 60.94853, ymax = 100),
            fill = "#88CF89", alpha = 0.2) +
  geom_point() + geom_line() + facet_wrap(~HGM_Class)

ggplot(vmmi, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 20, ymax = 41.48136),
            fill = "#CC6666", alpha = 0.6) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 41.48136, ymax = 60.94853),
            fill = "#FFF394", alpha = 0.6) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 60.94853, ymax = 100),
            fill = "#88CF89", alpha = 0.6) +
  geom_point() + geom_line() + facet_wrap(~FWS_Class_Code)

ggplot(vmmi_comb, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 20, ymax = 41.48136),
            fill = "#CC6666") +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 41.48136, ymax = 60.94853),
            fill = "#FFF394") +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 60.94853, ymax = 100),
            fill = "#88CF89") +
  geom_point() + geom_line() + facet_wrap(~Code) +
  labs(y = "Vegetation MMI")

# thresholds
# good > 60.94853
# fair 41.48136 - 60.94853
# poor < 41.48136

ggplot(vmmi, aes(x = Year, y = meanC, group = Code)) +
  theme_bw() + geom_point() + geom_line() + facet_wrap(~Code)

ggplot(vmmi, aes(x = Year, y = Bryophyte_Cover, group = Code)) +
  theme_bw() + geom_point() + geom_line() + facet_wrap(~Code)

ggplot(vmmi, aes(x = Year, y = Invasive_Cover, group = Code)) +
  theme_bw() + geom_point() + geom_line() + facet_wrap(~Code) +
  ylim(0, 40)

ggplot(vmmi, aes(x = Year, y = Cover_Tolerant, group = Code)) +
  theme_bw() + geom_point() + geom_line() + facet_wrap(~Code)

# no facet
ggplot(vmmi, aes(x = Year, y = meanC, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F)

ggplot(vmmi, aes(x = Year, y = Bryophyte_Cover, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F)

# weird bc so low
ggplot(vmmi, aes(x = Year, y = Invasive_Cover, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F)

ggplot(vmmi, aes(x = Year, y = Cover_Tolerant, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F)

spp <- left_join(VIEWS_RAM$species_list,
                 VIEWS_RAM$tlu_Plant[,c("TSN", "Coef_wetness")],
                 by = "TSN")

sum_spp <- spp |> group_by(Code, Year) |>
  summarize(mean_wet = mean(Coef_wetness, na.rm = T),
            num_inv = sum(Invasive, na.rm = T),
            num_exo = sum(Exotic, na.rm = T),
            .groups = "drop")
range(sum_spp$mean_wet)

ggplot(sum_spp, aes(x = Year, y = mean_wet, group = Code)) +
  theme_bw() + labs(y = "Mean Wetness") +
  ylim(-5, 1) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = -5, ymax = -3),
            fill = "dodgerblue4", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = -3, ymax = 0),
            fill = "dodgerblue2", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 0, ymax = 1),
            fill = "forestgreen", alpha = 0.2) +
  geom_point() + geom_line() + facet_wrap(~Code)

table(spp$Coef_wetness)

ggplot(sum_spp, aes(x = Year, y = mean_wet)) +
  theme_bw() + labs(y = "Mean Wetness") +
  ylim(-5, 1) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = -5, ymax = -3),
            fill = "dodgerblue4", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = -3, ymax = 0),
            fill = "dodgerblue2", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 0, ymax = 1),
            fill = "forestgreen", alpha = 0.2) +
  geom_point() + geom_smooth(se = F)


# Coef of Wetness
# -5 Obligate wetland
# -3 Facultative wetland
# 0 Facultative
# 3 Facultative upland
# 5 Obligate upland
head(vmmi)
vmmi$year_cen <- vmmi$Year - min(vmmi$Year)
vmmi$year_fac <- as.factor(vmmi$year_cen)

# Trend analysis

# VMMI Trends
vmmimod_full <- lmer(vmmi ~ year_cen + HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi)
vmmimod3 <- lmer(vmmi ~ HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi)
vmmimod2 <- lmer(vmmi ~ year_cen + (1 + year_cen|Code) + (1|year_fac), data = vmmi)
vmmimod1 <- lmer(vmmi ~ 1 + (1 + year_cen|Code) + (1|year_fac), data = vmmi)

anova(vmmimod_full, vmmimod3, vmmimod2, vmmimod1)
plot(vmmimod3)
qqnorm(residuals(vmmimod3))
summary(vmmimod3)

# Mean C trends
meancmod_full <- lmer(meanC ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi)
meancmod3 <- lmer(meanC ~ HGM_Class + (1 + year_cen|Code), data = vmmi)
meancmod2 <- lmer(meanC ~ year_cen + (1 + year_cen|Code), data = vmmi)
meancmod1 <- lmer(meanC ~ 1 + (1 + year_cen|Code), data = vmmi)

anova(meancmod_full, meancmod3, meancmod2, meancmod1)
plot(meancmod3)
qqnorm(residuals(meancmod3))
summary(meancmod3)

# % Bryophyte Trends
bryomod_full <- lmer(Bryophyte_Cover ~ year_cen + HGM_Class + (1|Code), data = vmmi)
bryomod3 <- lmer(Bryophyte_Cover ~ HGM_Class + (1|Code), data = vmmi) # random slopes failed to converge, so rand. int.
bryomod2 <- lmer(Bryophyte_Cover ~ year_cen + (1|Code), data = vmmi)
bryomod1 <- lmer(Bryophyte_Cover ~ 1 + (1|Code), data = vmmi)

# Bryo3 didn't converge. Diagnostics aren't very good. Bryo is kind of a weird metric
anova(bryomod_full, bryomod3, bryomod2, bryomod1)
plot(bryomod3)
qqnorm(residuals(bryomod3))
summary(bryomod3)

# % Tolerant Trends
tolmod_full <- lmer(Cover_Tolerant ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi)
tolmod3 <- lmer(Cover_Tolerant ~ HGM_Class + (1 + year_cen|Code), data = vmmi)
tolmod2 <- lmer(Cover_Tolerant ~ year_cen + (1 + year_cen|Code), data = vmmi)
tolmod1 <- lmer(Cover_Tolerant ~ 1 + (1 + year_cen|Code), data = vmmi)

anova(tolmod_full, tolmod3, tolmod2, tolmod1)
plot(tolmod3)
qqnorm(residuals(tolmod3))
summary(tolmod3)

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

stress <- left_join(locev,
                    stress_pre,
                    by = c("Code", "Year"))

#+++ ENDED HERE +++
# I don't remember what I was planning to do. Need to inspect stressors in more detail.

table(stress$Stressor_Category, stress$Stressor)
table(stress$Stressor_Category)
head(stress)
table(stress$Location_Level, stress$Stressor_Category)

# Stressors to drop
drop_stress <- c("Shrub layer browsed",  # in buffer zone, not consistently used and hard to assess

                 )
table(stress$Location_Level, stress$stress_cat_simp)

# Browse stressors
# "Excessive Grazing or Herbivory" # in Excessive Grazing or Herbivory usually to plants
# "Grazing by native ungulates" # in Stressors to substrate (usually means deer trails)

browse <- stress |> filter(grepl("wildlife herbivory|native ungulates", Stressor))
table(browse$Code, browse$Stressor)

# Going to combine the two grazing stressors, as I'm not sure they're used consistently
# to distinguish from different things.


# Analysis thoughts
#--- Try to model vmmi and individual metrics using stressors as predictors?
