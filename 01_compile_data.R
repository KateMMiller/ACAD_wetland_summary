#-------------------------------------------------------------------
# Compiling RAM data for freshwater wetland trend analysis in ACAD
#-------------------------------------------------------------------

library(tidyverse)
library(wetlandACAD)
importRAM(export_protected = T)

names(VIEWS_RAM)

vmmi <- sumVegMMI()
View(vmmi)
head(vmmi)
range(vmmi$vmmi)

#vmmi_plot <-
ggplot(vmmi, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 20, ymax = 52.785),
            fill = "indianred", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 52.786, ymax = 65.22746),
            fill = "gold", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 65.22746, ymax = 100),
            fill = "forestgreen", alpha = 0.2) +
  geom_point() + geom_line() + facet_wrap(~Code)

# thresholds
# good > 65.22746
# fair 52.785 - 65.22746
# poor < 52.785

head(VIEWS_RAM$species_list)
head(VIEWS_RAM$tlu_Plant)
View(VIEWS_RAM$tlu_Plant)


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
  geom_point() + geom_line() + facet_wrap(~Code) +
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

# Coef of Wetness
# -5 Obligate wetland
# -3 Facultative wetland
# 0 Facultative
# 3 Facultative upland
# 5 Obligate upland
