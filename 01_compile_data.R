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
