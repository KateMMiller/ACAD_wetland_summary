library(tidyverse)

vmmi_comb <- read.csv("./results/Vegetation_MMI_COW_2011-2021_ACAD_RAM_SENT.csv") |>
  mutate(site_type = ifelse(Panel == 0, "SENT", "RAM"))
vmmi_ram <- vmmi_comb |> filter(grepl("R-", Code))

thresh <- c(41.48136, 60.94853)

ggplot(vmmi_comb, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 20, ymax = 41.48136), fill = "#CC6666") +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 41.48136, ymax = 60.94853), fill = "#FFF394") +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 60.94853, ymax = 100), fill = "#88CF89") +
  geom_point() + geom_line() + facet_wrap(~Code) +
  labs(y = "Vegetation MMI")

ggplot(vmmi_ram, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 20, ymax = 41.48136),fill = "#CC6666", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 41.48136, ymax = 60.94853), fill = "#FFF394", alpha = 0.2) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 60.94853, ymax = 100), fill = "#88CF89", alpha = 0.2) +
  geom_point() + geom_line() + facet_wrap(~HGM_Class)

ggplot(vmmi_ram, aes(x = Year, y = vmmi, group = Code)) +
  theme_bw() +
  ylim(20, 100) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 20, ymax = 41.48136), fill = "#CC6666", alpha = 0.6) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 41.48136, ymax = 60.94853), fill = "#FFF394", alpha = 0.6) +
  geom_rect(aes(xmin = 2012, xmax = 2025, ymin = 60.94853, ymax = 100), fill = "#88CF89", alpha = 0.6) +
  geom_point() + geom_line() + facet_wrap(~FWS_Class_Code)

# facet on site type
ggplot(vmmi_comb, aes(x = Year, y = meanC, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F) +
  facet_wrap(~site_type)

ggplot(vmmi_comb, aes(x = Year, y = Bryophyte_Cover, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F) +
  facet_wrap(~site_type)

# Inv. cover acts weird bc so low everywhere
ggplot(vmmi_comb, aes(x = Year, y = Invasive_Cover, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(~site_type)

ggplot(vmmi_comb, aes(x = Year, y = Cover_Tolerant, group = Code)) +
  theme_bw() + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(~site_type)

ggplot(vmmi_comb, aes(x = Year, y = mean_wet, group = Code)) +
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

ggplot(vmmi_comb, aes(x = Year, y = mean_wet)) +
  theme_bw() + labs(y = "Mean Wetness") +
  ylim(-5, 1) +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = -5, ymax = -3),
            fill = "dodgerblue4", alpha = 0.2) +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = -3, ymax = 0),
            fill = "dodgerblue2", alpha = 0.2) +
  geom_rect(aes(xmin = 2011, xmax = 2025, ymin = 0, ymax = 1),
            fill = "forestgreen", alpha = 0.2) +
  geom_point() + geom_smooth(se = F) +
  facet_wrap(~site_type)


# Coef of Wetness
# -5 Obligate wetland
# -3 Facultative wetland
# 0 Facultative
# 3 Facultative upland
# 5 Obligate upland


# Trend analysis
vmmi_ram$year_cen <- vmmi_ram$Year - min(vmmi_ram$Year)
vmmi_ram$year_fac <- as.factor(vmmi_ram$year_cen)

# VMMI Trends
vmmimod_full <- lmer(vmmi ~ year_cen + HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod3 <- lmer(vmmi ~ HGM_Class + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod2 <- lmer(vmmi ~ year_cen + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)
vmmimod1 <- lmer(vmmi ~ 1 + (1 + year_cen|Code) + (1|year_fac), data = vmmi_ram)

anova(vmmimod_full, vmmimod3, vmmimod2, vmmimod1)
plot(vmmimod3)
qqnorm(residuals(vmmimod3))
summary(vmmimod3)

# Mean C trends
meancmod_full <- lmer(meanC ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod3 <- lmer(meanC ~ HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
meancmod2 <- lmer(meanC ~ year_cen + (1 + year_cen|Code), data = vmmi_ram)
meancmod1 <- lmer(meanC ~ 1 + (1 + year_cen|Code), data = vmmi_ram)

anova(meancmod_full, meancmod3, meancmod2, meancmod1)
plot(meancmod3)
qqnorm(residuals(meancmod3))
summary(meancmod3)

# % Bryophyte Trends
bryomod_full <- lmer(Bryophyte_Cover ~ year_cen + HGM_Class + (1|Code), data = vmmi_ram)
bryomod3 <- lmer(Bryophyte_Cover ~ HGM_Class + (1|Code), data = vmmi_ram) # random slopes failed to converge, so rand. int.
bryomod2 <- lmer(Bryophyte_Cover ~ year_cen + (1|Code), data = vmmi_ram)
bryomod1 <- lmer(Bryophyte_Cover ~ 1 + (1|Code), data = vmmi_ram)

# Bryo3 didn't converge. Diagnostics aren't very good. Bryo is kind of a weird metric
anova(bryomod_full, bryomod3, bryomod2, bryomod1)
plot(bryomod3)
qqnorm(residuals(bryomod3))
summary(bryomod3)

# % Tolerant Trends
tolmod_full <- lmer(Cover_Tolerant ~ year_cen + HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod3 <- lmer(Cover_Tolerant ~ HGM_Class + (1 + year_cen|Code), data = vmmi_ram)
tolmod2 <- lmer(Cover_Tolerant ~ year_cen + (1 + year_cen|Code), data = vmmi_ram)
tolmod1 <- lmer(Cover_Tolerant ~ 1 + (1 + year_cen|Code), data = vmmi_ram)

anova(tolmod_full, tolmod3, tolmod2, tolmod1)
plot(tolmod3)
qqnorm(residuals(tolmod3))
summary(tolmod3)

