
library(tidyverse)
library(patchwork)
library(wetlandACAD)

theme_wet <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        size = 0.4), plot.background = element_blank(), strip.background = element_rect(color = "#696969",
                                                                                                                        fill = "grey90", size = 0.4), legend.key = element_blank(),
        axis.line.x = element_line(color = "#696969", size = 0.4),
        axis.line.y = element_line(color = "#696969", size = 0.4),
        axis.ticks = element_line(color = "#696969", size = 0.4))
}

path_gm = "C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Freshwater_Wetland/Cromwell_Brk_Data/Hydrology_Data/"
path_netn = "C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Analysis/FINAL_DATA/"

wl_gilm <- read.csv(paste0(path_netn, "well_prec_data_2013-2025.csv"))
wl_gilm$year_fac <- as.factor(wl_gilm$Year)
wl_gilm <- wl_gilm |> mutate(timestamp2 = ifelse(hr == 0,
                                                 format(as.POSIXct(paste0(timestamp, " 00:00:00"),
                                                                   format = "%Y-%m-%d %H:%M:%S",
                                                                   tz = "America/New_York"),
                                                        "%Y-%m-%d %H:%M:%S"),
                                                 format(as.POSIXct(timestamp,
                                                                   format = "%Y-%m-%d %H:%M:%S",
                                                                   tz = "America/New_York"),
                                                        "%Y-%m-%d %H:%M:%S")),
                             Date = as.Date(Date, format = "%Y-%m-%d")) |>
  select(timestamp = timestamp2, Date:year_fac)

head(wl_gilm)

gilm_sum <- wl_gilm |> filter(Year >= 2016) |>
  summarize(num_samps = sum(!is.na(GILM_WL)),
            median_wl = median(GILM_WL, na.rm = T),
            min_wl = min(GILM_WL, na.rm = T),
            max_wl = max(GILM_WL, na.rm = T),
            lower95 = quantile(GILM_WL, 0.025, na.rm = T),
            upper95 = quantile(GILM_WL, 0.975, na.rm = T),
            lower50 = quantile(GILM_WL, 0.25, na.rm = T),
            upper50 = quantile(GILM_WL, 0.75, na.rm = T),
    .by = c(doy)
  )

head(gilm_sum)

wl_grme1 <- read.csv(paste0(path_gm, "great_meadow_well_data_2025_20260304.csv"))

wl_grme <- wl_grme1 |>
  mutate(year_fac = as.factor(year),
         Date = as.Date(date, format = "%Y-%m-%d"),
         timestamp = ifelse(hr == 0,
                             format(as.POSIXct(paste0(timestamp, " 00:00:00"),
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = "America/New_York"),
                                    "%Y-%m-%d %H:%M:%S"),
                             format(as.POSIXct(timestamp,
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = "America/New_York"),
                                    "%Y-%m-%d %H:%M:%S"))) |>
  select(timestamp, date, doy, Year = year, precip_cm = precip.cm, plot.num,
         water.depth, lag.precip, hr, doy_h, year_fac)  |>
  filter(Year >= 2016) |> filter(doy > 134 & doy < 275) |>
  filter(water.depth <200 & water.depth > -200) |> # some rogue data points in there
  filter(!(water.depth < -120 & doy == 159 & Year == 2016 & plot.num == 4)) |>
  filter(!(water.depth < -115 & doy == 215 & Year == 2017 & plot.num == 6))

grme_sum <- wl_grme |>
  filter(!(water.depth < -120 & doy == 159 & Year == 2016)) |>
  summarize(num_samps = sum(!is.na(water.depth)),
            median_wl = median(water.depth, na.rm = T),
            min_wl = min(water.depth, na.rm = T),
            max_wl = max(water.depth, na.rm = T),
            lower95 = quantile(water.depth, 0.025, na.rm = T),
            upper95 = quantile(water.depth, 0.975, na.rm = T),
            lower50 = quantile(water.depth, 0.25, na.rm = T),
            upper50 = quantile(water.depth, 0.75, na.rm = T),
            .by = c(doy, plot.num)
  )

d100 = "#E4F0F8"
d95 = "#B8D8ED"
d50 = "#7FB9DD"
med = "#1378b5"

p_grme <-
ggplot(data = grme_sum |> filter(plot.num == 1)) + theme_wet() +
  geom_ribbon(aes(ymin = min_wl, ymax = max_wl, x = doy,
                  color = "Historic range", fill = "Historic range")) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, x = doy,
                  color = "Hist. 95% range", fill = "Hist. 95% range")) +
  geom_ribbon(aes(ymin = lower50, ymax = upper50, x = doy,
                  color = "Hist. 50% range", fill = "Hist. 50% range")) +
  geom_line(aes(x = doy, y = median_wl, color = "Median water level",
                fill = "Median water level"), linewidth = 1) +
  scale_color_manual(values = c("Historic range" = d100,
                                "Hist. 95% range" = d95,
                                "Hist. 50% range" = d50,
                                "Median water level" = med), name = "Daily Distributions") +
  scale_fill_manual(values = c("Historic range" = d100,
                               "Hist. 95% range" = d95,
                               "Hist. 50% range" = d50,
                               "Median water level" = med), name = "Daily Distributions") +
  labs(y = "Water Level (cm)", x = NULL, title = "Great Meadow") +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                     labels = c("May-15", "Jun-15", "Jul-15",
                                "Aug-15", "Sep-15", "Oct-15"),
                     guide = guide_axis(minor.ticks = T)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme(title = element_text(size = 9), legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2, byrow = T),
         color = guide_legend(nrow = 2, byrow = T))


p_gilm <-
ggplot(data = gilm_sum) + theme_wet() +
  geom_ribbon(aes(ymin = min_wl, ymax = max_wl, x = doy,
                  color = "Historic range", fill = "Historic range")) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, x = doy,
                  color = "Hist. 95% range", fill = "Hist. 95% range")) +
  geom_ribbon(aes(ymin = lower50, ymax = upper50, x = doy,
                  color = "Hist. 50% range", fill = "Hist. 50% range")) +
  geom_line(aes(x = doy, y = median_wl, color = "Median water level",
                fill = "Median water level"), linewidth = 1) +
  scale_color_manual(values = c("Historic range" = d100,
                                "Hist. 95% range" = d95,
                                "Hist. 50% range" = d50,
                                "Median water level" = med), name = "Daily Distributions") +
  scale_fill_manual(values = c("Historic range" = d100,
                                "Hist. 95% range" = d95,
                                "Hist. 50% range" = d50,
                                "Median water level" = med), name = "Daily Distributions") +
  labs(y = "Water Level (cm)", x = NULL, title = "Gilmore Meadow") +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                     labels = c("May-15", "Jun-15", "Jul-15",
                                "Aug-15", "Sep-15", "Oct-15"),
                     guide = guide_axis(minor.ticks = T)) +
  geom_hline(yintercept = 0, color = 'black') +
  theme(title = element_text(size = 9), legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2, byrow = T),
         color = guide_legend(nrow = 2, byrow = T))

p_grme + p_gilm + plot_layout(axes = "collect", guides = 'collect') & theme(legend.position = 'bottom')

ggsave("./results/Great_vs_Gilmore_water_level_distributions.png", width = 10, height = 6)
# Individual years
year_pal = c("2020" = "#8754C7", "2021" = "#0027E3", "2022" = "#54AAC7",
             "2023" = "#C9C94F", "2024" = "#F79240", "2025" = "#AD050B")

p_grme <-
ggplot(wl_grme |> filter(Year > 2019) |> droplevels() |> filter(plot.num == 1),
       aes(x = doy_h, y = water.depth, group = year_fac, color = year_fac)) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = year_pal) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90), limits = c(-65, 95)) +
  geom_hline(yintercept = 0) +
  theme(legend.title = element_blank(), title = element_text(size = 9))+#,
  # axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  labs(y = "Water Level (cm)", x = NULL, title = "Great Meadow") +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                     labels = c("May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15", "Oct-15")) +
 # annotate("text", x = 135, y = 3.5, label = "Surface", color = "black") +
  theme_wet()

# p_grme + p_gilm + plot_layout(guides = "collect", axes = "collect")
#
# ggsave("./results/Great_vs_Gilm_hydrograph.png", width = 9, height = 6)

p_gilm <-
ggplot(wl_gilm |> filter(Year > 2019) |> droplevels(),
       aes(x = doy_h, y = GILM_WL, group = year_fac, color = year_fac)) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = year_pal) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90), limits = c(-65, 95)) +
  geom_hline(yintercept = 0) +
  theme(legend.title = element_blank(), title = element_text(size = 9))+#,
  # axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  labs(y = "Water Level (cm)", x = NULL, title = "Gilmore Meadow") +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                     labels = c("May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15", "Oct-15")) +
 # annotate("text", x = 135, y = 3.5, label = "Surface", color = "black") +
  theme_wet()

p_grme + p_gilm + plot_layout(guides = "collect", axes = "collect")

# ggsave("./results/Great_vs_Gilm_hydrograph.png", width = 9, height = 6)

# Add rug for when drought conditions
drgt <- climateNETN::getClimDrought(park = "ACAD", years = 2020:2025)

drgt <- drgt |>
  filter(County == "Hancock County") |>
  select(DSCI, ValidStart)

new_drgt <- data.frame(Date = seq.Date(min(drgt$ValidStart), max(drgt$ValidStart), 1))

new_drgt2 <- left_join(new_drgt, drgt, by = c("Date" = "ValidStart")) |>
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         month = format(Date, "%d"),
         doy = as.numeric(format(Date, "%j")),
         year = as.numeric(format(Date, "%Y")),
         year_fac = as.factor(year)) |>
  fill(DSCI, .direction = 'down') |>
  mutate(drgt = ifelse(DSCI > 0, 1, NA_real_)) |>
  filter(!is.na(drgt)) |>
  filter(doy >= 135 & doy <= 274)

wl_gilm2 <- left_join(wl_gilm, new_drgt2, by = c("doy", "Year" = "year", "year_fac", "Date"))

# facet by year
ggplot(wl_gilm2 |> filter(Year > 2019) |> droplevels(),
       aes(x = doy_h, y = GILM_WL, group = year_fac)) +
  geom_line(linewidth = 0.75, aes(color = "Gilmore Meadow")) + theme_wet() +
  geom_line(data = wl_grme |> filter(Year > 2019) |> droplevels() |> filter(plot.num == 1),
            linewidth = 0.75,
            aes(x = doy_h, y = water.depth, group = year_fac, color = "Great Meadow")) +
  labs(y = "Water Level (cm)", x = NULL) +
  scale_color_manual(values = c("Gilmore Meadow" = "#0A60D1", "Great Meadow" = "#FFBB14")) +
  scale_x_continuous(breaks = c(135, 166, 196, 227, 258),
                     limits = c(135, 275),
                     labels = c("May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15")) +
  geom_hline(yintercept = 0) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  geom_rug(data = new_drgt2 |> filter(year > 2019), stat = 'identity',
           aes(x = doy, y = drgt, group = year_fac),
           color = "dimgrey", linewidth = 1.5) +
  facet_wrap(~year_fac, ncol = 2)

head(wl_gilm2)

p_grme + p_gilm + plot_layout(guides = "collect", axes = "collect")
ggsave()
