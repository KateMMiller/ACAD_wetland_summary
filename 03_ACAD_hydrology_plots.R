
library(tidyverse)
library(patchwork)

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
summary(wl_grme)

wl_grme <- read.csv(paste0(path_gm, "great_meadow_well_data_2025_20260304.csv")) |>
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
  filter(Year > 2015) |> filter(doy > 134 & doy < 275)

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

p_grme + p_gilm + plot_layout(guides = "collect", axes = "collect")

ggsave("./results/Great_vs_Gilm_hydrograph.png", width = 9, height = 6)

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
