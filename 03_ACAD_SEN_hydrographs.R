library(tidyverse)

# read in data
path_netn = "C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Analysis/FINAL_DATA/"
wl_sen <- read.csv(paste0(path_netn, "well_prec_data_2013-2025.csv"))

# set up plotting function
wl_bands <- function(site_code, wet_title = NA){

  df <- wl_sen[, c("doy", "Year", site_code)]
  colnames(df) <- c("doy", "Year", "site")

  wl_sum <- df |> filter(Year >= 2016) |>
    summarize(num_samps = sum(!is.na(site)),
              median_wl = median(site, na.rm = T),
              min_wl = min(site, na.rm = T),
              max_wl = max(site, na.rm = T),
              lower95 = quantile(site, 0.025, na.rm = T),
              upper95 = quantile(site, 0.975, na.rm = T),
              lower50 = quantile(site, 0.25, na.rm = T),
              upper50 = quantile(site, 0.75, na.rm = T),
              .by = c(doy)
  )

  ggplot(data = wl_sum) + theme_wet() +
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
    labs(y = "Water Level (cm)", x = NULL, title = wet_title) +
  #  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90, 120), limits = c(-65, 125)) +
    scale_x_continuous(breaks = c(135, 166, 196, 227, 258, 288),
                       labels = c("May-15", "Jun-15", "Jul-15",
                                "Aug-15", "Sep-15", "Oct-15"),
                       guide = guide_axis(minor.ticks = T)) +
    geom_hline(yintercept = 0, color = 'black') +
    theme(title = element_text(size = 9), legend.position = 'bottom') +
    guides(fill = guide_legend(nrow = 2, byrow = T),
           color = guide_legend(nrow = 2, byrow = T))
}

wl_bands("BIGH_WL", "Big Heath") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("DUCK_WL", "Duck Pond") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("GILM_WL", "Gilmore Meadow") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("HEBR_WL", "Heath Brook") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("HODG_WL", "Hodgdon Swamp") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("LIHU_WL", "Little Hunter's Brook") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("NEMI_WL", "New Mills Meadow - NW") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
wl_bands("WMTN_WL", "Western Mtn. Swamp") + scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), limits = c(-65, 65))
