#---------------------------------------------------------------------
# Compiling NETN and ACAD hydro data from difference sources/formats
#---------------------------------------------------------------------

library(tidyverse)
library(readxl)

xlpath <- "C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Freshwater_Wetland/Cromwell_Brk_Data/Hydrology_Data/"
gm25a <- read_xlsx(paste0(xlpath,
                  "GREAT MEADOW Full Data & New Graphs 2025 October.xlsx"),
                   sheet = "Full Data to Oct 2025") |> data.frame()
head(gm25a)

gm25 <- gm25a |> select(
  ID,
  time1 = Date.Time..GMT.04.00,
  time2 = TimeStamp..TEXT.K29199..m.dd.yy.......TEXT.L29199..hh.mm....for.lookup.for.daily.precip,
  GRME_1 = Plot1.FINAL.Corrected.Logger.Depth,
  GRME_2 = Plot2.Filtered.Corrected...Corrected.Logger.Depth,
  GRME_3 = Plot3.Filtered.Corrected...Corrected.Logger.Depth,
  GRME_4 = Plot4.Filtered.Corrected...Corrected.Logger.Depth,
  GRME_5 = Plot5.Filtered.Corrected...Corrected.Logger.Depth,
  GRME_6 = Plot6.Filtered.Corrected.Logger.Depth) |>
  mutate(timestamp = as.POSIXct(time1, format = "%Y-%m-%d %H:%M:%S") + 1,
         Date = as.Date(timestamp),
         hour = format(timestamp, "%H"),
         month = format(timestamp, "%d"),
         doy = format(timestamp, "%j"),
         doy_h = paste0(doy, ".", hour),
         year = as.numeric(format(timestamp, "%Y")),
         GRME_1 = GRME_1 * 100,
         GRME_2 = GRME_2 * 100,
         GRME_3 = GRME_3 * 100,
         GRME_4 = GRME_4 * 100,
         GRME_5 = GRME_5 * 100,
         GRME_6 = GRME_6 * 100) |>
  filter(year > 2024) |>
  select(ID, timestamp, Date, year, hour, month, doy, doy_h, GRME_1:GRME_6)

head(gm25)
str(gm25)

library(wetlandACAD)
prec <- get_NADP_precip(start_date = paste0('01/01/2025'),
                        end_date = paste0('10/17/2025'),
                        stationID = "ME98", quietly = FALSE) |>  # download precip data
        select(timestamp, precip.cm = precip_cm)

prec <- prec |> mutate(Date = as.Date(timestamp),
                       hour = format(timestamp, "%H"),
                       month = format(timestamp, "%d"),
                       doy = format(timestamp, "%j"),
                       doy_h = paste0(doy, ".", hour),
                       year = as.numeric(format(timestamp, "%Y")))

gm_prec25 <- left_join(gm25,
                       prec |> select(year, month, doy, hour, precip.cm),
                       by = c("year", "month", "doy", "hour")) |>
  filter(doy > 134 & doy < 275) |>
  arrange(timestamp) |> data.frame() |>
  select(timestamp, Date, year, GRME_1:GRME_6, precip.cm, doy, hr = hour, doy_h)

head(gm_prec25)

gm15_24 <- read.csv(paste0(xlpath, "great_meadow_well_data_2015-2024/",
                           "great_meadow_well_data_2024_20250715.csv"))
head(gm15_24)

gm15_24 <- gm15_24 |>
  mutate(timestamp2 = ifelse(hr == 0,
                             format(as.POSIXct(paste0(timestamp, " 00:00:00"),
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = "America/New_York"),
                                    "%Y-%m-%d %H:%M:%S"),
                             format(as.POSIXct(timestamp,
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "America/New_York"),
                                    "%Y-%m-%d %H:%M:%S"))) |>
  select(timestamp = timestamp2, date, year, plot.num, precip.cm, doy, hr, doy_h,
         water.depth)

gm15_24_wide <- gm15_24 |> pivot_wider(names_from = plot.num, values_from = water.depth,
                                       names_prefix = "GRME_") |>
  mutate(Date = as.Date(date, format = "%Y-%m-%d")) |>
  select(timestamp, Date, year, GRME_1:GRME_6, precip.cm, doy, hr, doy_h) |>
  filter(Date < as.Date("2025-05-15", format = "%Y-%m-%d"))

gm15_25 <- rbind(gm15_24_wide, gm_prec25)

write.csv(gm15_25, paste0(xlpath, "great_meadow_2015_2025.csv"), row.names = F)
