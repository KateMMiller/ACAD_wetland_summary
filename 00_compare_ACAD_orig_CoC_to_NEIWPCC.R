library(tidyverse)
library(wetlandACAD)
library(readxl)
importRAM(export_protected = T)

tluspp <- VIEWS_RAM$tlu_Plant |> select(Latin_Name, PLANTS_Code, Rank_Name, TSN, CoC_ME_ACAD)
head(tluspp)

regc <- read_xlsx("./data/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx", sheet = 2) |>
  select(Symbol = `Accepted Symbol`, Accepted_Name = `Accepted Name`, cval82 = `82`)

coc_2011_list <- read_xlsx("C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Freshwater_Wetland/EPA_NWCA/2011_Data/NWCA_NCE_PLANT_LIST_COC.xlsx")
head(coc_2011_list)

spp_comb <- left_join(tluspp, regc, by = c("PLANTS_Code" = "Symbol")) |>
  mutate(C_diff = CoC_ME_ACAD - cval82,
         NWCA_NAME = toupper(Latin_Name)) |>
  filter(!is.na())

spp_comb2 <- full_join(spp_comb, coc_2011_list |> select(USDA_NAME, ME),
                       by = c("NWCA_NAME" = "USDA_NAME"))
d
