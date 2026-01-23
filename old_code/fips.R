library(tidycensus)
library(tidyverse)
inv_spp1 <- read.csv("./data/taxa_lists/USDA_PLANTS_Invasive_Species_By_State_20251217_clean.csv") |>
  mutate(FIPS = gsub("US", "", FIPS.Code))

data(fips_codes) # from tidycensus
inv_spp <- left_join(inv_spp1, fips_codes |> select(state, state_code) |> unique(),
                     by = c("FIPS" = "state_code"))
head(inv_spp)

# Code ended up not being useful.
