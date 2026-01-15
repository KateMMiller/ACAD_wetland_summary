library(tidyverse)

thresh <- c(41.48136, 60.94853)

acad_ref <- read.csv("./results/Vegetation_MMI_2011-2021_ACAD_REF.csv") |>
  select(SITE_ID, YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "ACAD Sent.")

head(acad_ref)

nwca_prob1 <- read.csv("./results/Vegetation_MMI_2011-2021_EPA_PROB.csv")

table(nwca_prob1$US_L3CODE)

nwca_prob <- nwca_prob1 |>
  select(SITE_ID, YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "EPA Prob.")

nwca_ref <- read.csv('./results/Vegetation_MMI_2011-2021_EPA_allsites.csv') |>
  filter(site_type_fac == "REF") |>
  select(SITE_ID, YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "EPA Ref.")

acad_ram <- read.csv("./results/Vegetation_MMI_2011-2021_ACAD_RAM.csv") |>
  select(SITE_ID = Code, YEAR = Year, vmmi, vmmi_rating) |>
  mutate(site_type = "ACAD RAM")

vmmi_comb <- rbind(nwca_ref, nwca_prob, acad_ref, acad_ram)

vmmi_comb$site_type_fac <- factor(vmmi_comb$site_type,
                                  levels = c("EPA Ref.", "EPA Prob.", "ACAD Sent.", "ACAD RAM"))

# Add great meadow sites here, after updating the thresholds for ratings.
ggplot(vmmi_comb, aes(x = site_type_fac, y = vmmi)) +
  geom_boxplot() + theme_classic() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "Veg. MMI") +
  geom_hline(yintercept = thresh[1], linewidth = 0.1, color = "#696969",
             linetype = 'dashed') +
  geom_hline(yintercept = thresh[2], linewidth = 0.05, color = "#696969") +
  labs(y = "Vegetation MMI", x = NULL)


ggsave("./results/VMMI_distribution_site_type_ACAD_EPA.png", height = 8, width = 10)
