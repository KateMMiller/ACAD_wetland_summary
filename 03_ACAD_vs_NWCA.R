library(tidyverse)

thresh <- c(41.48136, 60.94853)

theme_wet <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#696969", fill = "white",
                                        size = 0.4), plot.background = element_blank(), strip.background = element_rect(color = "#696969",
                                                                                                                        fill = "grey90", size = 0.4), legend.key = element_blank(),
        axis.line.x = element_line(color = "#696969", size = 0.4),
        axis.line.y = element_line(color = "#696969", size = 0.4),
        axis.ticks = element_line(color = "#696969", size = 0.4))
}


vmmi_acad <- read.csv("./results/Vegetation_MMI_2011-2021_ACAD_RAM_SENT_GRME.csv") |>
  mutate(site_type = case_when(grepl("R-", Code) ~ "ACAD RAM",
                               grepl("GRME0|GRME10", Code) ~ "ACAD GRME",
                               Code %in% c("BIGH", "DUCK", "FRAZ", "GILM", "GRME", "HEBR",
                                           "HODG", "LITH", "NEMI", "WMTN") ~ "ACAD Sent.",
                               grepl("GIME", Code) ~ "ACAD GILM",
                               TRUE ~ "UNK"
         )) |>
  select(Code, Year, vmmi, vmmi_rating, site_type)

vmmi_grme <- vmmi_acad |> filter(site_type == "ACAD GRME") |> filter(Year == 2025)

head(vmmi_grme)

nwca_prob1 <- read.csv("./results/Vegetation_MMI_2011-2021_EPA_PROB.csv")

table(nwca_prob1$US_L3CODE)

nwca_prob <- nwca_prob1 |>
  filter(YEAR %in% c(2021, 2022)) |>
  select(Code = SITE_ID, Year = YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "EPA Prob.")

table(nwca_prob$Year)

nwca_ref <- read.csv('./results/Vegetation_MMI_2011-2021_EPA_allsites.csv') |>
  filter(site_type_fac == "REF") |>
  select(Code = SITE_ID, Year = YEAR, vmmi, vmmi_rating) |>
  mutate(site_type = "EPA Ref.") # ACAD Sent. not included


vmmi_comb <- rbind(vmmi_acad |> filter(site_type %in% c("ACAD RAM", "ACAD Sent.")),
                   vmmi_grme, nwca_prob, nwca_ref)

vmmi_comb$site_type_fac <- factor(vmmi_comb$site_type,
                                  levels = c("EPA Ref.", "EPA Prob.", "ACAD Sent.", "ACAD RAM",
                                             "ACAD GRME"))#, "ACAD GILM"))

# Add great meadow sites here, after updating the thresholds for ratings.
ggplot(vmmi_comb,
       aes(x = site_type_fac, y = vmmi)) +
  geom_boxplot() + theme_wet() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "Veg. MMI") +
  geom_hline(yintercept = thresh[1], linewidth = 0.5, color = "#696969",
             linetype = 'dashed') +
  geom_hline(yintercept = thresh[2], linewidth = 0.75, color = "#696969") +
  labs(y = "Vegetation MMI", x = NULL)

ggsave("./results/VMMI_distribution_site_type_ACAD_EPA.png", height = 4, width = 6)
