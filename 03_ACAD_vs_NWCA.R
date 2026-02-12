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


vmmi_acad <- read.csv("./results/Vegetation_MMI_2011-2025_ACAD_RAM_SENT_GRME.csv") |>
  mutate(site_type = case_when(grepl("R-", Code) ~ "ACAD RAM",
                               grepl("GRME0|GRME10", Code) ~ "ACAD GRME",
                               Code %in% c("BIGH", "DUCK", "FRAZ", "GILM", "GRME", "HEBR",
                                           "HODG", "LITH", "NEMI", "WMTN") ~ "ACAD Sent.",
                               grepl("GIME", Code) ~ "ACAD GILM",
                               TRUE ~ "UNK"
         )) |>
  select(Code, Year, vmmi, vmmi_rating, site_type)

vmmi_grme <- vmmi_acad |> filter(site_type == "ACAD GRME") |> filter(Year == 2025)
vmmi_sent <- vmmi_acad |> filter(site_type == "ACAD Sent.") |> filter(Year == 2021)
vmmi_ram <- vmmi_acad |> filter(site_type == "ACAD RAM") |> filter(Year > 2021)

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


vmmi_comb <- rbind(vmmi_ram, vmmi_sent,
                   vmmi_grme, nwca_prob, nwca_ref)

vmmi_comb$site_type_fac <- factor(vmmi_comb$site_type,
                                  levels = c("EPA Ref.", "EPA Prob.", "ACAD Sent.", "ACAD RAM",
                                             "ACAD GRME"))#, "ACAD GILM"))

# Add great meadow sites here, after updating the thresholds for ratings.
ggplot(vmmi_comb,
       aes(x = site_type_fac, y = vmmi)) +
  geom_boxplot() + theme_wet() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "Vegetation MMI") +
  geom_hline(yintercept = thresh[1], linewidth = 0.5, color = "#696969",
             linetype = 'dashed') +
  geom_hline(yintercept = thresh[2], linewidth = 0.75, color = "#696969")

ggsave("./results/VMMI_distribution_site_type_ACAD_EPA.png", height = 4, width = 6)

# Plot stressors
#+ Add REF data into this, if I can
stress_all1 <- read.csv("./results/Stressor_Counts_NWCAPROB_ACAD_GRME_most_recent.csv")
stress_ref <- read.csv("./results/Stressor_Counts_REF_2011-2021.csv") |>
  mutate(site_type = "EPA Ref.") |>
  select(Code = SITE_ID, Year = YEAR, site_type, AA, BUFF)

stress_all <- rbind(stress_all1, stress_ref)

stress_all$site_type_fac <- factor(stress_all$site_type,
                                  levels = c("EPA Ref.", "EPA Prob.", "ACAD Sent.", "ACAD RAM",
                                             "ACAD GRME"))#, "ACAD GILM"))

stress_all <- stress_all |>
  mutate(Code = case_when(grepl("ME-HP301", Code) ~ "DUCK",
                          grepl("ME-HP302", Code) ~ "WMTN",
                          grepl("ME-HP303", Code) ~ "BIGH",
                          grepl("ME-HP304", Code) ~ "GILM",
                          grepl("ME-HP305", Code) ~ "LITH",
                          grepl("ME-HP306", Code) ~ "NEMI",
                          grepl("ME-HP307", Code) ~ "GRME",
                          grepl("ME-HP308", Code) ~ "HEBR",
                          grepl("ME-HP309", Code) ~ "HODG",
                          grepl("ME-HP310", Code) ~ "FRAZ",
                          TRUE ~ Code))

write.csv(stress_all, "./results/Stressor_Counts_NWCAPROB_ACAD_GRME_most_recent_REF.csv",
          row.names = F)

head(stress_all)

stress_long <- stress_all |> pivot_longer(cols = c(AA, BUFF), names_to = "loc", values_to = "num_stress")

ggplot(stress_long, aes(x = site_type_fac, y = num_stress)) +
  geom_boxplot() + theme_wet() +
  geom_jitter(alpha = 0.2) +
  labs(x = "Site Disturbance Type", y = "# Stressors") +
  facet_wrap(~loc)

ggsave("./results/Stressor_boxplots.png", width = 8, height = 5)



