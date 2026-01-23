#-------------------------------------------------------------------------
# Download and compile EPA NWCA data for sites in the NCE reporting unit
# Later change to Northeast Level 3 Ecoregion codes 58, 59, 82, 83, 84
# because the Coefficient of Conservation values were assigned during the
# same workshop/same botanists, and most closely resemble the values used
# to develop the vegetation MMI and thresholds. The CoC values in the EPA
# taxa tables change a lot among years, affecting the MMI calculations for
# mathematical and not ecological reasons. All MMI calculations will use
# The NEIWPCC CoCs assigned here:
#   https://neiwpcc.org/wp-content/uploads/2018/03/Northeast-FQA_NEWIPCC_-FINAL-REPORT_March-2018.pdf
#   http://neiwpcc.org/wp-content/uploads/2018/03/Northeast-FQA_NEIWPCC_FINAL-Appendix-6_Ecoregional-C.xlsx
#-------------------------------------------------------------------------

#--- Params ----
library(rvest)
library(stringr)
library(tidyverse)
# library(tidycensus)

download_path <- "C:/NETN/R_Dev/ACAD_wetland_summary/data/epa_all/"

#--- Read html on website ---
page_orig <- "https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys"
page_read <- read_html(page_orig)

#--- Download data files ---
# Detect hyperlinks ending in .csv
csv_list <- page_read |> html_nodes("a") |> html_attr("href") |> str_subset(regex("\\.csv", ignore_case = T))
nwca_data <- str_subset(csv_list, "nwca")
nwca_data_names <- sub(".*\\/", "", nwca_data)

# Download all hyperlinks with nwca and .csv in their name
lapply(seq_along(nwca_data_names), function(x){
  link = nwca_data[x]
  filename = nwca_data_names[x]
  download.file(link, paste0(download_path, filename))
})

#--- Download metadata files ---
txt_list <- page_read |> html_nodes("a") |> html_attr("href") |> str_subset("\\.txt")
nwca_meta <- str_subset(txt_list, "nwca")
nwca_meta_names <- sub(".*\\/", "", nwca_meta)

#dir.create("./data/metadata/") # added folder for metadata
lapply(seq_along(nwca_meta_names), function(x){
  link = nwca_meta[x]
  filename = nwca_meta_names[x]
  download.file(link, paste0(download_path, 'metadata/', filename))
})

#--- Compile plots in NCE, Visit 1, PROB only ---
# 2011 Data
siteinfo_2011 <- read.csv("./data/epa_all/nwca11_siteinfo.csv")
nce_sites_2011 <- siteinfo_2011$UID[siteinfo_2011$RPT_UNIT == "NCE" &
                                    siteinfo_2011$VISIT_NO == 1 ] #&
                                    #siteinfo_2011$SITETYPE == "PROB"] # dropped ACAD sites
nce_sites_2011
csv_list <- list.files(paste0("./data/epa_all/"))
csv2011a <- csv_list[grepl("nwca11|nwca2011", csv_list)]
csv2011 <- csv2011a[!grepl("planttaxa", csv2011a)]
csv2011_name1 <- gsub("nwca11", "nwca2011", csv2011)
csv2011_name <- gsub("nwca2011_siteinfo", "nwca2011_site_info", csv2011_name1)

invisible(
lapply(seq_along(csv2011), function(x){
  cat(csv2011[x], " = ", csv2011_name[x], "\n")
  dat <- read.csv(paste0("./data/epa_all/", csv2011[x]), fileEncoding = "latin1") |>
    dplyr::filter(UID %in% nce_sites_2011)
  write.csv(dat, paste0("./data/epa_nce/", csv2011_name[x]), row.names = F)
})
)

# 2016 Data
siteinfo_2016 <- read.csv("./data/epa_all/nwca-2016-site-information-data_0.csv")
nce_sites_2016 <- siteinfo_2016$UID[siteinfo_2016$RPT_UNIT == "NCE" &
                                      siteinfo_2016$VISIT_NO == 1 &
                                      #siteinfo_2016$SITETYPE == "PROB" &
                                      !is.na(siteinfo_2016$UID)]
nce_sites_2016
csv_list <- list.files(paste0("./data/epa_all/"))
csv2016a <- csv_list[grepl("nwca-2016|nwca_2016|nwca16_", csv_list)]
csv2016 <- csv2016a[!grepl("plant_|condition_estimates|veg_mmi", csv2016a)]
csv2016_name1 <- gsub("nwca16|nwca-2016|nwca_2016", "nwca2016", csv2016)
csv2016_name2 <- gsub("_-_", "_", csv2016_name1)
csv2016_name3 <- gsub("-", "_", csv2016_name2)
csv2016_name4 <- gsub("_csv.", ".", csv2016_name3)
csv2016_name <- gsub("nwca2016_site_information_data_0", "nwca2016_site_info", csv2016_name4)

invisible(
  lapply(seq_along(csv2016), function(x){
    cat(csv2016[x], " = ", csv2016_name[x], "\n")
    dat <- read.csv(paste0("./data/epa_all/", csv2016[x])) |> #, fileEncoding = "latin1") |>
      dplyr::filter(UID %in% nce_sites_2016)
    write.csv(dat, paste0("./data/epa_nce/", csv2016_name[x]), row.names = F)
  })
)

# 2021 Data
siteinfo_2021 <- read.csv("./data/epa_all/nwca21_siteinfo-data.csv")
nce_sites_2021 <- siteinfo_2021$UID[siteinfo_2021$RPT_UNIT == "NCE" &
                                      siteinfo_2021$VISIT_NO == 1 &
                                      #siteinfo_2021$SITETYPE == "PROB" &
                                      !is.na(siteinfo_2021$UID)]
nce_sites_2021
csv_list <- list.files(paste0("./data/epa_all/"))
csv2021a <- csv_list[grepl("nwca-2021|nwca_2021|nwca21_", csv_list)]
csv2021 <- csv2021a[!grepl("plantcval|plantnative|planttaxa|plantwis|condition_estimates|landscape_metrics", csv2021a)]
csv2021_name1 <- gsub("nwca21|nwca-2021|nwca_2021", "nwca2021", csv2021)
csv2021_name2 <- gsub("_-_", "_", csv2021_name1)
csv2021_name3 <- gsub("-", "_", csv2021_name2)
csv2021_name <- gsub("nwca2021_siteinfo_data", "nwca2021_site_info", csv2021_name3)

invisible(
  lapply(seq_along(csv2021), function(x){
    cat(csv2021[x], " = ", csv2021_name[x], "\n")
    dat <- read.csv(paste0("./data/epa_all/", csv2021[x])) |> #, fileEncoding = "latin1") |>
      dplyr::filter(UID %in% nce_sites_2021)
    write.csv(dat, paste0("./data/epa_nce/", csv2021_name[x]), row.names = F)
  })
)
#--- DOWNLOAD NOTES ---
# 2021 Site info includes records for sites that weren't sampled. Have to drop anything where is.na(UID)
# Consider using weights set by EPA for each site (TNT_CAT) for small area estimation approach?
# SITE_ID is the same across years, except the NWCA## changes.
# The UNIQUE_ID column in siteinfo is what links the same sites across years. It appears that sites are
# only sampled twice, so if they were sampled in 2011 and 2021, they were dropped in 2021.
#   - Need to compile plant taxa tables separately
#   - Dropped nwca_2016_veg_mmi.csv from initial compile, because doesn't include UID (only SITE_ID)
#   - Dropped ncwa21_landscape_metrics-data.csv because no UID column (only SiteID)


#--- Combining data across years ----
# The CoCs are so different across years that I can't calculate the vegetation MMI for the entire
# NCE reporting area without a lot of effort matching C across states. Instead, comparisons between
# ACAD and the region will focus on the Northeast, EPA's Region 1, and use NEIWPCC's CoCs that were
# developed in 2018 by ecoregion, and that most reflect the Cs used to develop the Vegetation MMI
# and thresholds. That analysis starts in 01_compile_NWCA_R1_data.R

