require(sf)
require(tidyverse)


# Read in line list data
data = 
  read.csv("U:\\Datasets\\combined_file_JamesWood_06FEB2022_4PM\\CASES_FROM_20200701_0000_TO_20220207_1145.csv")


# Filter down to case ID, date and spatial location
data = 
  data %>%
  select(project_recid,
         notification_date = EARLIEST_CONFIRMED_OR_PROBABLE,
         lhd_2010_name = LHD_2010_NAME,
         sa2_2016_code = SA2_2016_CODE,
         pcr_confirmed = confirmed_by_pcr) %>%
  filter(!is.na(sa2_2016_code)) %>%
  unique() %>%
  as_tibble()


##################################################################
##  MAKE SA2 2016 TO LGA 2019 AND POA 2016 CORRESPONDANCE FILE  ##
##################################################################
# 
# 
# # Get 2016 SA2 centroids
# sa2s =
#   st_read('D:/Data/abs/spatial/SA2_2016_AUST.gpkg', quiet = T) %>%
#   filter(STATE_CODE_2016 == 1) %>%
#   select(sa2_2016_code = SA2_MAINCODE_2016,
#          sa2_2016_name = SA2_NAME_2016) %>%
#   st_point_on_surface()
# 
# 
# # Check
# tmap_mode('view')
# qtm(sa2s)
# 
# 
# # Get 2019 LGAs
# lgas =
#   st_read('D:/Data/abs/spatial/LGA_2019_AUST.gpkg', quiet = T) %>%
#   filter(STATE_CODE_2016 == 1) %>%
#   select(lga_code19 = LGA_CODE_2019,
#          lga_name19 = LGA_NAME_2019)
# 
# 
# # Check
# qtm(lgas)
# 
# 
# # Intersect
# conc = st_intersection(sa2s, lgas)
# 
# 
# # Check
# lgas %>%
#   tm_shape() +
#   tm_polygons('lga_name19') +
#   tm_shape(conc) +
#   tm_dots('lga_name19')
# 
# 
# # Save SA2-LGA concordance file
# st_geometry(conc) = NULL
# conc = as_tibble(conc)
# saveRDS(conc, 'data/sa2_to_lga19_concordance_file.Rds')
# 
# 
# # Get 2016 POAs
# poas =
#   st_read('D:/Data/abs/spatial/POA_2016_AUST.gpkg', quiet = T) %>%
#   select(postcode = POA_CODE_2016)
# 
# 
# # Check
# qtm(poas)
# 
# 
# # Intersect
# conc = st_intersection(sa2s, poas)
# 
# 
# # Check
# poas %>%
#   filter(as.numeric(postcode) >= 2000,
#          as.numeric(postcode) < 3000) %>%
#   tm_shape() +
#   tm_polygons('postcode') +
#   tm_shape(conc) +
#   tm_dots('postcode')
# 
# 
# # Save SA2-POA concordance file
# st_geometry(conc) = NULL
# conc = as_tibble(conc)
# saveRDS(conc, 'data/sa2_to_poa_concordance_file.Rds')


############################################################
## USE CONCORDANCE FILES TO GET LGAS AND POAS IN DATASET  ##
############################################################


# Convert SA2 numbers to LGAs and POAs
data = 
  data %>%
  mutate(sa2_2016_code = as.character(sa2_2016_code)) %>%
  left_join(readRDS('data/sa2_to_lga19_concordance_file.Rds'), by = 'sa2_2016_code') %>%
  left_join(readRDS('data/sa2_to_poa_concordance_file.Rds'), by = c('sa2_2016_code', 'sa2_2016_name')) %>%
  select(-project_recid,
         -starts_with('sa2')) %>%
  mutate(notification_date = ymd(notification_date),
         pcr_confirmed = pcr_confirmed == 'Yes')


#############
##  CHECK  ##
#############


# Load public data
nsw_data_url <- "https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/21304414-1ff1-4243-a5d2-f52778048b29/download/confirmed_cases_table1_location.csv"
cases <- readr::read_csv(nsw_data_url)


# Aggregate public data
public = 
  cases %>%
  group_by(notification_date) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(version = 'public')


# Aggregate private data
check_lga = 
  data %>%
  group_by(notification_date, pcr_confirmed) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(version = 'private')


# Plot the two
check_lga %>%
  ggplot(aes(x = notification_date, y = count)) +
  geom_col(aes(fill = pcr_confirmed)) +
  geom_line(data = public) +
  labs(y = 'Number of New Cases',
       fill = 'PCR Confirmed',
       title = 'Overall Agreement Between Public and Private Datasets') +
  xlim(c(ymd('2021-12-01'), today()))



notification_date
postcode
lhd_2010_code
lhd_2010_name
lga_code19
lga_name19
