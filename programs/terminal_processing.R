# Cleaning Boston terminal data

# Libraries
library(tidyverse)
library(openxlsx)

# input library
input_dir <- "data/redtomato/Boston Terminal Market Reports"
output_dir <- "data/redtomato/output"

# read in data
terminal_raw_list <- map(c("2014", "2015", "2017"), function(x){
  read.xlsx(paste0(input_dir, "/terminal_", x, ".xlsx"))
})

terminal_raw <- bind_rows(terminal_raw_list)

# data checks
stopifnot(nrow(terminal_raw) == sum(map_dbl(terminal_raw_list, nrow))) # same number of rows
stopifnot(length(terminal_raw) == unique(map_dbl(terminal_raw_list, length))) # same number of columns

# processing
terminal_data <- terminal_raw %>% 
  filter(Commodity.Name == "APPLES",
         Package %in% c("cartons tray pack", 
                        "cartons 12 3-lb film bags"),
         City.Name == "BOSTON") %>% 
  mutate(weights = case_when(!Item.Size %in% c("2 1/2\" up", "2 1/2\" min") ~ as.numeric(str_split(Item.Size, "s")[[1]][1]),
                            TRUE ~ 1))

terminal_avg <- terminal_data %>% 
  mutate(region = case_when(Origin %in% c("NEW HAMPSHIRE", "MAINE", "PENNSYLVANIA", "NEW YORK") ~ "Northeast",
                            Origin %in% c("WASHINGTON") ~ "Northwest")) %>% 
  group_by(Variety, region) %>% 
  summarise(avg_high    = weighted.mean(Low.Price, weights),
            avg_low     = weighted.mean(High.Price, weights),
            overall_avg = mean(avg_high, avg_low, na.rm = T) %>%  round(2)) %>% 
  ungroup() %>% 
  mutate_at(vars(Variety), tolower) %>% 
  na.omit() %>% 
  select(variety = Variety, region, overall_avg)

write.csv(terminal_avg, file.path(output_dir, "terminal_boston.csv"))
