
# TOIL tracker

# These are the packages used in this script
packages <- c('easypackages','tidyr', 'ggplot2', 'dplyr', 'purrr', 'stringr', 'viridis', 'nomisr', 'lemon',  'flextable', 'googlesheets4', 'lubridate', 'scales')

# This command installs only those packages (from our object packages) which are not already installed.
install.packages(setdiff(packages, rownames(installed.packages())))

# This loads the packages
easypackages::libraries(packages)

# Parameters
working_week_hours = 37
assumed_lunch_break = .5

# I want this process to work automatically without a break in the running of the script but have not figured out a way yet as the prompt goes to console window 
# Add some new fields and calculations
 tt_df <- read_sheet('https://docs.google.com/spreadsheets/d/16WF3SQJ5blnY_E7UE3SMLIIqjAkLBtje4_s1G_G-xqo/edit?usp=sharing') %>% 
    mutate(Start = as.POSIXct(paste0(Date, format(Start, '%H:%M')), format = '%Y-%m-%d %H:%M')) %>% 
    mutate(End = as.POSIXct(paste0(Date, format(End, '%H:%M')), format = '%Y-%m-%d %H:%M')) %>% 
    mutate(Hours = ifelse(!is.na(Hours), Hours, difftime(End, Start, unit = 'hours'))) %>% # I think we could adapt to situations where I've recorded the start and end time or whether I have just entered the number of hours I've done
    mutate(Minutes = ifelse(!is.na(Hours), Hours * 60, difftime(End, Start, unit = 'mins'))) %>% 
    mutate(Hours = ifelse(Given_taken == 'Taken', 0, Hours)) %>% 
    mutate(Day = format(Date, '%A'),
           Week = format(Date, '%W'),
           Month = format(Date, '%B'),
           Year = format(Date, '%Y')) %>% 
   mutate(Week_commencing = paste0(ordinal(as.numeric(format(floor_date(Date, unit = 'week', week_start = 1), '%d'))), ' ', format(floor_date(Date, unit = 'week', week_start = 1), '%B')))
 
 2

# TODO Make a rolling summary 

 weekdays_in_df <- tt_df %>% 
   select(Date, Day, Week_commencing) %>% 
   unique() %>% 
   filter(!Day %in% c('Saturday', 'Sunday')) %>% 
   mutate(Normal_hours = 7.4) %>% 
   group_by(Week_commencing) %>% 
   mutate(Normal_hours_so_far = sum(Normal_hours)) %>%
   mutate(Number_of_days = n()) %>% 
   select(Week_commencing, Number_of_days, Normal_hours_so_far) %>%
   unique()
 
tt_df

weekly_tt <- tt_df %>% 
  group_by(Week_commencing) %>% 
  summarise(Hours_worked = sum(Hours, na.rm = TRUE)) %>% 
  left_join(weekdays_in_df, by = 'Week_commencing') %>% 
  mutate(Hours_owed = Hours_worked - Normal_hours_so_far - (assumed_lunch_break * Number_of_days))

weekly_tt %>% 
  ungroup() %>% 
  summarise(Hours_currently_owed = sum(Hours_owed))

weekly_tt

# TODO create a years worth of Week_commencing values as a factor with levels to keep the right order 
# TODO Make a current week summary
# TODO Make a TOIL status (hours owed) table
 
