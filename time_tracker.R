
# TOIL tracker

# These are the packages used in this script
packages <- c('easypackages','tidyr', 'ggplot2', 'dplyr', 'purrr', 'stringr', 'viridis', 'nomisr', 'lemon',  'flextable', 'googlesheets4', 'lubridate', 'scales')

# This command installs only those packages (from our object packages) which are not already installed.
install.packages(setdiff(packages, rownames(installed.packages())))

# This loads the packages
easypackages::libraries(packages)

# tt_theme
library(showtext)
showtext_auto(TRUE)

# BUG - find file path for working on own laptop
# Load up Poppins font
font_paths("\\\\chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Rich Tyler/Fonts")
# font_add(family = "roboto", "Roboto-Regular.ttf")
font_add(family = "poppins", "Poppins-Regular.ttf")
font_add(family = "poppinsb", "Poppins-Bold.ttf")

tt_theme = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 16, family = 'poppinsb'),
    plot.subtitle = element_text(colour = "#000000", size = 15),
    plot.caption = element_text(colour = "#000000", size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.y = element_line(colour = "#E7E7E7", size = .3),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(colour = "#000000", size = 11),
    strip.background = element_blank(),
    legend.title = element_text(colour = "#000000", size = 11, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    #  legend.key.size = unit(1,"line"),
    legend.text = element_text(colour = "#000000", size = 11),
    axis.text = element_text(colour = "#000000", size = 11),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.ticks = element_line(colour = "#dbdbdb"),
    axis.title =  element_text(colour = "#000000", size = 11, face = "bold"),
    axis.line = element_line(colour = "#dbdbdb"),
    text = element_text(family = 'poppins', size = 14, lineheight = 0.5))}

# Parameters
working_week_hours = 37
assumed_lunch_break = .6

# I want this process to work automatically without a break in the running of the script but have not figured out a way yet as the prompt goes to console window 
# Add some new fields and calculations
 tt_df <- read_sheet('https://docs.google.com/spreadsheets/d/16WF3SQJ5blnY_E7UE3SMLIIqjAkLBtje4_s1G_G-xqo/edit?usp=sharing') %>% 
    mutate(Start = as.POSIXct(paste0(Date, format(Start, '%H:%M')), format = '%Y-%m-%d %H:%M')) %>% 
    mutate(End = as.POSIXct(paste0(Date, format(End, '%H:%M')), format = '%Y-%m-%d %H:%M')) %>% 
    mutate(Hours = ifelse(!is.na(Hours), Hours, difftime(End, Start, unit = 'hours'))) %>% # I think we could adapt to situations where I've recorded the start and end time or whether I have just entered the number of hours I've done
    mutate(Minutes = ifelse(!is.na(Hours), Hours * 60, difftime(End, Start, unit = 'mins'))) %>% 
  #  mutate(Hours = ifelse(Given_taken == 'Taken', 0, Hours)) %>% 
    mutate(Day = format(Date, '%A'),
           Week = format(Date, '%W'),
           Month = format(Date, '%B'),
           Year = format(Date, '%Y')) %>% 
   mutate(Week_commencing = paste0(ordinal(as.numeric(format(floor_date(Date, unit = 'week', week_start = 1), '%d'))), ' ', format(floor_date(Date, unit = 'week', week_start = 1), '%B'), format(floor_date(Date, unit = 'week', week_start = 1), ' %Y'))) %>%
   group_by(Date) %>% 
   mutate(Log_off = as.numeric(format(max(End), '%H%M')))
 
2

weekdays_in_df <- tt_df %>% 
   filter(Given_taken != 'Bank holiday') %>% 
   select(Date, Day, Week_commencing) %>% 
   unique() %>% 
   filter(!Day %in% c('Saturday', 'Sunday')) %>% 
   mutate(Normal_hours = 7.4) %>% 
   group_by(Week_commencing) %>% 
   mutate(Normal_hours_so_far = sum(Normal_hours)) %>%
   mutate(Number_of_days = n()) %>% 
   select(Week_commencing, Number_of_days, Normal_hours_so_far) %>%
   unique()

weeks_levels <- weekdays_in_df %>% 
  select(Week_commencing) %>% 
  unique()

if(length(unique(tt_df$Given_taken))!= 4){
  print('There may be some additional descriptions of given taken added (or typos)')
  beepr::beep(9)
}
  
# Daily summary 
daily_tt <- tt_df %>% 
  mutate(Normal_hours = ifelse(Day %in% c('Saturday', 'Sunday'), 0, 7.4)) %>% 
  group_by(Date, Day, Log_off, Given_taken, Normal_hours, Week_commencing, Month) %>% 
  summarise(Hours_worked = sum(Hours, na.rm = TRUE)) %>% 
  pivot_wider(names_from = 'Given_taken',
              values_from = 'Hours_worked',
              values_fill = 0) %>% 
  mutate(Total_hours = (Given + Taken + `Annual leave` + `Bank holiday`) - assumed_lunch_break) %>% # Your day is accounted for by either giving hours of time, using toil (Taken), using annual leave, or being a bank holiday
  mutate(Hours_relative_to_normal = Total_hours - Normal_hours) %>% 
  mutate(Within_week_hours = factor(ifelse(Hours_relative_to_normal >= 0, 'Above or at hours', ifelse(Hours_relative_to_normal < 0, 'Less than normal hours', NA)), levels = c('Above or at hours', 'Less than normal hours'))) %>% 
  mutate(Extra_hours = factor(ifelse(Hours_relative_to_normal <= 0, 'None', ifelse(Hours_relative_to_normal <= 1, 'Up to 1 hour', ifelse(Hours_relative_to_normal <= 2, '1-2 hours', ifelse(Hours_relative_to_normal <= 3, '2-3 hours', 'More than 3 hours')))), levels = c('None', 'Up to 1 hour', '1-2 hours', '2-3 hours', 'More than 3 hours')))

daily_tt %>% 
  filter(!Day %in% c('Saturday', 'Sunday')) %>%
  ungroup() %>% 
  summarise(max(Hours_relative_to_normal),
            min(Hours_relative_to_normal))

# Which days tend to have extra hours?
daily_tt %>% 
  filter(!Day %in% c('Saturday', 'Sunday')) %>% 
  mutate(Day = factor(Day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>% 
  ggplot(aes(x = as.Date(Date),
             y = Hours_relative_to_normal,
             fill = Day,
             group = 1)) +
  labs(title = 'Number of hours above normal working hours (7.4 hours)',
       subtitle = "Mondays and Tuesdays are typically extra hours days, by Friday it's definitely pimms o'clock",
       caption = 'This is not just late working, this is hours on top of the normal working day.',
       y = 'Hours',
       x = '') +
  scale_fill_manual(values = viridis::turbo(5)) +
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(-1,5,1),
                     expand = c(0,0.01)) +
  scale_x_date(labels = date_format("%d-%b"),
               breaks = seq.Date(from = as.Date(min(daily_tt$Date)), to = as.Date(max(daily_tt$Date)), by = '7 days')) +
  geom_bar(stat = 'identity',
           width = 1) +
  tt_theme() +
  theme(legend.position = 'top')

daily_tt %>% 
  filter(!Day %in% c('Saturday', 'Sunday')) %>% 
  mutate(Day = factor(Day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>% 
  ggplot(aes(x = as.Date(Date),
             y = Hours_relative_to_normal,
             fill = Extra_hours,
             group = 1)) +
  labs(title = 'Number of hours above normal working hours (7.4 hours)',
       subtitle = "Mondays and Tuesdays are typically extra hours days, by Friday it's definitely pimms o'clock",
       caption = 'This is not just late working, this is hours on top of the normal working day.',
       y = 'Hours',
       x = '') +
  scale_fill_manual(values = viridis::viridis(5, direction = -1),
                    drop = FALSE)+
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(-1,5,1),
                     expand = c(0,0.01)) +
  scale_x_date(labels = date_format("%d-%b"),
               breaks = seq.Date(from = as.Date(min(daily_tt$Date)), to = as.Date(max(daily_tt$Date)), by = '7 days')) +
  geom_bar(stat = 'identity',
           width = 1) +
  tt_theme() +
  theme(legend.position = 'top')

# TODO How many days am I working later than 9pm ###

daily_tt %>% 
  filter(`Annual leave` == 0 && `Bank holiday` == 0) %>% 
  ggplot(aes(x = as.Date(Date),
             y = Log_off,
             fill = Extra_hours,
             group = 1
           )) +
 # geom_line() +
  geom_point(shape = 21,
             size = 4) +
  labs(title = 'Working late: finish times',
       subtitle = "Late working is common, with many days ending around 9pm, although some weeks it is to fulfil normal hours.",
       caption = 'This excludes annual leave and bank holidays.',
       y = 'Time finished',
       x = '') +
  scale_fill_manual(values = viridis::viridis(5, direction = -1),
                    drop = FALSE,
                    name = 'Additional hours')+
  scale_y_continuous(limits = c(1200,2400),
                     breaks = seq(1200, 2400, 100),
                     labels = c('12:00', '13:00', '14:00', '15:00', '16:00', '17:00', '18:00', '19:00', '20:00', '21:00', '22:00', '23:00', 'Midnight'),
                     expand = c(0,0.01)) +
  scale_x_date(labels = date_format("%d-%b"),
               breaks = seq.Date(from = as.Date(min(daily_tt$Date)), to = as.Date(max(daily_tt$Date)), by = '7 days')) +

  tt_theme() +
  theme(legend.position = 'top')

# TODO calendar plot ###
# Breaks, up to 1 hour, 1-2 hours, 2+ hours over

ph_cal_theme = function(){
  theme(
    text = element_text(size = 11, family = 'poppins'),
    plot.title = element_text(colour = "#000000", face = "bold", size = 12, vjust = 1, family = 'poppinsb'),
    plot.background = element_rect(fill = "white", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25),
    axis.text.y = element_blank(),
    axis.title = element_text(colour = "#327d9c", family = "poppinsb"),
    strip.text = element_text(colour = "#000000", family = "poppinsb"),
    strip.background = element_rect(fill = "#ffffff"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())}

all_dates <- data.frame(Date = seq.Date(as.Date('2023-11-01', format = "%Y-%m-%d"), as.Date('2024-12-31', format = "%Y-%m-%d"), by="day")) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = format(Date, '%Y')) %>% 
  mutate(Month_n = format(Date, '%m')) %>% 
  mutate(Month_name = format(Date, '%b')) %>% 
  mutate(Weekday = ifelse(format(Date, '%w') == 0, 7, format(Date, '%w'))) %>% 
  mutate(Weekday_name = format(Date, '%a')) %>% 
  mutate(Month_year = format(Date, '%m-%Y'))

# Create a dataframe of every month between the start and end of the data

# If you have the first of the starting month in the dataset (as I added above) there is no need to execute the next few rows, if not (in our case it would have started on the 2/11/2013) then it will create a dataframe with the 2nd of each month as the date monthy and then yo have to retrospectively make the date the 1st

months_in_between <- data.frame(Date = seq.Date(from = min(all_dates$Date), to = max(all_dates$Date), by = "months"))

# This asks if the first day of the month for the first observation is '01'. If it is then it will skip over the next three lines, if it is not then it will create a new field that concatenates the month-year of the date with 01 at the start, and then overwrites the date field with the dates starting on the 1st of the month. The third line removes the created field.

if (!(format(months_in_between$Date, "%d")[1] == "01")){
  months_in_between$Date_1 <- paste("01", format(months_in_between$Date, "%m-%Y"), sep = "-")
  months_in_between$Date <-as.Date(months_in_between$Date_1, format="%d-%m-%Y")
  months_in_between$Date_1 <- NULL
}  

# Day of the week (name and then number)
months_in_between <- months_in_between %>% 
  mutate(dw = format(Date, "%A"),
         dw_n = ifelse(format(Date, "%w") == 0, 7, format(Date, '%w'))) %>% # For some reason R thinks the week should start on Sunday (terrible idea), so we need to turn the 0 to 7 so that it is seen as the last day.
  mutate(Month_year = format(Date, "%m-%Y"))

# To make the calendar plot we are going to need to create a grid of 42 tiles (representing seven days in each week for six weeks, as any outlook calendar shows). From this we can start the data somewhere between tile one and tile seven depending on the day of the week the month starts (e.g. if the month starts on a wednesday, then we want the data to start on tile three).

# Make an empty dataframe with each month of the 'months_in_between' dataframe repeated 42 times
df_1 <- data.frame(Month_year = rep(months_in_between$Month_year, 42)) %>% 
  group_by(Month_year) %>% 
  mutate(id = 1:n()) # For each month, add a field with the sequence one to 42 (or the number of times the month has been repeated)

# Add the information we created about the day each month starts
final_df <- all_dates %>% 
  left_join(months_in_between[c("Month_year", "dw_n")], by = "Month_year") %>% 
  mutate(dw_n = as.numeric(dw_n)) %>% # When we created the values for day of the week with the format() function, the values returned were all characters. Make sure R reads the number of the starting day as a number and not a character
  group_by(Month_year) %>% 
  mutate(id = 1:n()) %>% # We need to create an id number to show the order of dates within a month
  mutate(id = id + dw_n - 1) %>% # If we add this id number to the dw (day of the week that the month starts) number, the id number becomes the position in our grid of 42 that the date should be. 
  # As we can only start a sequence from 1 onwards, and not zero, we need to subtract one from the total otherwise the position is offset too far.
  mutate(Week = ifelse(id <= 7, 1, ifelse(id <= 14, 2, ifelse(id <= 21, 3, ifelse(id <= 28, 4, ifelse(id <= 35, 5, 6)))))) # We can now overwrite the Week field in our dataframe to show what it should be given the grid of 42 days

# Rebuilding ####

final_df_1 <- left_join(df_1, final_df, by = c("Month_year", "id")) %>% 
  mutate(Year = substr(Month_year, 4, 7)) %>%  # take the four to seventh characters from the Month_year field to create the year
  mutate(Month_n = substr(Month_year, 1,2)) %>% # take the first and second characters from the Month_year field to create the month number
  mutate(Weekday_name = ifelse(id %in% c(1,8,15,22,29,36) & is.na(Date), "Mon", 
                               ifelse(id %in% c(2,9,16,23,30,37) & is.na(Date), "Tue", 
                                      ifelse(id %in% c(3,10,17,24,31,38) & is.na(Date), "Wed", 
                                             ifelse(id %in% c(4,11,18,25,32,39) & is.na(Date), "Thu",
                                                    ifelse(id %in% c(5,12,19,26,33,40) & is.na(Date), "Fri", 
                                                           ifelse(id %in% c(6,13,20,27,34,41) & is.na(Date), "Sat", 
                                                                  ifelse(id %in% c(7,14,21,28,35,42) & is.na(Date), "Sun", 
                                                                         Weekday_name)))))))) %>% # look through the dataframe and where the date is missing (indicating a non-date filler value within our 42 grid) and the id value is 1,8,15,22,29, or 36 (i.e. the monday value in our 42 grid for the month) then add a "Mon" for the day of the week and so on.
  mutate(Weekday = ifelse(id %in% c(1,8,15,22,29,36) & is.na(Date), 1,
                          ifelse(id %in% c(2,9,16,23,30,37) & is.na(Date), 2, 
                                 ifelse(id %in% c(3,10,17,24,31,38) & is.na(Date), 3,
                                        ifelse(id %in% c(4,11,18,25,32,39) & is.na(Date), 4,
                                               ifelse(id %in% c(5,12,19,26,33,40) & is.na(Date), 5,
                                                      ifelse(id %in% c(6,13,20,27,34,41) & is.na(Date), 6,
                                                             ifelse(id %in% c(7,14,21,28,35,42) & is.na(Date), 7, 
                                                                    Weekday )))))))) %>% # Similar to above but numbers not days
  mutate(Week = factor(ifelse(id <= 7,  1,
                              ifelse(id <= 14, 2,  
                                     ifelse(id <= 21, 3,
                                            ifelse(id <= 28, 4,
                                                   ifelse(id <= 35, 5,
                                                          ifelse(id <= 42, 6, 
                                                                 NA)))))), levels = c(6,5,4,3,2,1))) %>% # Add a calendar week value for faceting (1-6 from our 42 tile grid). # This is not the same as the week of the month and we save the levels of this field so R knows how to plot them.
  mutate(Month_name = factor(ifelse(Month_n == "01", "Jan",
                                    ifelse(Month_n == "02", "Feb",
                                           ifelse(Month_n == "03", "Mar",
                                                  ifelse(Month_n == "04", "Apr",
                                                         ifelse(Month_n == "05", "May",
                                                                ifelse(Month_n == "06", "Jun",
                                                                       ifelse(Month_n == "07", "Jul",
                                                                              ifelse(Month_n == "08", "Aug",
                                                                                     ifelse(Month_n == "09", "Sep",
                                                                                            ifelse(Month_n == "10", "Oct",
                                                                                                   ifelse(Month_n == "11", "Nov", 
                                                                                                          ifelse(Month_n == "12", "Dec", 
                                                                                                                 NA)))))))))))), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% # Fill in the blanks of month name using the value in Month_n
  mutate(Weekday_name = factor(ifelse(Weekday_name == "Mon", "Monday",
                                      ifelse(Weekday_name == "Tue", "Tuesday",
                                             ifelse(Weekday_name == "Wed", "Wednesday",
                                                    ifelse(Weekday_name == "Thu", "Thursday", 
                                                           ifelse(Weekday_name == "Fri", "Friday",
                                                                  ifelse(Weekday_name == "Sat", "Saturday",
                                                                         ifelse(Weekday_name == "Sun", "Sunday", 
                                                                                Weekday_name))))))), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% # Use the full weekday name
  select(!dw_n)


calendar_df <- final_df_1 %>% 
  left_join(daily_tt[c('Date', 'Given', 'Taken', 'Total_hours', 'Hours_relative_to_normal', 'Normal_hours', 'Log_off', 'Within_week_hours', 'Extra_hours')], by = 'Date') %>% 
  mutate(Bins_extra = factor(ifelse(is.na(Date), "non date",
                              ifelse(is.na(Extra_hours), 'data missing',
                                     ifelse(Extra_hours == 'None', "Normal day",
                                            as.character(Extra_hours) ))), levels = c("Normal day",'Up to 1 hour',"1-2 hours", "2-3 hours", "More than 3 hours", "data missing", "non date"))) %>% 
  mutate(Bins_finish = factor(ifelse(is.na(Date), "non date",
                                    ifelse(is.na(Log_off), 'data missing',
                                           ifelse(Log_off <= 1800, 'By 6pm',
                                                  ifelse(Log_off <= 1900, 'By 7pm',
                                                         ifelse(Log_off <= 2000, 'By 8pm',
                                                                ifelse(Log_off <= 2100, 'By 9pm',
                                                                       ifelse(Log_off <= 2200, 'By 10pm',
                                                                              ifelse(Log_off <= 2300, 'By 11pm',
                                                                                     "Near midnight")))))))),
                              levels = c("By 6pm",'By 7pm', 'By 8pm', 'By 9pm', 'By 10pm', 'By 11pm', 'Near midnight', "data missing", "non date")))

bin_colours <- c('#fcc5c0','#fa9fb5','#f768a1','#c51b8a','#7a0177', "#8e8e8e", "#f9f9f9")

bin_lateness_colours <- c("#49C1AD",  "#359EAA","#357BA2","#3B5698","#3E356B", "#2B1C35",'#0B0405', "#8e8e8e", "#f9f9f9")

calendar_df %>% 
  ggplot() +
  geom_tile(aes(x = Weekday_name, 
                y = Week, 
                fill = Bins_extra),
            colour = "#ffffff") + 
  facet_grid(Year ~ Month_name) +
  scale_x_discrete(expand = c(0,0.1)) +
  scale_fill_manual(values = bin_colours,
                    drop = FALSE,
                    breaks = c('Normal day','Up to 1 hour','1-2 hours','2-3 hours','More than 3 hours'),
                    name = '',
                    na.value = '#f9f9f9') +
  labs(title = 'Extra working hours by day;',
       subtitle = 'West Sussex; 1st November 2023 - 31st March 2024',
       x = '', 
       y = '') +
  ph_cal_theme() +
  guides(fill = guide_legend(nrow = 1, 
                             byrow = TRUE))

calendar_df %>% 
  ggplot() +
  geom_tile(aes(x = Weekday_name, 
                y = Week, 
                fill = Bins_finish),
            colour = "#ffffff") + 
  facet_grid(Year ~ Month_name) +
  scale_x_discrete(expand = c(0,0.1)) +
  scale_fill_manual(values = bin_lateness_colours,
                    drop = FALSE,
                    breaks = c("By 6pm",'By 7pm', 'By 8pm', 'By 9pm', 'By 10pm', 'By 11pm', 'Near midnight'),
                    name = '',
                    na.value = '#f9f9f9') +
  labs(title = 'Final log off time by day;',
       subtitle = 'West Sussex; 1st November 2023 - 31st March 2024',
       x = '', 
       y = '') +
  ph_cal_theme() +
  guides(fill = guide_legend(nrow = 1, 
                             byrow = TRUE))

# Weekly tt ####

weekly_tt <- tt_df %>% 
  group_by(Week_commencing) %>% 
  summarise(Hours_worked = sum(Hours, na.rm = TRUE)) %>% 
  left_join(weekdays_in_df, by = 'Week_commencing') %>% 
  mutate(Hours_owed = Hours_worked - Normal_hours_so_far - (assumed_lunch_break * Number_of_days))

weekly_tt %>% 
  ungroup() %>% 
  summarise(Hours_currently_owed = sum(Hours_owed))

weekly_tt %>% 
  mutate(within_week_hours = factor(ifelse(Hours_worked >= Normal_hours_so_far, 'Above or at hours', ifelse(Hours_worked < Normal_hours_so_far, 'Less than normal hours', NA)), levels = c('Above or at hours', 'Less than normal hours'))) %>% 
  pivot_longer(cols = c('Hours_worked', 'Normal_hours_so_far'),
               names_to = 'Measure',
               values_to = 'Hours') %>% 
  ggplot(aes(x = Week_commencing,
             y = Hours,
             group = Measure)) +
  geom_line() +
  geom_point(shape = 21,
             aes(fill = within_week_hours,
                 size = Measure),
             colour = '#000000'
             ) +
  scale_size_manual(values = c(5, 0)) +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 55,5)) +
  tt_theme()

# TODO create a years worth of Week_commencing values as a factor with levels to keep the right order 

# TODO Make a current week summary

tt_df %>%
  filter(Week_commencing == '29th January 2024') %>% 
  filter(Given_taken == 'Given') %>%
  group_by(Week_commencing) %>% 
  summarise(Hours = sum(Hours)) %>% 
  left_join(weekdays_in_df, by = 'Week_commencing') %>% 
  mutate(Hours_owed = Normal_hours_so_far - Hours)

# TODO Make a TOIL status (hours owed) table

 
