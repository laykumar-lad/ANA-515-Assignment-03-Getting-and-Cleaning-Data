# Load necessary libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(usmap)
library(tidyverse) # For US map plotting

# 1. Read the CSV file
storm_data <- read.csv("D:/McDanile/ANA 515 Data Preparation/Week 6/Assignment 03/StormEvents_details-ftp_v1.0_d1996_c20220425.csv")

# 2. Limit the dataframe to the following columns:
storm_data <- storm_data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

# 3. Arrange the data by the state name (STATE) 
storm_data <- storm_data %>%
  arrange(STATE)

# 4. Change state and county names to title case 
storm_data <- storm_data %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))

# 5.	Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then
#     remove the CZ_TYPE column
storm_data <- storm_data %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

# 6. Pad FIPS Columns and Unite
storm_data <- storm_data %>%
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 3, pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0")) %>%
  unite(FIPS, STATE_FIPS, CZ_FIPS, sep = "")

# 7.	Change all the column names to lower case
storm_data <- storm_data %>%
  rename_all(tolower)

# 8. Create Dataframe with State Information
data("state")
state_info <- data.frame(state = tolower(state.name), 
                         area = state.area, 
                         region = state.region)

# 9. Create Dataframe with Number of Events per State and Merge with State Info
event_count <- storm_data %>%
  group_by(state) %>%
  summarise(num_events = n()) %>%
  mutate(state = tolower(state))

merged_data <- merge(x=event_count, y=state_info, by.x = "state", by.y = "state", all.x = TRUE)
merged_data <- na.omit(merged_data)

# 10. Create Plot
ggplot(merged_data, aes(x = area, y = num_events, color = region)) +
  geom_point(size = 3) +
  labs(title = "# of Storm Events in 1996 vs. Land Area",
       x = "Land area (square miles)",
       y = "# of storm events in 1996",
       color = "region") +
  theme_minimal()


