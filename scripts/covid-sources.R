# Script information
#' Title:
#' Author: Laura Espinosa
#' Date created: 11 November 2021
#' Date modified: 1 December 2021

# Packages --------------
library(pacman) # install.packages('pacman') if the package is not installed
p_load(tidyverse, jsonlite, rjson, plyr, hms)

# Import data --------------
# Data from webscraper
df_web <- fromJSON(file = "data/website.json")

# Data from social media
df_smedia <- read_csv("data/social_media_timestamp.csv")

# Clean df_web ---------------
# country names
df_names <- names(df_web)
df_names_smedia <- unique(df_smedia$Country)

## Clean website df --------------
# empty df to save time stamps
df_web_clean <- NULL

# df with time stamps, cases and country
for (i in c(1:20)) {
  # create dataframe for each country from the json file
  assign("df", 
         rownames_to_column(as.data.frame(do.call(rbind, df_web[[i]])), 
                            "datetime_web"))
  df <- df %>% 
    # create new variable or change format of existing variables
    mutate(Country = df_names[i],
           datetime_web = as.POSIXct(df$datetime_web, format = "%Y-%m-%d_%H-%M-%OS"),
           date_web = as.Date(df$datetime_web),
           cases_web = V1) %>% 
    # delete old variable
    select(-V1)
  df$time_web <- as_hms(df$datetime_web, format = "%H:%M:%S")
  # Bind all dataframes from each country in an unique dataframe
  df_web_clean <- rbind(df_web_clean, df) 
}

# # option b
# for (i in c(1:16, 19)) {
#   assign("df", 
#          rownames_to_column(data.frame(do.call("rbind", df_web[[i]])), 
#                             "datetime_web"))
#   df <- df %>% 
#     mutate(Country = df_names[i],
#            datetime_web = as.POSIXct(df$datetime_web, format = "%Y-%m-%d_%H-%M-%OS"),
#            date_web = as.Date(df$datetime_web)) 
#   df$time_web <- as_hms(df$datetime_web, format = "%H:%M:%S")
#   df_web_clean <- rbind(df_web_clean, df) 
# }

## Clean social media df ----------------
df_smedia_clean <- df_smedia %>% 
  mutate(
    date_smedia = as.Date(Date, format = "%d/%m/%Y"),
    time_smedia = as_hms(Time),
    datetime_smedia = as.POSIXct(paste(Date, Time, sep = " "),
                                  format = "%d/%m/%Y %H:%M:%OS")
  )

## Merge both datasets ----------------------
df_all <- df_web_clean %>% 
  left_join(df_smedia_clean, by = c("Country"="Country", "date_web"="date_smedia")) %>% 
  select(-WHO_reg, -Source, -Deaths, -Comments) %>% 
  mutate(diff_min = round(difftime(datetime_web, datetime_smedia, units = "mins"), 2)) %>% 
  filter(datetime_web >= "2021-11-01") %>% 
  arrange(Country, datetime_web)

# Descriptive analysis ---------------
# merged dataset -----------
df_all_country <- df_all %>% 
  select(diff_min, Country, date_web) %>% 
  group_by(Country) %>% 
  ungroup() %>% 
  # Create new variable with three categories depending on difference in minutes
  mutate(diff_min_num = as.numeric(diff_min),
         diff_min_cat = case_when(diff_min_num < 0 ~ "Difference < 0",
                                  diff_min_num == 0 ~ "Difference = 0",
                                  diff_min_num > 0 ~ "Difference > 0",
                                  TRUE ~ NA_character_))

## Plot with all countries ------------
plot <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  geom_point(size = 2) +
  # specific colour according to category
  scale_color_manual(values = c("Difference < 0" = "red", 
                                "Difference = 0" = "black",
                                "Difference > 0" = "blue")) +
  scale_x_date(breaks = "2 days") +
  scale_y_continuous()+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in Europe",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       color = "Difference* between \nwebsite and social media",
       caption = "* Website earlier (difference < 0) or social media earlier (difference > 0)")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1)) +
  facet_wrap(~ Country)



plot
ggsave(paste("outputs/time_diff_", Sys.Date(), ".jpeg", sep=""), plot)

## Plot for a single country ------------
country = "Andorra"

plot_country <- df_all_country %>% 
  filter(Country == country) %>% 
  filter(!is.na(diff_min)) %>% 
  ggplot(aes(x = date_web, y = diff_min, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  geom_point(size = 2) +
  # specific colour according to category
  scale_color_manual(values = c("Difference < 0" = "red", 
                                "Difference = 0" = "black",
                                "Difference > 0" = "blue")) +
  scale_x_date(breaks = "1 day") +
  scale_y_continuous()+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = paste("Time difference (minutes) between website pages and social media posts \non COVID-19 cases in ", country, sep = ""),
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       caption = "Website earlier (difference < 0) or social media earlier (difference > 0)")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1)) 

plot_country
ggsave(paste("outputs/time_diff", Sys.Date(), country, ".jpeg", sep="_"), plot)

## Countries according to website/social media timeliness ----------------
# Web earlier
df_all_country_web <- df_all_country %>% 
  filter(diff_min < 0)

# Social media earlier
df_all_country_smedia <- df_all_country %>% 
  filter(diff_min >= 0)

