# Script information
#' Title:
#' Author: Laura Espinosa
#' Date created: 11 November 2021
#' Date modified: 13 March 2022

# Packages --------------
# install/load "pacman" to help installing and loading other packages
while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# Install and/or load packages
p_load(tidyverse, jsonlite, rjson, plyr, hms, gplots, plotly)

# Import data --------------
# Data from webscraper
df_web <- fromJSON(file = "data/website.json")

# Data from social media
df_smedia <- read_csv("data/social_media_timestamp.csv")
df_smedia$Date <- as.Date(df_smedia$Date, format = "%d/%m/%Y")

# data on countries per WHO region
who_reg <- read_csv("data/who_regions.csv")

## Clean website df --------------
# empty df to save time stamps
df_web_clean <- NULL

# df with time stamps, cases and country
for (i in c(1:length(df_web))) {
  # create dataframe for each country from the json file
  assign("df", 
         rownames_to_column(as.data.frame(do.call(rbind, df_web[[i]])), 
                            "datetime_web"))
  df <- df %>% 
    # create new variable or change format of existing variables
    mutate(Country = names(df_web)[i],
           datetime_web = as.POSIXct(df$datetime_web, format = "%Y-%m-%d_%H-%M-%OS"),
           date_web = as.Date(df$datetime_web),
           cases_web = V1) %>% 
    # delete old variable
    select(-V1)
  df$time_web <- as_hms(df$datetime_web, format = "%H:%M:%S")
  # Bind all dataframes from each country in an unique dataframe
  df_web_clean <- rbind(df_web_clean, df) 
}

# filter to keep one entry per day and one cum per country (just new entries when new cases reported)
df_web_clean <- df_web_clean %>% 
  arrange(Country, desc(cases_web), datetime_web) %>% 
  distinct(Country, date_web, .keep_all = TRUE) %>% 
  distinct(Country, cases_web, .keep_all = TRUE) %>% 
  left_join(who_reg) %>% 
  filter(Country!= "Cote_dIvoire_One") %>% 
  filter(Country!="South_Africa_One")

# rename countries
df_web_clean$Country[df_web_clean$Country == 'Cote_dIvoire_Two'] <- 'Cote_dIvoire'
df_web_clean$Country[df_web_clean$Country == 'South_Africa_Two'] <- 'South_Africa'
df_web_clean$Country[df_web_clean$Country == 'Nigerr'] <- 'Niger'


## Clean social media df ----------------
start_study <- as.Date("2022-01-31") # filter by start date of the study
#end_study <- as.Date("2022-02-24")

df_smedia_clean <- df_smedia %>% 
  mutate(
    date_smedia = as.Date(Date, format = "%d/%m/%Y"),
    time_smedia = as_hms(Time),
    datetime_smedia = case_when(is.na(time_smedia) ~ as.POSIXct(NA),
                                !is.na(time_smedia) & !is.na(date_smedia) ~ as.POSIXct(paste(date_smedia, time_smedia, sep = " "),
                                                                                       format = "%Y-%m-%d %H:%M:%OS"),
                                TRUE ~ as.POSIXct(NA))
  ) %>% 
  filter(date_smedia >= start_study) #%>% 
  #filter(date_smedia <= end_study)

# Countries in web and social media ---------------
# country names
df_names <- unique(df_web_clean$Country)
df_names_smedia <- unique(df_smedia$Country)

# Countries in social media not present in webscraper
missing <- setdiff(df_names_smedia, df_names)
missing

# Dates and countries with updates in website and/or social media-----------
# website
website_updates <- distinct(df_web_clean, Country, date_web) %>% 
  mutate(update_web = 1) %>% 
  complete(date_web, nesting(Country), fill = list(count_number = 1))

# social media
smedia_updates <- distinct(df_smedia_clean, Country, Date) %>% 
  mutate(update_smedia = 1) %>% 
  complete(Date, nesting(Country), fill = list(count_number = 1))

smedia_updates <- dplyr::rename(smedia_updates, "date_smedia" = "Date")
smedia_updates$date_smedia <- as.Date(smedia_updates$date_smedia,
                                      format = "%d/%m/%Y") 

# merge
web_smedia_updates <- full_join(website_updates, smedia_updates, 
                                by = c("date_web" = "date_smedia",
                                       "Country" = "Country")) %>% 
  left_join(who_reg) %>% 
  mutate(Date = date_web,
         Update = case_when(is.na(update_web) & is.na(update_smedia) ~ "1. No update", # no update
                            !is.na(update_web) & is.na(update_smedia)  ~ "2. Website update", # only website update
                            is.na(update_web) & !is.na(update_smedia) ~ "3. Social media update", # only social media update
                            !is.na(update_web) & !is.na(update_smedia) ~ "4. Social media & \nwebsite update", # website and social media update
                            TRUE ~ "None")) %>% 
  select(Date, Country, WHO_reg, Update) %>%  
  filter(Date >= start_study) %>% 
  arrange(Country, Date) 
  
plot_update <- ggplot(web_smedia_updates, aes(Date, Country)) +
  geom_tile(aes(fill = Update)) +
  scale_x_date(breaks = "1 days", date_labels = "%d %b %Y") +
  scale_fill_brewer(palette = 'YlOrBr') +
  ggtitle("Dates when website and/or social media platforms have been updated by the countries") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(xlim = c(min(web_smedia_updates$Date), 
                           max(web_smedia_updates$Date)))

plot_update

ggsave(paste("outputs/updates_asof", Sys.Date(), ".jpeg", sep=""), plot_update,
       width = 20, height = 10, units = "in")


plot_update_plotly <- ggplotly(plot_update)
plot_update_plotly


plot_update_EURO <- web_smedia_updates %>% 
  filter(WHO_reg == "Europe" | WHO_reg == "Europe/EUEEA") %>% 
  ggplot(aes(Date, Country)) +
  geom_tile(aes(fill = Update)) +
  scale_x_date(breaks = "1 days", date_labels = "%d %b %Y") +
  scale_fill_brewer(palette = 'YlOrBr') +
  ggtitle("Dates when website and/or social media platforms have been updated by the countries, WHO EURO region") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

plot_update_EURO

  
ggsave(paste("outputs/updates_EURO_asof", Sys.Date(), ".jpeg", sep=""), plot_update_EURO,
       width = 20, height = 10, units = "in")

plot_update_AFRO <- web_smedia_updates %>% 
  filter(WHO_reg == "Africa" & Date >= "2022-02-11") %>% 
  ggplot(aes(Date, Country)) +
  geom_tile(aes(fill = Update)) +
  scale_x_date(breaks = "1 days", date_labels = "%d %b %Y") +
  scale_fill_brewer(palette = 'YlOrBr') +
  ggtitle("Dates when website and/or social media platforms have been updated by the countries, WHO AFRO region") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

plot_update_AFRO


ggsave(paste("outputs/updates_AFRO_asof", Sys.Date(), ".jpeg", sep=""), plot_update_AFRO,
       width = 20, height = 10, units = "in")


## Merge both datasets ----------------------
df_all <- df_web_clean %>% 
  left_join(df_smedia_clean, by = c("Country"="Country", "date_web"="date_smedia", "WHO_reg"="WHO_reg")) %>% 
  select(-Source, -Deaths, -Comments) %>% 
  mutate(diff_min = round(difftime(datetime_web, datetime_smedia, units = "mins"), 2)) %>% 
  filter(datetime_web >= "2021-11-01") %>% 
  arrange(Country, datetime_web)

# Descriptive analysis ---------------
# merged dataset -----------
df_all_country <- df_all %>% 
  select(diff_min, Country, date_web, WHO_reg) %>% 
  group_by(Country) %>% 
  ungroup() %>% 
  # Create new variable with three categories depending on difference in minutes
  mutate(diff_min_num = as.numeric(diff_min),
         diff_min_cat = case_when(diff_min_num < 0 ~ "Website",
                                  diff_min_num == 0 ~ "No difference",
                                  diff_min_num > 0 ~ "Social media",
                                  TRUE ~ NA_character_))

## Plot with all countries and regions ------------
plot <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  geom_point(size = 2) +
  geom_point(aes(x = date_web, y = diff_min), alpha = 0) +
  # specific colour according to category
  scale_color_manual(values = c("Website" = "red", 
                                "No difference" = "black",
                                "Social media" = "blue")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous()+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO EURO and AFRO regions",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       color = "Source with earliest \ndaily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot # show plot

ggsave(paste("outputs/time_diff_allregions_", Sys.Date(), ".jpeg", sep=""), plot,
       width = 20, height = 10, units = "in")

## Plot for EURO ------------
plot_euro <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  filter(WHO_reg == "Europe" | WHO_reg == "Europe/EUEEA") %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  geom_point(size = 2) +
  geom_point(aes(x = date_web, y = diff_min), alpha = 0) +
  # specific colour according to category
  scale_color_manual(values = c("Website" = "red", 
                                "No difference" = "black",
                                "Social media" = "blue")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous()+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO EURO region",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       color = "Source with earliest \ndaily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot_euro # show plot

ggsave(paste("outputs/time_diff_euro_", Sys.Date(), ".jpeg", sep=""), plot_euro,
       width = 20, height = 10, units = "in")

## Plot for AFRO ------------
plot_afro <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  filter(WHO_reg == "Africa") %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  geom_point(size = 2) +
  geom_point(aes(x = date_web, y = diff_min), alpha = 0) +
  # specific colour according to category
  scale_color_manual(values = c("Website" = "red", 
                                "No difference" = "black",
                                "Social media" = "blue")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous()+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO AFRO region",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       color = "Source with earliest \ndaily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot_afro # show plot

ggsave(paste("outputs/time_diff_afro_", Sys.Date(), ".jpeg", sep=""), plot_afro,
       width = 20, height = 10, units = "in")


## Plot for a single country ------------
country = "Andorra"

## Plot for EURO ------------
plot_country <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  filter(Country == country) %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  geom_point(size = 2) +
  geom_point(aes(x = date_web, y = diff_min), alpha = 0) +
  # specific colour according to category
  scale_color_manual(values = c("Website" = "red", 
                                "No difference" = "black",
                                "Social media" = "blue")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous()+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO EURO region",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       color = "Source with earliest daily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot_country
ggsave(paste("outputs/time_diff", Sys.Date(), country, ".jpeg", sep="_"), plot_country,
       width = 20, height = 10, units = "in")

## Countries according to website/social media timeliness ----------------
# At least one day website earlier
df_all_country_web <- df_all_country %>% 
  filter(diff_min < 0)

unique(df_all_country_web$Country)

# At least one day social media earlier
df_all_country_smedia <- df_all_country %>% 
  filter(diff_min >= 0)

unique(df_all_country_smedia$Country)

# Countries with earliest updates always from social media
social_media_countries <- setdiff(unique(df_all_country_smedia$Country), unique(df_all_country_web$Country))
social_media_countries 
length(social_media_countries) #9

# Countries with earliest updates always from website
website_countries <- setdiff(unique(df_all_country_web$Country), unique(df_all_country_smedia$Country))
website_countries
length(website_countries) #10

# Countries with not a unique source being the earliest always
countries_unique_source <- append(social_media_countries, website_countries)
all_countries <- unique(df_all_country$Country)
countries_not_unique_source <- setdiff(all_countries, countries_unique_source)
countries_not_unique_source 
length(countries_not_unique_source) # 45

# Total countries

length(unique(df_all_country$Country)) #64

## EURO total
all_countries_euro <- df_all_country %>% 
  filter(WHO_reg == "Europe" | WHO_reg == "Europe/EUEEA") 

all_countries_euro <- unique(all_countries_euro$Country)
length(all_countries_euro) # 35

## AFRO total
all_countries_afro <- df_all_country %>% 
  filter(WHO_reg == "Africa") 

all_countries_afro <- unique(all_countries_afro$Country)
length(all_countries_afro) # 24
