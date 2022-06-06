# Script information
#' Title: Timeliness of COVID-19 public sources
#' Author: Laura Espinosa
#' Date created: 11 November 2021
#' Date modified: 1 May 2022

# Packages --------------
# install/load "pacman" to help installing and loading other packages
while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# Install and/or load packages
p_load(tidyverse, rjson, hms, flextable, rstatix, webshot, 
       ggpubr, coin, janitor)

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
  filter(Country!= "Cote_dIvoire_One") %>% 
  filter(Country!="South_Africa_One")

# rename countries
df_web_clean$Country[df_web_clean$Country == 'Cote_dIvoire_Two'] <- 'Cote_dIvoire'
df_web_clean$Country[df_web_clean$Country == 'South_Africa_Two'] <- 'South_Africa'
df_web_clean$Country[df_web_clean$Country == 'Nigerr'] <- 'Niger'

# join WHO regions
df_web_clean <- df_web_clean %>% 
  left_join(who_reg)


## Clean social media df ----------------
start_study <- as.Date("2022-01-31") # filter by start date of the study

df_smedia_clean <- df_smedia %>% 
  mutate(
    date_smedia = as.Date(Date, format = "%d/%m/%Y"),
    time_smedia = as_hms(Time),
    datetime_smedia = case_when(is.na(time_smedia) ~ as.POSIXct(NA),
                                !is.na(time_smedia) & !is.na(date_smedia) ~ as.POSIXct(paste(date_smedia, time_smedia, sep = " "),
                                                                                       format = "%Y-%m-%d %H:%M:%OS"),
                                TRUE ~ as.POSIXct(NA))
  ) %>% 
  filter(date_smedia >= start_study) 

# Countries in web and social media ---------------
# country names
df_names <- unique(df_web_clean$Country)
df_names_smedia <- unique(df_smedia$Country)

# Countries in social media not present in webscraper
missing <- setdiff(df_names_smedia, df_names)

if(length(missing)==0){
  print("No missing countries in the webscraper")
} else {
  if(length(missing)==1){
    print(paste0(missing, " country missing in the webscraper"))
  } else {
    print(paste0(missing, " countries missing in the webscraper"))
  }
}

# Dates and countries with updates in website and/or social media-----------
# website
website_updates <- df_web_clean %>% 
  filter(!is.na(time_web)) %>% 
  distinct(Country, date_web) %>% 
  mutate(update_web = 1) %>% 
  complete(date_web, nesting(Country), fill = list(count_number = 1)) %>% 
  mutate(update_web = replace(update_web, 
                                 is.na(update_web), 0))

# social media
smedia_updates <- df_smedia_clean %>% 
  filter(!is.na(Time)) %>% 
  distinct(Country, date_smedia) %>% 
  mutate(update_smedia = 1) %>% 
  complete(date_smedia, nesting(Country), fill = list(count_number = 1)) %>% 
  mutate(update_smedia = replace(update_smedia, 
                                 is.na(update_smedia), 0))

#smedia_updates <- dplyr::rename(smedia_updates, "date_smedia" = "Date")

# merge
web_smedia_updates <- full_join(website_updates, smedia_updates, 
                                by = c("date_web" = "date_smedia",
                                       "Country" = "Country")) %>% 
  left_join(who_reg) %>% 
  mutate(Date = date_web,
         Update = case_when(update_web == 0 & update_smedia == 0 ~ "1. No update", # no update
                            update_web == 1 & update_smedia == 0  ~ "2. Website update", # only website update
                            update_web == 0 & update_smedia ==1 ~ "3. Social media update", # only social media update
                            update_web == 1 & update_smedia == 1 ~ "4. Social media & \nwebsite update", # website and social media update
                            TRUE ~ "None"),
         Filter = case_when(WHO_reg == "Africa" & Date <= "2022-02-12" ~ "filter", # to delete entries outside study period
                            TRUE ~ "keep")) %>% 
  filter(Filter == "keep") %>% 
  select(Date, Country, WHO_reg, Update) %>%  
  filter(Date >= start_study) %>% 
  arrange(Country, Date) 
  
## summary table ----------
updates_tab <- web_smedia_updates %>% 
  group_by(WHO_reg, Update) %>% 
  tally() 

updates_total <- updates_tab %>% 
  dplyr::group_by(WHO_reg) %>% 
  dplyr::summarise(n = sum(n))

set_flextable_defaults(background.color = "white")

updates_merge_table1 <- updates_tab %>% 
  full_join(updates_total, by = c("WHO_reg", "n")) %>% 
  arrange(WHO_reg) %>% 
  mutate(Update = case_when(is.na(Update) ~ "Overall",
                            TRUE ~ Update),
         Percentage = case_when(WHO_reg == "Africa" ~ paste(round((n / filter(updates_total, WHO_reg == "Africa")$n * 100), 
                                                            digits = 1), "%", sep = " "),
                                WHO_reg == "Europe/EU-EEA" ~ paste(round((n / filter(updates_total, WHO_reg == "Europe/EU-EEA")$n * 100), 
                                                                   digits = 1), "%", sep = " "),
                                WHO_reg == "Europe/Non-EU-EEA" ~ paste(round((n / filter(updates_total, WHO_reg == "Europe/Non-EU-EEA")$n * 100), 
                                                                       digits = 1), "%", sep = " "),
                                TRUE ~ NA_character_)) %>% 
  dplyr::rename("Region" = WHO_reg,
                "Number of entries" = n) %>% 
  flextable() %>% 
  bold(bold = TRUE, part = "header") %>% 
  #autofit() %>% 
  width(width = c(1.8, 1.5, 1.5, 1)) %>% 
  #align_nottext_col(align = "center", header = TRUE) %>% 
  align(j = c(3,4), align = "center", part = "all") 

updates_merge_table1

flextable::save_as_image(updates_merge, 'outputs/table1.jpeg')

## plot with updates ----------------------------
plot_update_fig1 <- ggplot(web_smedia_updates, aes(Date, Country)) +
  geom_tile(aes(fill = Update)) +
  scale_x_date(breaks = "4 days", date_labels = "%d %b %Y") +
  scale_fill_manual(values = c("white", "yellow", "orange", "red")) +
  #ggtitle("Dates when website and/or social media platforms have been updated by the countries") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 16),
        axis.text.y = element_text(size = 16),
        title = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 16),
        legend.text=element_text(size=16)) +
  coord_cartesian(xlim = c(min(web_smedia_updates$Date), 
                           max(web_smedia_updates$Date))) +
  facet_wrap(~ WHO_reg, scales = 'free_y', ncol = 3)

plot_update_fig1

ggsave("outputs/fig1.jpeg", plot_update_fig1, width = 20, height = 10, units = "in")

plot_update_EURO <- web_smedia_updates %>% 
  filter(WHO_reg == "Europe/Non-EU-EEA" | WHO_reg == "Europe/EU-EEA") %>% 
  ggplot(aes(Date, Country)) +
  geom_tile(aes(fill = Update)) +
  scale_x_date(breaks = "1 days", date_labels = "%d %b %Y") +
  scale_fill_manual(values = c("white", "yellow", "orange", "red")) +
  ggtitle("Dates when website and/or social media platforms have been updated by the countries, WHO EURO region") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 16),
        axis.text.y = element_text(size = 16),
        title = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text=element_text(size=16)) 

plot_update_EURO

  
ggsave("outputs/updates_EURO.jpeg", plot_update_EURO, width = 20, height = 10, units = "in")

plot_update_AFRO <- web_smedia_updates %>% 
  filter(WHO_reg == "Africa" & Date >= "2022-02-11") %>% 
  ggplot(aes(Date, Country)) +
  geom_tile(aes(fill = Update)) +
  scale_x_date(breaks = "1 days", date_labels = "%d %b %Y") +
  scale_fill_manual(values = c("white", "yellow", "orange", "red")) +
  ggtitle("Dates when website and/or social media platforms have been updated by the countries, WHO AFRO region") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 16),
        axis.text.y = element_text(size = 16),
        title = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text=element_text(size=16)) 

plot_update_AFRO


ggsave("outputs/updates_AFRO.jpeg", plot_update_AFRO, width = 20, height = 10, units = "in")


## Merge both datasets ----------------------
df_all <- df_web_clean %>% 
  left_join(df_smedia_clean, by = c("Country"="Country", "date_web"="date_smedia", "WHO_reg"="WHO_reg")) %>% 
  select(-Source, -Comments) %>% 
  mutate(diff_min = round(difftime(datetime_web, datetime_smedia, units = "mins"), 2)) %>% 
  #filter(datetime_web >= "2021-11-01") %>% 
  arrange(Country, datetime_web)

# Descriptive analysis ---------------
## merged dataset -----------
df_all_country <- df_all %>% 
  select(diff_min, Country, date_web, WHO_reg) %>% 
  group_by(Country) %>% 
  ungroup() %>% 
  # Create new variable with three categories depending on difference in minutes
  mutate(diff_min_num = as.numeric(diff_min),
         diff_min_cat = case_when(diff_min_num < 0 ~ "Website",
                                  diff_min_num == 0 ~ "No difference",
                                  diff_min_num > 0 ~ "Social media",
                                  TRUE ~ NA_character_),
         diff_min_num = abs(diff_min_num)) %>% 
  filter(!is.na(diff_min_num))

## diff per source, country and region ------------------
diff_stats <- df_all_country %>% 
  #mutate(diff_min_num = abs(diff_min_num)) %>% 
  dplyr::group_by(diff_min_cat, WHO_reg) %>% 
  dplyr::summarise("Median \n(min)" = round(median(diff_min_num), digits = 2),
                   "Quartile 1 (Q1) \n(min)" = round(quantile(diff_min_num, prob = 0.25, na.rm = TRUE), digits = 2),
                   "Quartile 3 (Q3) \n(min)" = round(quantile(diff_min_num, prob = 0.75, na.rm = TRUE), digits = 2),
                   "Interquartile range \n(Q3-Q1) (min)" = round(IQR(diff_min_num, na.rm = TRUE), digits = 2)) %>% 
  filter(!is.na(diff_min_cat)) %>% 
  filter(diff_min_cat != "No difference") %>% 
  dplyr::rename("Earliest source" = diff_min_cat,
         "WHO region" = WHO_reg) %>% 
  flextable() %>% 
  bold(bold = TRUE, part = "header") %>% 
  width(width = 1.5) %>% 
  align_nottext_col(align = "center", header = TRUE)
  
diff_stats


## Plot with all countries and regions ------------
plot <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min_num)) %>% 
  arrange(desc(diff_min_num)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min_num)) +
  geom_line(size = 1, colour = "light grey") +
  #geom_point(size = 2, shape = diff_min_cat) +
  geom_point(aes(x = date_web, y = diff_min_num, shape = diff_min_cat, colour = diff_min_cat)) +
  # specific colour according to category
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous(n.breaks = 4)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO EURO and AFRO regions",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)")+
  #guides(colour =guide_legend(title = "Earliest shource")) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot # show plot

ggsave("outputs/time_diff_allregions.jpeg", plot, width = 20, height = 10, units = "in")

## Plot for EURO ------------
plot_euro <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  filter(WHO_reg == "Europe/Non-EU-EEA" | WHO_reg == "Europe/EU-EEA") %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min_num)) +
  geom_line(size = 1, colour = "light grey") +
  #geom_point(size = 2, shape = diff_min_cat) +
  geom_point(aes(x = date_web, y = diff_min_num, shape = diff_min_cat, colour = diff_min_cat)) +
  # specific colour according to category
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous(n.breaks = 4)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO European region",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       legend = "Source with earliest \ndaily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot_euro # show plot

ggsave("outputs/time_diff_euro.jpeg", plot_euro, width = 20, height = 10, units = "in")

## Plot for AFRO ------------
plot_afro <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  filter(WHO_reg == "Africa") %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min_num)) +
  geom_line(size = 1, colour = "light grey") +
  #geom_point(size = 2, shape = diff_min_cat) +
  geom_point(aes(x = date_web, y = diff_min_num, shape = diff_min_cat, colour = diff_min_cat)) +
  # specific colour according to category
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous(n.breaks = 4)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO African region",
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       legend = "Source with earliest \ndaily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_wrap(~ Country, scales = 'free_y', ncol = 6)

plot_afro # show plot

ggsave("outputs/time_diff_afro.jpeg", plot_afro, width = 20, height = 10, units = "in")


## Plot for a single country ------------
country = "Andorra"

## Plot for EURO ------------
plot_country <- df_all_country %>%
  # delete NAs
  filter(!is.na(diff_min)) %>% 
  filter(Country == country) %>% 
  arrange(desc(diff_min)) %>% 
  # plot categories by colour
  ggplot(aes(x = date_web, y = diff_min_num, colour = diff_min_cat)) +
  geom_line(size = 1, colour = "light grey") +
  #geom_point(size = 2, shape = diff_min_cat) +
  geom_point(aes(x = date_web, y = diff_min_num, shape = diff_min_cat, colour = diff_min_cat)) +
  # specific colour according to category
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_x_date(breaks = "5 days") +
  scale_y_continuous(n.breaks = 4)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  theme_classic() +
  labs(title = paste("Time difference (minutes) between website pages and social media posts \non COVID-19 cases in ", country, sep = ""),
       x = "Date (year, month and day)",
       y = "Time difference (minutes)",
       legend = "Source with earliest \ndaily update")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) 

plot_country
ggsave(paste("outputs/time_diff", country, ".jpeg", sep="_"), plot_country,
       width = 20, height = 10, units = "in")

## Countries and updates without time component ----------------------------
df_all_notime <- df_all_country %>% 
  select(Country, diff_min_num, diff_min_cat, WHO_reg) %>% 
  group_by(Country) %>% 
  filter(!is.na(diff_min_num))

### t-tests --------------
#### Europe/Non-EU-EEA ----------------------
df_all_notime_euro <- df_all_notime %>% 
  ungroup() %>% 
  filter(diff_min_cat != "No difference" & WHO_reg == "Europe/Non-EU-EEA") %>% 
  select(diff_min_cat, diff_min_num) %>% 
  mutate(diff_min_num = abs(diff_min_num)) 

df_all_notime_euro %>% 
  group_by(diff_min_cat) %>% 
  get_summary_stats(diff_min_num, type = "median_iqr")

stat_euro <- df_all_notime_euro %>% 
  rstatix::wilcox_test(diff_min_num ~ diff_min_cat, conf.level = 0.95) %>% 
  add_significance() 
stat_euro

stat_euro_size <- df_all_notime_euro %>% 
  wilcox_effsize(diff_min_num ~ diff_min_cat)
stat_euro_size

euro_plot_notime <- ggplot(df_all_notime_euro, aes(diff_min_cat, diff_min_num)) +
  geom_boxplot() +
  stat_pvalue_manual(stat_euro %>% add_xy_position(x = "diff_min_cat"), tip.length = 0) +
  labs(x = "Earliest source",
       y = "Difference with the other source (min)",
       title = "Non-EU/EEA countries from WHO European region",
       subtitle = get_test_label(stat_euro, detailed = TRUE)) +
  theme_bw() +
  theme(plot.title = element_text(size = 12))
euro_plot_notime

#### Europe/EU-EEA ----------------------
df_all_notime_eueea <- df_all_notime %>% 
  ungroup() %>% 
  filter(diff_min_cat != "No difference" & WHO_reg == "Europe/EU-EEA") %>% 
  select(diff_min_cat, diff_min_num) %>% 
  mutate(diff_min_num = abs(diff_min_num)) 

df_all_notime_eueea %>% 
  group_by(diff_min_cat) %>% 
  get_summary_stats(diff_min_num, type = "median_iqr")

stat_eueea <- df_all_notime_eueea %>% 
  rstatix::wilcox_test(diff_min_num ~ diff_min_cat, conf.level = 0.95) %>% 
  add_significance() 
stat_eueea

stat_eueea_size <- df_all_notime_eueea %>% 
  wilcox_effsize(diff_min_num ~ diff_min_cat)
stat_eueea_size

eueea_plot_notime <- ggplot(df_all_notime_eueea, aes(diff_min_cat, diff_min_num)) +
  geom_boxplot() +
  stat_pvalue_manual(stat_eueea %>% add_xy_position(x = "diff_min_cat"), tip.length = 0) +
  labs(x = "Earliest source",
       y = "Difference with the other source (min)",
       title = "EU/EEA countries from WHO European region",
       subtitle = get_test_label(stat_eueea, detailed = TRUE)) +
  theme_bw() +
  theme(plot.title = element_text(size = 12))

eueea_plot_notime

#### Africa ----------------------
df_all_notime_afro <- df_all_notime %>% 
  ungroup() %>% 
  filter(diff_min_cat != "No difference" & WHO_reg == "Africa") %>% 
  select(diff_min_cat, diff_min_num) %>% 
  mutate(diff_min_num = abs(diff_min_num)) 

df_all_notime_afro %>% 
  group_by(diff_min_cat) %>% 
  get_summary_stats(diff_min_num, type = "median_iqr")

stat_afro <- df_all_notime_afro %>% 
  rstatix::wilcox_test(diff_min_num ~ diff_min_cat, conf.level = 0.95) %>% 
  add_significance() 
stat_afro

stat_afro_size <- df_all_notime_afro %>% 
  wilcox_effsize(diff_min_num ~ diff_min_cat)
stat_afro_size

afro_plot_notime <- ggplot(df_all_notime_afro, aes(diff_min_cat, diff_min_num)) +
  geom_boxplot() +
  stat_pvalue_manual(stat_afro %>% add_xy_position(x = "diff_min_cat"), tip.length = 0) +
  labs(x = "Earliest source",
       y = "Difference with the other source (min)",
       title = "Countries from WHO African region",
       subtitle = get_test_label(stat_afro, detailed = TRUE)) +
  theme_bw() +
  theme(plot.title = element_text(size = 12))

afro_plot_notime

### All regions ------------
stat_all <- rbind(stat_euro, stat_eueea, stat_afro) 
stat_all %>% 
  mutate(p = formatC(p, format = "e", digits = 2)) %>% 
  flextable()

plot_all_notime_fig3 <- ggarrange(euro_plot_notime, eueea_plot_notime, afro_plot_notime, ncol = 3)
plot_all_notime_fig3

ggsave("outputs/fig3.jpeg", plot_all_notime_fig3, width = 20, height = 10, units = "in")

### Plots ---------------
plot_notime_fig2 <- df_all_notime %>% 
  filter(diff_min_cat != "No difference") %>% 
  mutate(diff_min_num = abs(diff_min_num)) %>% 
  ggplot(aes(x = Country, y = diff_min_num, colour = diff_min_cat)) +
  geom_boxplot() +
  #geom_box(size = 2, shape = 5) +
  scale_color_manual(values = c("Website" = "red", 
                                "Social media" = "blue")) +
  labs(title = "Time difference (minutes) between website pages and social media posts \non COVID-19 cases in WHO European and African regions",
       x = "Countries",
       y = "Time difference (minutes)",
       color = "Source with earliest \ndaily update")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.text = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14)) +
  facet_grid(diff_min_cat ~ WHO_reg, scales = "free")
  
plot_notime_fig2

ggsave("outputs/fig2.jpeg", plot_notime_fig2, width = 20, height = 10, units = "in")

## Countries according to website/social media timeliness ----------------
# Table with earliest source per WHO region
df_all_notime_total <- df_all_notime %>% 
  dplyr::group_by(WHO_reg, diff_min_cat) %>% 
  tally() %>%  
  dplyr::group_by(WHO_reg) %>% 
  summarise(n = sum(n)) 

df_all_notime_table2 <- df_all_notime %>% 
  dplyr::group_by(WHO_reg, diff_min_cat) %>% 
  tally() %>% 
  full_join(df_all_notime_total) %>% 
  arrange(WHO_reg) %>% 
  mutate(diff_min_cat = case_when(is.na(diff_min_cat) ~ "Overall",
                            TRUE ~ diff_min_cat),
         Percentage = case_when(WHO_reg == "Africa" ~ paste(round((n / filter(df_all_notime_total, WHO_reg == "Africa")$n * 100), 
                                                                  digits = 1), "%", sep = " "),
                                WHO_reg == "Europe/EU-EEA" ~ paste(round((n / filter(df_all_notime_total, WHO_reg == "Europe/EU-EEA")$n * 100), 
                                                                         digits = 1), "%", sep = " "),
                                WHO_reg == "Europe/Non-EU-EEA" ~ paste(round((n / filter(df_all_notime_total, WHO_reg == "Europe/Non-EU-EEA")$n * 100), 
                                                                             digits = 1), "%", sep = " "),
                                TRUE ~ NA_character_)) %>% 
  rename("Earliest source" = diff_min_cat,
         "Region" = WHO_reg, "Number of entries" = n) %>% 
  flextable() %>% 
  bold(bold = TRUE, part = "header") %>% 
  width(width = c(1.8, 1.5, 1.5, 1.5)) %>% 
  align(j = c(3,4), align = "center", part = "all") 
  

df_all_notime_table2


flextable::save_as_image(df_all_notime_table2, 'outputs/table2.jpeg')

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
length(social_media_countries) #7

# Countries with earliest updates always from website
website_countries <- setdiff(unique(df_all_country_web$Country), unique(df_all_country_smedia$Country))
website_countries
length(website_countries) #9

# Countries with not a unique source being the earliest always
countries_unique_source <- append(social_media_countries, website_countries)
all_countries <- unique(df_all_country$Country)
countries_not_unique_source <- setdiff(all_countries, countries_unique_source)
countries_not_unique_source 
length(countries_not_unique_source) # 47

# Total countries
length(unique(df_all_country$Country)) #63

## EURO total
all_countries_euro <- df_all_country %>% 
  filter(WHO_reg == "Europe/Non-EU-EEA" | WHO_reg == "Europe/EU-EEA") 

all_countries_euro <- unique(all_countries_euro$Country)
length(all_countries_euro) # 34

## AFRO total
all_countries_afro <- df_all_country %>% 
  filter(WHO_reg == "Africa") 

all_countries_afro <- unique(all_countries_afro$Country)
length(all_countries_afro) # 29
