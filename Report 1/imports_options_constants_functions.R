#' Loading in needed packages, declaring needed constants, and creating useful
#' functions for `sylvan_crossword_report.Rmd` and the scripts its sources

library(tidyverse)
library(lubridate)
library(readxl)
library(extraDistr) # discrete uniform dist
library(ggthemes) # theme_fivethirtyeight()
library(scales) # breaks_pretty() and percent()
library(mosaic) #zscore()
library(zoo) # rollapply()
library(janitor) # clean_names()

weekdays_vec <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Small edits to theme_fivethirtyeight()
theme_fivethirtyeight_mod <- function() {
    theme_fivethirtyeight() +
        theme(#legend.position = "right",
              #legend.direction = "vertical",
              legend.position = "none",
              axis.title = element_text(),
              axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))
}

crossword_scores_raw <- read_excel("sylvan_crossword_data.xlsx")



# Cleaning crossword_scores_raw and converting to longer
crossword_scores <- crossword_scores_raw %>%
    
    # converting times to seconds
    mutate(Date = as_date(Date),
           Weekday = factor(Weekday, weekdays_vec),
           across(`mike dale`:sgh, function(time) {
        min <- as.numeric(str_sub(time, end = 2))
        sec <- as.numeric(str_sub(time, start = 4))
        
        min * 60 + sec
    })) %>%
    pivot_longer(cols = `mike dale`:sgh, names_to = "Name", values_to = "Time") %>%
    clean_names %>%
    filter(name != "sgh") %>%
    na.omit %>%
    mutate(name = factor(name, sort(unique(.$name))), # allows for consistent plot coloring
           crossword_week = cut(date, "week", start.on.monday = FALSE)) %>% # weeks from Sun-Sat
    
    # standardizing and ranking scores for each date
    group_by(date) %>%
    mutate(time_z = zscore(time),
           rank = rank(time, ties.method = "min")) %>%
    ungroup %>%
    
    select(date:size, crossword_week, everything())

names_vec <- levels(crossword_scores$name)

#' vline_at_everydays_mean(): Plotting vertical line at the mean of a given variable for each weekday
#' 
#' df: dataframe
#' xint_var: crossword time variable
vline_at_everydays_mean <- function(df, xint_var = c("time", "time_z", "rank")) {
    
    stopifnot(xint_var %in% c("time", "time_z", "rank"))
    
    map(weekdays_vec,
        function(weekday_in) {
            geom_vline(data = df %>% filter(weekday == weekday_in),
                       aes(xintercept = mean(!!sym(xint_var))), linetype = "dotted")
        })
}

#' hline_at_means(): Plotting horizontal line at the mean of a given variable for each subplot
#' 
#' df: dataframe
#' yint_var: crossword time variable
#' filter_on: whether we are filtering on name, weekday, or both
hline_at_means <- function(df, yint_var = c("time", "time_z", "rank"),
                           filter_on = c("name", "weekday", "both")) {
    
    stopifnot(yint_var %in% c("time", "time_z", "rank"))
    stopifnot(filter_on %in% c("name", "weekday", "both"))
    
    vec <- if (filter_on == "name") {
        names_vec
    } else if (filter_on == "weekday") {
        weekdays_vec
    } else {
        cross2(weekdays_vec, names_vec)
    }
    
    map(vec,
        function(ele) {
            geom_hline(data = df %>% 
                           {
                               if (filter_on == "both") {
                                   (.) %>%
                                       filter(weekday == ele[[1]],
                                              name == ele[[2]])
                               } else {
                                   (.) %>%
                                       filter(!!sym(filter_on) == ele)
                               }
                           },
                       aes(yintercept = mean(!!sym(yint_var))),
                       color = ifelse(filter_on == "weekday", "red", "gray33"),
                       linetype = "dashed")
        })
}

#' generate_plot_title(): Generating plot titles
#' 
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
#' facet_by_weekday: whether or not to create different plots for each weekday
#' time_series: whether or not the data rpresents time-series data
generate_plot_title <- function(time_var, group_by_person, facet_by_weekday, time_series) {
    str_c(
        ifelse(!group_by_person, "Overall ", "Individual "),
        # case_when(str_detect(time_var, "cumulative") ~ "Cumulative Mean ",
        #           str_detect(time_var, "moving") ~ "Moving 7-Day Mean ",
        #           TRUE ~ ""),
        "Crossword ",
        case_when(time_var == "rank" ~ "Daily Ranks",
                  str_detect(time_var, "z") ~ "Z-Scores",
                  !str_detect(time_var, "z") ~ "Times"),
        ifelse(time_series, " by Date", ""),
        ifelse(facet_by_weekday, ", Grouped by Weekday", "")
    )
}

#' catching_non_grouped_zscores(): Looking at z-scores for entire group is pretty unmeaningful
#' 
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person 
catching_non_grouped_zscores_or_ranks <- function(df, time_var, group_by_person) {
    if (time_var %in% c("time_z", "rank") &
        !group_by_person &
        length(unique(df$name)) > 1) {
        stop(str_c("Overall group's average",
                   ifelse(time_var == "time_z", "z-score", "rank"),
                   "is not meaningful, will be",
                   ifelse(time_var == "time_z", 0, 5.5),
                   "every day",
                   sep = " "))
    }
}

#' round_up_to(): Round the maximum value of a vector up to the nearest value
#' 
#' vec: vector to find max value and round up to the nearest rount_to
#' round_to: what value to round max value of vec up to
round_up_to <- function(vec, round_to) {
    ceiling(max(vec) / round_to) * round_to
}

#' num_people_in_df(): Counts the number of people represented in df
#' 
#' df: dataframe
num_people_in_df <- function(df) {
    length(unique(df$name))
}

# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

# named vector from every person to their color
names_to_colors <- gg_color_hue(9) %>%
    set_names(names_vec)

