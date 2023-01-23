#### Tibbles for Best Weeks

#' best_weeks(): Calculating best individual or overall crossword weeks, according to average time or zscore
#' 
#' df: dataframe
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
best_weeks <- function(df, time_var = c("time", "time_z", "rank"), group_by_person) {
    
    stopifnot(time_var %in% c("time", "time_z", "rank"))
    catching_non_grouped_zscores_or_ranks(df, time_var, group_by_person)
    
    # helper function: calculates the table with min avg time_var
    best_weeks_calc <- function(df, time_var = c("time", "time_z", "rank")) {
        
        week_avg_time_var <- str_c("week_avg_", time_var)
        
        df %>%
            
            # only counting full 7-day weeks
            group_by(crossword_week) %>%
            mutate(num_days_in_week = length(unique(date))) %>%
            ungroup %>%
            filter(num_days_in_week == 7) %>%
            
            group_by(crossword_week) %>%
            mutate(week_avg_time = mean(time),
                   week_avg_time_z = mean(time_z),
                   week_avg_rank = mean(rank)) %>%
            ungroup %>%
            
            # Following line causes error for those who don;t have a 7-day week
            {
                if (nrow(.) == 0) {
                    (.)
                } else {
                    (.) %>%
                        filter(!!sym(week_avg_time_var) == min(!!sym(week_avg_time_var)))
                }
            } %>%
            
            group_by(crossword_week, date, weekday) %>%
            summarize(time = mean(time),
                      week_avg_time = first(week_avg_time),
                      time_z = mean(time_z),
                      week_avg_time_z = first(week_avg_time_z),
                      rank = mean(rank),
                      week_avg_rank = first(week_avg_rank),
                      .groups = "drop")
    }
    
    if (group_by_person) {
        
        # apply best_weeks_calc for each person
        map(names_vec,
            function(name_in) {
                df %>%
                    filter(name == name_in) %>%
                    best_weeks_calc(time_var = time_var) %>%
                    mutate(name = name_in) %>%
                    select(name, everything())
            })
    } else {
        
        # apply best_weeks_calc for overall
        best_weeks_calc(df, time_var = time_var) %>%
            {
                if (num_people_in_df(df) > 1) {
                    (.) %>%
                        select(-ends_with("time_z"), # time_z col will be 0 for mean overall z-score
                               -ends_with("rank")) # rank col will be 5 for mean overall rank
                } else {
                    (.)
                }
            }
    }
}

# Tibble for overall best week by time
best_weeks(crossword_scores, time_var = "time", group_by_person = FALSE)

# Tibbles for each person's best week by time
best_weeks(crossword_scores, time_var = "time", group_by_person = TRUE)

# Tibbles for each person's best week by zscore
best_weeks(crossword_scores, time_var = "time_z", group_by_person = TRUE)

# Tibbles for each person's best week by rank
best_weeks(crossword_scores, time_var = "rank", group_by_person = TRUE)

