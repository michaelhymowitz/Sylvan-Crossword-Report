#### Tibbles for Most Extreme Scores

#' extreme_scores(): Calculating frequency each person fell above or below a given crossword time, zscore, or rank
#'
#' df: dataframe
#' time_var: crossword time variable
#' sign: mathematical comparison operator
#' threshold: cutoff point for notion of extremity
extreme_scores <- function(df, time_var = c("time", "time_z", "rank"), sign = c(">", ">=", "<", "<=", "=="), threshold) {
    
    stopifnot(time_var %in% c("time", "time_z", "rank"))
    stopifnot(sign %in% c(">", ">=", "<", "<=", "=="))
    
    df %>%
        
        # concatenates (time_var, sign, and threshold) into a formula, which is evaluated to TRUE or FALSE
        mutate(met_threshold = eval(parse(text = str_c(time_var, sign, threshold, sep = " ")))) %>%
        
        group_by(name) %>%
        summarize(count = sum(met_threshold),
                  prop = mean(met_threshold)) %>%
        mutate(prop = percent(prop, accuracy = 0.1)) %>%
        arrange(name)
}

# Tibble for time_z > 2
extreme_scores(crossword_scores, time_var = "time_z", sign = "<", threshold = -.5)

# Tibble for time_z < -1
extreme_scores(crossword_scores, time_var = "time_z", sign = "<", threshold = -1)

# Tibble for time <= 60
extreme_scores(crossword_scores, time_var = "time", sign = "<=", threshold = 90)

# Tibble for rank <= 3
extreme_scores(crossword_scores, time_var = "rank", sign = "<=", threshold = 5)

