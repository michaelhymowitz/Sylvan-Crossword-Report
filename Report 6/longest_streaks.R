#### Tibbles for Most Consecutive Times Achieving a Feat

#' longest_streaks(): Calculating most consecutive days in whic each person fell above or below a given
#'                    crossword time, zscore, or rank
#'
#' df: dataframe
#' time_var: crossword time variable
#' sign: mathematical comparison operator
#' threshold: cutoff point for notion of extremity TODO: put this is own script
longest_streaks <- function(df, time_var = c("time", "time_z", "rank"), sign = c(">", ">=", "<", "<=", "=="), threshold) {
    
    stopifnot(time_var %in% c("time", "time_z", "rank"))
    stopifnot(sign %in% c(">", ">=", "<", "<=", "=="))
    
    df %>%
        arrange(name, date) %>%
        complete(name, date) %>%
        
        # concatenates (time_var, sign, and threshold) into a formula, which is evaluated to TRUE or FALSE
        mutate(met_threshold =  eval(parse(text = str_c(time_var, sign, threshold, sep = " "))),
               met_threshold = replace_na(met_threshold, FALSE)) %>%
        
        group_by(name) %>%
        mutate(met_threshold_streak = ave(met_threshold, cumsum(!met_threshold), FUN = cumsum)) %>%
        filter(met_threshold_streak == max(met_threshold_streak)) %>%
        ungroup %>%
        mutate(begin_date = date - met_threshold_streak + 1) %>%
        rename(end_date = date) %>%
        select(name, begin_date, end_date, met_threshold_streak)
}

longest_streaks(crossword_scores, time_var = "time", sign = "<=", threshold = 150) %>%
    arrange(desc(met_threshold_streak))
    # filter(name == "addieavery")




