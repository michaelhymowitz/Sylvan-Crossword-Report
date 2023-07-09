#### Tibbles for Most Consecutive Times Achieving a Feat

#' find_streaks(): Calculating most consecutive days in whic each person fell above or below a given
#'                 crossword time, zscore, or rank
#'
#' df: dataframe
#' time_var: crossword time variable
#' sign: mathematical comparison operator
#' threshold: cutoff point for notion of extremity TODO: put this is own script
#' return_just_longest: whether or not to just each person's longest streak
find_streaks <- function(df, time_var = c("time", "time_z", "rank"), sign = c(">", ">=", "<", "<=", "=="),
                         threshold, return_just_longest) {
    
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
        
        # if return_just_longest, the resulting table only has each person's longest streak
        {
            if (return_just_longest) {
                (.) %>%
                    filter(met_threshold_streak == max(met_threshold_streak))
            } else {
                (.)
            }
        } %>%
        
        ungroup %>%
        mutate(begin_date = date - met_threshold_streak + 1) %>%
        
        # fixing multiple length streaks starting on same day
        # so that, for example, if an 8-day streak begins on 12/21, there isn't a 
        # 1-day, 2-day, ..., 8-day streak don't all begin on same say
        {
            if (return_just_longest) {
                (.)
            } else {
                (.) %>%
                    group_by(name, begin_date) %>%
                    filter(met_threshold_streak == max(met_threshold_streak)) %>%
                    ungroup
            }
        } %>%
        
        rename(end_date = date) %>%
        select(name, begin_date, end_date, met_threshold_streak)
}

find_streaks(crossword_scores, time_var = "time_z", sign = "<=",
             threshold = -1, return_just_longest = FALSE) %>%
    arrange(desc(met_threshold_streak))
    # filter(name != "addieavery")






