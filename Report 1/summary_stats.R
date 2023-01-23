#### Summary stats

#' summary_stats(): Summary stats for crossword times, zscores, or daily ranks
#'
#' df: dataframe
#' summary_var: crossword time variable to summarize
summary_stats <- function(df, summary_var = c("time", "time_z", "rank")) {
    
    stopifnot(summary_var %in% c("time", "time_z", "rank"))
    
    map_dfr(names_vec,
            function(name_in) {
                
                # getting quantiles of summary_var
                quants <- df %>%
                    filter(name == name_in) %>%
                    .[,summary_var] %>%
                    deframe %>%
                    summary
                
                # combining quantiles and trimmed mean into a one row tibble
                tibble(name = name_in,
                       min = deframe(quants[1]),
                       quant1 = deframe(quants[2]),
                       med = deframe(quants[3]),
                       mean = deframe(quants[4]),
                       trim_mean = if (summary_var != "rank") {
                           df %>%
                               filter(name == name_in) %>%
                               .[,summary_var] %>%
                               deframe %>%
                               mean(trim = 0.05)
                       },
                       quant3 = deframe(quants[5]),
                       max = deframe(quants[6]))
            })
}

# Summary stats for overall times
summary(crossword_scores$time)

# Summary stats for each person's times
summary_stats(crossword_scores, "time")

# Summary stats for each person's zscores
summary_stats(crossword_scores, "time_z")

# Summary stats for each person's ranks
summary_stats(crossword_scores, "rank")

# Frequency count for each person in each rank
map_dfr(names_vec,
        function(name_in) {
            crossword_scores %>%
                filter(name == name_in) %>%
                .$rank %>%
                table %>% # frequency count
                as_tibble %>%
                rename(rank = ".") %>%
                pivot_wider(names_from = rank, values_from = n) %>%
                mutate(name = name_in)
        }) %>%
    replace(is.na(.), 0) %>%
    mutate(total_points =
               8 * `1` + 7 * `2` + 6 * `3` + 5 * `4` +
               4 * `5` + 3 * `6` + 2 * `7` + 1 * `8`) %>%
    select(name, as.character(1:length(names_vec)), total_points)
