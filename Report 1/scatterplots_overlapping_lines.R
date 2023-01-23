#### Scatterplots for time

#' scatterplot_for_time(): Histograms for crossword times or zscores, including cumulative or rolling stats
#' 
#' df: dataframe
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
#' facet_by_weekday: whether or not to create different plots for each weekday
scatterplot2_for_time <- function(df, time_var = c("time", "time_z", "rank"),
                                 group_by_person, facet_by_weekday,
                                 add_cumulative_mean, add_moving_mean) {
    
    stopifnot(time_var %in% c("time", "time_z", "rank"))
    catching_non_grouped_zscores_or_ranks(time_var, group_by_person)
    
    df %>%
        {
            if (group_by_person & facet_by_weekday) {
                        (.) %>%
                            group_by(name, weekday)
                } else if (group_by_person & !facet_by_weekday) {
                    (.) %>%
                        group_by(name)
                } else if (!group_by_person & facet_by_weekday) {
                    (.) %>%
                        group_by(date, weekday) %>%
                        summarize(!!time_var := mean(!!sym(time_var)),
                                  .groups = "drop") %>%
                        group_by(weekday)
                } else if (!group_by_person & !facet_by_weekday) {
                    (.) %>%
                        group_by(date) %>%
                        summarize(!!time_var := mean(!!sym(time_var)))
                }
        } %>%
        mutate(!!time_var := !!sym(time_var),
               !!str_c(time_var, "_cumulative_mean") :=
                   cummean(!!sym(time_var)),
               !!str_c(time_var, "_moving_mean") :=
                   rollapply(!!sym(time_var), 7, mean, fill = NA,
                             align = "right", partial = TRUE)) %>%
        ungroup %>%
        ggplot(aes(x = date)) +
        geom_point(aes(y = !!sym(time_var), color = if (group_by_person) name)) +
        geom_smooth(aes(y = !!sym(time_var)),
                    color = ifelse(group_by_person, "black", "#3366FF"), se = FALSE) + # default ggplot blue or black
        {
            if (add_cumulative_mean) {
                geom_smooth(aes(y = !!sym(str_c(time_var, "_cumulative_mean"))),
                            color = ifelse(group_by_person, "black", "#3366FF"), se = FALSE,  # default ggplot blue or black
                            linetype = "dashed")
            }
        } +
        {
            if (add_moving_mean) {
                geom_smooth(aes(y = !!sym(str_c(time_var, "_moving_mean"))), 
                            color = ifelse(group_by_person, "black", "#3366FF"), se = FALSE,  # default ggplot blue or black
                            linetype = "dotted")
            }
        } +
        scale_y_continuous(breaks = if (time_var == "time") {
            seq(0, round_up_to(df$time, 60), # rounds up to nearest minute (nearest greater multiple of 60)
                by = ifelse(group_by_person & facet_by_weekday, 120, 60))
        } else if (time_var == "time_z") {
            floor(min(df$time_z)):ceiling(max(df$time_z))
        } else if (time_var == "rank") { 
            1:10
        }, limits = if (time_var == "time") {
            c(0, NA) # ensures scatterplot for time starts at 0
        }) +
        labs(title = generate_plot_title(time_var, group_by_person, facet_by_weekday, time_series = TRUE),
             x = "Date",
             y = case_when(
                 time_var == "time" ~ "Time (sec)",
                 time_var == "time_z" ~ "Z-Score",
                 time_var == "rank" ~ "Daily Rank"
             )) +
        theme_fivethirtyeight_mod() +
        
        {
            if (group_by_person) {
                list(
                    if (facet_by_weekday) {
                        facet_grid(weekday~name)
                    } else {
                        facet_grid(~name)
                    },
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
                )
            } else if (facet_by_weekday) {
                facet_grid(weekday~.)
            }
        }
}


# Scatterplot of group's average time wrt date
scatterplot2_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = FALSE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot of group's average time for each weekday wrt date
scatterplot2_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = TRUE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot for each person's times wrt date
scatterplot2_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = FALSE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot for each person's times for each weekday wrt date
scatterplot2_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = TRUE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot for each person's zscores wrt date
scatterplot2_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = FALSE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot for each person's zscores for each weekday wrt date
scatterplot2_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = TRUE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot for each person's ranks wrt date
scatterplot2_for_time(crossword_scores, time_var = "rank", group_by_person = TRUE, facet_by_weekday = FALSE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)

# Scatterplot for each person's ranks for each weekday wrt date
scatterplot2_for_time(crossword_scores, time_var = "rank", group_by_person = TRUE, facet_by_weekday = TRUE,
                      add_cumulative_mean = TRUE, add_moving_mean = TRUE)
