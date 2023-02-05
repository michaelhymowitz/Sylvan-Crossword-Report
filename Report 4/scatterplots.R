#### Scatterplots for time

#' scatterplot_for_time(): Histograms for crossword times or zscores, including cumulative or rolling stats
#' 
#' df: dataframe
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
#' facet_by_weekday: whether or not to create different plots for each weekday
#' add_mean_line: whether or not to add a vertical line at the mean
scatterplot_for_time <- function(df, time_var = c("time", "time_z", "rank"),
                                 group_by_person, facet_by_weekday, add_mean_line) {
    
    stopifnot(time_var %in% c("time", "time_z", "rank"))
    catching_non_grouped_zscores_or_ranks(df, time_var, group_by_person)
    
    df %>%
        # grouping time_var to be overall time if !group_by_person
        {
            if (!group_by_person & !facet_by_weekday) {
                (.) %>%
                    group_by(date) %>%
                    summarize(!!time_var := mean(!!sym(time_var)))
            } else if (!group_by_person & facet_by_weekday) {
                (.) %>%
                    group_by(date, weekday) %>%
                    summarize(!!time_var := mean(!!sym(time_var)),
                              .groups = "drop")
            } else {
                (.)
            }
        } %>%
        ggplot(aes(x = date, y = !!sym(time_var))) +
        geom_point(mapping = if (group_by_person) aes(color = name)) +
        
        # adding horizontal mean line for all subplots
        {
            if (add_mean_line) {
                if (group_by_person | facet_by_weekday) {
                    hline_at_means(df, yint_var = time_var,
                                   filter_on = case_when(
                                       group_by_person & facet_by_weekday ~ "both",
                                       group_by_person & !facet_by_weekday ~ "name",
                                       !group_by_person & facet_by_weekday ~ "weekday"))
                } else  {
                    geom_hline(yintercept = mean(deframe(df[,time_var])),
                               color = "red", linetype = "dashed")
                }
            }
        } +
        
        geom_smooth(color = ifelse(group_by_person, "black", "#3366FF"), # default ggplot blue or black
                    se = FALSE) +
        {
            if (time_var == "time") {
                scale_y_continuous(breaks = seq(0, round_up_to(df$time, 60),
                                               by = ifelse(facet_by_weekday, 120, 60)),
                                   limits = c(0, NA)) # ensures scatterplot for time starts at 0
            } else if (time_var == "time_z") {
                scale_y_continuous(breaks = floor(min(df$time_z)):ceiling(max(df$time_z)),
                                   limits = c(floor(min(df$time_z)),
                                              ceiling(max(df$time_z)))) # widens range of z-scores on axis
            } else if (time_var == "rank") {
                scale_y_continuous(breaks = 1:max_ovr_rank, limits = c(1, max_ovr_rank))
            }
        } +
        labs(title = generate_plot_title(time_var, group_by_person, facet_by_weekday,
                                         facet_by_num_solvers = FALSE, time_series = TRUE),
             x = "Date",
             y = case_when(
                 time_var == "time" ~ "Time (sec)",
                 time_var == "time_z" ~ "Z-Score",
                 time_var == "rank" ~ "Daily Rank"
             )) +
        {
            if (num_people_in_df(df) == 1) { # TODO: add error checking so that if only 1 name, group by person is true (idk if need this, explore)
                scale_color_manual(values = names_to_colors[unique(df$name)])
            }
        } +
        theme_fivethirtyeight_mod() +
        
        {
            if (group_by_person) {
                list(
                    if (facet_by_weekday) {
                        facet_grid(weekday~name)
                    } else {
                        facet_grid(~name)
                    },
                    if (num_people_in_df(df) > 1) {
                        # making dates diagonal
                        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
                    }
                )
            } else if (facet_by_weekday) {
                facet_grid(weekday~.)
            }
        }
}


# Scatterplot of group's average time wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = FALSE,
                     facet_by_weekday = FALSE, add_mean_line = TRUE)

# Scatterplot of group's average time for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = FALSE,
                     facet_by_weekday = TRUE, add_mean_line = TRUE)

# Scatterplot for each person's times wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = TRUE,
                     facet_by_weekday = FALSE, add_mean_line = TRUE)

# Scatterplot for each person's times for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = TRUE,
                     facet_by_weekday = TRUE, add_mean_line = TRUE)

# Scatterplot for each person's zscores wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE,
                     facet_by_weekday = FALSE, add_mean_line = TRUE)

# Scatterplot for each person's zscores for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE,
                     facet_by_weekday = TRUE, add_mean_line = TRUE)

# Scatterplot for each person's ranks wrt date
scatterplot_for_time(crossword_scores, time_var = "rank", group_by_person = TRUE,
                     facet_by_weekday = FALSE, add_mean_line = TRUE)

# Scatterplot for each person's ranks for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "rank", group_by_person = TRUE,
                     facet_by_weekday = TRUE, add_mean_line = TRUE)
