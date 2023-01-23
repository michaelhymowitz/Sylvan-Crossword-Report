#### Scatterplots for time

#' scatterplot_for_time(): Histograms for crossword times or zscores, including cumulative or rolling stats
#' 
#' df: dataframe
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
#' facet_by_weekday: whether or not to create different plots for each weekday
scatterplot_for_time <- function(df, time_var = c("time", "time_z", "rank",
                                                  "time_cumulative_mean", "time_z_cumulative_mean", "rank_cumulative_mean",
                                                  "time_moving_mean", "time_z_moving_mean", "rank_moving_mean"),
                                 group_by_person, facet_by_weekday) {
    
    stopifnot(time_var %in% c("time", "time_z", "rank",
                              "time_cumulative_mean", "time_z_cumulative_mean", "rank_cumulative_mean",
                              "time_moving_mean", "time_z_moving_mean", "rank_moving_mean"))
    catching_non_grouped_zscores_or_ranks(time_var, group_by_person)
    
    # time or time_z
    time_var_base <- str_extract(time_var, "^(time(_z|)|rank)")
    
    df %>%
        {
            if (!str_detect(time_var, "(cumulative)|(moving)") & group_by_person) {
                (.)
            } else {
                {
                    # grouping by the proper variables and summarizing if grouping by date
                    if (group_by_person & facet_by_weekday) {
                        (.) %>%
                            group_by(name, weekday)
                    } else if (group_by_person & !facet_by_weekday) {
                        (.) %>%
                            group_by(name)
                    } else if (!group_by_person & facet_by_weekday) {
                        (.) %>%
                            group_by(date, weekday) %>%
                            summarize(!!time_var_base := mean(!!sym(time_var_base)),
                                      .groups = "drop") %>%
                            group_by(weekday)
                    } else if (!group_by_person & !facet_by_weekday) {
                        (.) %>%
                            group_by(date) %>%
                            summarize(!!time_var_base := mean(!!sym(time_var_base)))
                    }
                } %>%
                    
                    # calculating the statistic of interest
                    mutate(!!time_var := if (!str_detect(time_var, "(cumulative)|(moving)")) {
                        !!sym(time_var)
                    } else if (str_detect(time_var, "cumulative")) {
                        # cumulative mean of time or time_z
                        cummean(!!sym(time_var_base))
                    } else if (str_detect(time_var, "moving")) {
                        # moving mean of time or time_z
                        rollapply(!!sym(time_var_base), 7, mean, fill = NA, align = "right", partial = TRUE)
                    }) %>%
                    ungroup
            }
        } %>%
        ggplot(aes(x = date, y = !!sym(time_var))) +
        geom_point(mapping = if (group_by_person) aes(color = name)) +
        geom_smooth(color = ifelse(group_by_person, "black", "#3366FF"), se = FALSE) + # default ggplot blue or black
        scale_y_continuous(breaks = if (time_var_base == "time") {
            seq(0, round_up_to(df$time, 60), # rounds up to nearest minute (nearest greater multiple of 60)
                by = ifelse(group_by_person & facet_by_weekday, 120, 60))
        } else if (time_var_base == "time_z") {
            floor(min(df$time_z)):ceiling(max(df$time_z))
        } else if (time_var_base == "rank") { 
            1:10
        }, limits = if (time_var == "time") {
            c(0, NA) # ensures scatterplot for time starts at 0
        }) +
        labs(title = generate_plot_title(time_var, group_by_person, facet_by_weekday, time_series = TRUE),
             x = "Date",
             y = str_c(
                 case_when(
                     str_detect(time_var, "cumulative") ~ "Cumulative Mean ",
                     str_detect(time_var, "moving") ~ "Moving 7-Day Mean ",
                     TRUE ~ ""
                 ),
                 case_when(
                     time_var_base == "time" ~ "Time (sec)",
                     time_var_base == "time_z" ~ "Z-Score",
                     time_var_base == "rank" ~ "Daily Rank"
                 )
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
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = FALSE)

# Scatterplot of group's average time for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = TRUE)

# Scatterplot for each person's times wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's times for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = TRUE)

# Scatterplot for each person's zscores wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's zscores for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = TRUE)

# Scatterplot for each person's ranks wrt date
scatterplot_for_time(crossword_scores, time_var = "rank", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's ranks for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "rank", group_by_person = TRUE, facet_by_weekday = TRUE)


# Scatterplot of group's cumulative average time wrt date
scatterplot_for_time(crossword_scores, time_var = "time_cumulative_mean", group_by_person = FALSE, facet_by_weekday = FALSE)

# Scatterplot of group's cumulative average time for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_cumulative_mean", group_by_person = FALSE, facet_by_weekday = TRUE)

# Scatterplot for each person's cumulative average time wrt date
scatterplot_for_time(crossword_scores, time_var = "time_cumulative_mean", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's cumulative average time for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_cumulative_mean", group_by_person = TRUE, facet_by_weekday = TRUE)

# Scatterplot for each person's cumulative average zscore wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z_cumulative_mean", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's cumulative average zscore for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z_cumulative_mean", group_by_person = TRUE, facet_by_weekday = TRUE)

# Scatterplot for each person's cumulative average rank wrt date
scatterplot_for_time(crossword_scores, time_var = "rank_cumulative_mean", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's cumulative average rank for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "rank_cumulative_mean", group_by_person = TRUE, facet_by_weekday = TRUE)



# Scatterplot of group's moving 7-day average time wrt date
scatterplot_for_time(crossword_scores, time_var = "time_moving_mean", group_by_person = FALSE, facet_by_weekday = FALSE)

# Scatterplot of group's moving 7-day average time for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_moving_mean", group_by_person = FALSE, facet_by_weekday = TRUE)

# Scatterplot for each person's moving 7-day average time wrt date
scatterplot_for_time(crossword_scores, time_var = "time_moving_mean", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's moving 7-day average time for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_moving_mean", group_by_person = TRUE, facet_by_weekday = TRUE)

# Scatterplot for each person's moving 7-day average zscore wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z_moving_mean", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's moving 7-day average zscore for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "time_z_moving_mean", group_by_person = TRUE, facet_by_weekday = TRUE)

# Scatterplot for each person's moving 7-day average rank wrt date
scatterplot_for_time(crossword_scores, time_var = "rank_moving_mean", group_by_person = TRUE, facet_by_weekday = FALSE)

# Scatterplot for each person's moving 7-day average rank for each weekday wrt date
scatterplot_for_time(crossword_scores, time_var = "rank_moving_mean", group_by_person = TRUE, facet_by_weekday = TRUE)

