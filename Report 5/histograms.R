#### Histograms for time

#' histogram_for_time(): Histograms for crossword times or zscores
#' 
#' df: dataframe
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
#' facet_by_weekday: whether or not to create different plots for each weekday
histogram_for_time <- function(df, time_var = c("time", "time_z"), group_by_person, facet_by_weekday) {
    
    stopifnot(time_var %in% c("time", "time_z"))
    catching_non_grouped_zscores_or_ranks(df, time_var, group_by_person)
    
    # max_bin_height has the size of the largest histogram group. If the max height is less than 5, adjustments
    # must be made to breaks_pretty() later on so that the y-axis does not display decimals
    max_bin_height <- df %>%
        
        # simulating histogram grouping for num_people_in_df(df) > 1
        mutate(hist_group = cut(!!sym(time_var),
                           breaks =
                               if (time_var == "time") {
                               seq(0, ceiling(max(df$time) / 60) * 60, by = ifelse(group_by_person, 60, 20))
                           } else {
                               seq(floor(min(df$time_z)), ceiling(max(df$time_z)), by = 1/3)
                           },
                           right = FALSE)) %>%
        
        
        {
            if (group_by_person) {
                (.) %>%
                    group_by(name)
            } else {
                (.)
            }
        } %>%
        {
            if (facet_by_weekday) {
                (.) %>%
                    group_by(weekday, .add = TRUE)
            } else {
                (.)
            }
        } %>%
        group_by(hist_group, .add = TRUE) %>%
        summarize(n = n(),
                  .groups = "drop") %>%
        .$n %>%
        max
    
    # default vline color (black) classes with histogram bars
    vline_color <- "darkgray"
    
    ggplot(df) +
        geom_histogram(aes(x = !!sym(time_var),
                           fill = if (group_by_person) name),
                       color = "black",
                       bins = if (num_people_in_df(df) == 1) 15,
                       binwidth = if (num_people_in_df(df) > 1) {
                           case_when(
                               time_var == "time_z" ~ 1/3,
                               time_var == "time" & group_by_person ~ 60,
                               time_var == "time" & !group_by_person ~ 20
                           )
                       }, 
                       boundary = 0,
                       closed = "left") +
        # add label above bars
        # stat_bin(aes(x = !!sym(time_var), label = stat(count), group = if (group_by_person) name), geom = "text",
        #          vjust = -0.5, size = 2.5, 
        #          binwidth = case_when(
        #              time_var == "time_z" ~ 1/3,
        #              time_var == "time" & group_by_person ~ 60,
        #              time_var == "time" & !group_by_person ~ 20
        #          )) +
        {
            if (time_var == "time") {
                scale_x_continuous(breaks = seq(0, round_up_to(df$time, 60),
                                                by = ifelse(group_by_person & num_people_in_df(df) > 1,
                                                             120, 60)),
                                   limits = c(0, NA))
            } else {
                scale_x_continuous(breaks = floor(min(df$time_z)):ceiling(max(df$time_z)),
                                   limits = c(floor(min(df$time_z)), ceiling(max(df$time_z))))
            }
        } +
        {
            if (num_people_in_df(df) == 1) {
                scale_y_continuous(breaks = breaks_pretty()) # removes potential decimal breaks
            } else {
                scale_y_continuous(breaks = breaks_pretty(n = min(max_bin_height + 1, 5)), # removes potential decimal breaks
                                   limits = c(0, max_bin_height + 1)) # doesn't cutoff max rank label
            }
        } +
        labs(title = generate_plot_title(time_var, group_by_person, facet_by_weekday,
                                         facet_by_num_solvers = FALSE, time_series = FALSE),
             x = ifelse(time_var == "time", "Time (sec)", "Z-Score"),
             y = "Count") +
        {
            if (num_people_in_df(df) == 1) { # TODO: add error checking so that if only 1 name, group by person is true (idk if need this, explore)
                scale_fill_manual(values = names_to_colors[unique(df$name)])
            }
        } +
        theme_fivethirtyeight_mod() +
        
        {
            if (facet_by_weekday) {
                list(
                    if (group_by_person) {
                        facet_grid(weekday~name)
                    } else {
                        facet_grid(weekday~.)
                    },
                    
                    # dotted vline at each weekday's mean
                    vline_at_everydays_mean(df, time_var, color = vline_color,
                                            linewidth = 1)
                )
            } else {
                list(
                    if (group_by_person) {
                        facet_grid(~name)
                    },
                    
                    # dotted vline at the overall mean
                    geom_vline(xintercept = mean(deframe(df[,time_var])),
                               color = vline_color, linetype = "dotted",
                               linewidth = 1)
                )
            }
        } +
        if (group_by_person & time_var == "time" & num_people_in_df(df) > 1) {
            # making seconds vertical
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        }
}

# Histogram of group's times
histogram_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = FALSE)

# Histogram of group's times for each weekday
histogram_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = TRUE)

# Histograms for each person's times
histogram_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = FALSE)

# Histograms for each person's times for each weekday
histogram_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = TRUE)

# Histograms for each person's zscores
histogram_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = FALSE)

# Histograms for each person's zscores for each weekday
histogram_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = TRUE)
