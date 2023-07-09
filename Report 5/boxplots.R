#### Boxplots for time and zscores of time

#' boxplot_for_time(): Boxplots for crossword times or zscores
#' 
#' df: dataframe
#' time_var: crossword time variable
#' group_by_person: whether or not to create different plots for each person
#' facet_by_weekday: whether or not to create different plots for each weekday
boxplot_for_time <- function(df, time_var = c("time", "time_z"), group_by_person, facet_by_weekday) {
    
    stopifnot(time_var %in% c("time", "time_z"))
    catching_non_grouped_zscores_or_ranks(df, time_var, group_by_person)
    
    ggplot(df, aes(x = !!sym(time_var),
                   y = if (group_by_person) fct_rev(name) else 0, # centering boxplot and scatterplot at 0
                   group = if (group_by_person) name,
                   color = if (group_by_person) name)) +
        geom_boxplot(outlier.shape = NA) +
        geom_point(position = position_jitter(height = 0.1, seed = 313736126), size = 0.5) + # scatterplots over boxplots
        {
            if (time_var == "time") {
                scale_x_continuous(breaks = seq(0, round_up_to(df$time, 60),
                                                by = ifelse(group_by_person & facet_by_weekday, 120, 60)),
                                   limits = c(0, NA)) # ensures scatterplot for time starts at 0
            } else {
                scale_x_continuous(breaks = floor(min(df$time_z)):ceiling(max(df$time_z)))
            }
        } +
        {
            if (!group_by_person) {
                scale_y_continuous(breaks = NULL) # removing meaningless breaks
            }
        } +
        labs(title = generate_plot_title(time_var, group_by_person, facet_by_weekday,
                                         facet_by_num_solvers = FALSE, time_series = FALSE),
             x = ifelse(time_var == "time", "Time (sec)", "Z-Score"),
             y = if (group_by_person) "Name") +
        {
            if (num_people_in_df(df) == 1) {
                scale_color_manual(values = names_to_colors[unique(df$name)])
            }
        } +
        theme_fivethirtyeight_mod() +
        
        {
            if (facet_by_weekday) {
                list(
                    if (group_by_person) {
                        facet_grid(~weekday)
                    } else {
                        facet_grid(weekday~.)
                    },
                    
                    # dotted vline at each weekday's mean
                    vline_at_everydays_mean(df, time_var),
                    
                    if (group_by_person & time_var == "time") {
                        # making seconds vertical
                        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
                    }
                )
            } else {
                
                # dotted vline at the overall mean
                geom_vline(xintercept = mean(deframe(df[,time_var])), linetype = "dotted")
            }
        }
}

# Boxplot of group's average time
boxplot_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = FALSE)

# Boxplot of group's average time for each weekday
boxplot_for_time(crossword_scores, time_var = "time", group_by_person = FALSE, facet_by_weekday = TRUE)

# Boxplots for each person's times
boxplot_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = FALSE)

# Boxplots for each person's time for each weekday
boxplot_for_time(crossword_scores, time_var = "time", group_by_person = TRUE, facet_by_weekday = TRUE)

# Boxplots for each person's zscores
boxplot_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = FALSE)

# Boxplots for each person's zscores for each weekday
boxplot_for_time(crossword_scores, time_var = "time_z", group_by_person = TRUE, facet_by_weekday = TRUE)

