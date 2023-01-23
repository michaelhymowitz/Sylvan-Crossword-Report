#### Barplots for overall rank

#' barplot_for_rank(): Barplots for crossword daily ranks
#' 
#' df: dataframe
#' facet_by_weekday: whether or not to create different plots for each weekday
#' add_bar_label: whether or not to add count label at the top of each bar
barplot_for_rank <- function(df, facet_by_weekday, add_bar_label) {
    
    # max_rank has the max rank group size. If the max height is less than 5, adjustments
    # must be made to breaks_pretty() later on so that the y-axis does not display decimals
    max_rank <- df %>%
        {
            if (facet_by_weekday) {
                (.) %>%
                    group_by(weekday)
            } else {
                (.)
            }
        } %>%
        group_by(name, rank, .add = TRUE) %>%
        summarize(n = n(),
                  .groups = "drop") %>%
        .$n %>%
        max
    
    ggplot(df, aes(x = rank, fill = name)) +
        geom_bar(color = "black") +
        {
            # adding label count on top of bars
            if (add_bar_label) {
                geom_text(stat = "Count", aes(label = stat(count)), vjust = -0.5,
                          size = ifelse(num_people_in_df(df) == 1, 4, 2.25))
            }
        } +
        {
            # makes breaks integers
            if (num_people_in_df(df) == 1) {
                scale_x_continuous(breaks = 1:(length(names_vec)), limits = c(0.5, length(names_vec) + 0.5))
            } else {
                scale_x_continuous(breaks = breaks_pretty())
            }
        } +
        scale_y_continuous(breaks = breaks_pretty(n = min(max_rank + 1, 5)), # removes potential decimal breaks
                           limits = c(0, max_rank + 1)) + # doesn't cutoff max rank label
        labs(title = generate_plot_title(time_var = "rank", group_by_person = TRUE,
                                         facet_by_weekday, time_series = FALSE),
             x = "Rank",
             y = "Count") +
        {
            if (num_people_in_df(df) == 1) { # TODO: add error checking so that if only 1 name, group by person is true (idk if need this, explore)
                scale_fill_manual(values = names_to_colors[unique(df$name)])
            }
        } +
        theme_fivethirtyeight_mod() +
        
        {
            if (facet_by_weekday) {
                facet_grid(weekday~name)
            } else {
                facet_grid(~name)
            }
        }
}

# Barplots for each person's daily rank
barplot_for_rank(crossword_scores, facet_by_weekday = FALSE, add_bar_label = TRUE)

# Barplots for each person's daily rank for each weekday
barplot_for_rank(crossword_scores, facet_by_weekday = TRUE, add_bar_label = TRUE)
