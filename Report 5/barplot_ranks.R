#### Barplots for overall rank

#' barplot_for_rank(): Barplots for crossword daily ranks
#' 
#' df: dataframe
#' facet_by_weekday: whether or not to create different plots for each weekday
#' facet_by_num_solvers: whether or not to create different plots for the number of total puzzle solvers
#' add_bar_label: whether or not to add count label at the top of each bar
barplot_for_rank <- function(df, facet_by_weekday, facet_by_num_solvers, add_bar_label) {
    
    # Adding total number of puzzle solvers to `df`
    df_w_num_solvers <- df %>%
        mutate(num_solvers = n(), .by = date) # need to use `n()` in the event of ties for last-place
    
    # Can only facet by one of `facet_by_weekday` or `facet_by_num_solvers`
    stopifnot(!(facet_by_weekday & facet_by_num_solvers))
    
    # max_rank has the max rank group size. If the max height is less than 5, adjustments
    # must be made to breaks_pretty() later on so that the y-axis does not display decimals
    max_rank <- df_w_num_solvers %>%
        {
            if (facet_by_weekday) {
                (.) %>%
                    group_by(weekday)
            } else if (facet_by_num_solvers) {
                (.) %>%
                    group_by(num_solvers)
            } else {
                (.)
            }
        } %>%
        group_by(name, rank, .add = TRUE) %>%
        summarize(n = n(),
                  .groups = "drop") %>%
        .$n %>%
        max
    
    ggplot(df_w_num_solvers, aes(x = rank, fill = name)) +
        geom_bar(color = "black") +
        {
            # adding label count on top of bars
            if (add_bar_label) {
                geom_text(stat = "Count", aes(label = after_stat(count)), vjust = -0.5,
                          size = ifelse(num_people_in_df(df_w_num_solvers) == 1, 4, 1))
            }
        } +
        {
            # makes breaks integers
            if (num_people_in_df(df_w_num_solvers) == 1) {
                scale_x_continuous(breaks = 1:max_ovr_rank, limits = c(0.5, max_ovr_rank + 0.5))
            } else {
                scale_x_continuous(breaks = breaks_pretty())
            }
        } +
        
        {
            if (facet_by_num_solvers) {
                # https://stackoverflow.com/questions/11353287/how-do-you-add-a-general-label-to-facets-in-ggplot2
                scale_y_continuous(breaks = breaks_pretty(n = min(max_rank + 1, 5)), # removes potential decimal breaks
                                   limits = c(0, max_rank + 1), # doesn't cutoff max rank label
                                   sec.axis = sec_axis(~ . , name = "Total # of Puzzle Solvers",
                                                       breaks = NULL, labels = NULL))
            } else {
                scale_y_continuous(breaks = breaks_pretty(n = min(max_rank + 1, 5)), # removes potential decimal breaks
                                   limits = c(0, max_rank + 1)) # doesn't cutoff max rank label
            }
        } +
        
        labs(title = generate_plot_title(time_var = "rank", group_by_person = TRUE,
                                         facet_by_weekday, facet_by_num_solvers, time_series = FALSE),
             x = "Rank",
             y = "Count") +
        {
            if (num_people_in_df(df_w_num_solvers) == 1) { # TODO: add error checking so that if only 1 name, group by person is true (idk if need this, explore)
                scale_fill_manual(values = names_to_colors[unique(df_w_num_solvers$name)])
            }
        } +
        theme_fivethirtyeight_mod() +
        
        {
            if (facet_by_weekday) {
                facet_grid(weekday~name)
            } else if (facet_by_num_solvers) {
                facet_grid(num_solvers~name)
            } else {
                facet_grid(~name)
            }
        }
}

# Barplots for each person's daily rank
barplot_for_rank(crossword_scores, facet_by_weekday = FALSE, facet_by_num_solvers = FALSE, add_bar_label = TRUE)

# Barplots for each person's daily rank for each weekday
barplot_for_rank(crossword_scores, facet_by_weekday = TRUE, facet_by_num_solvers = FALSE, add_bar_label = TRUE)
