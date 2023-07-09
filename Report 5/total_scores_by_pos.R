#### Total Scores Based on Position

#' create_total_scores_by_pos_table(): total scores for daily crossword scores,
#'                                     based either on daily ranking or daily
#'                                     number of fellow competitors bested
#' 
#' EX: finishing 1st out of 6 competitors means one bested 5 fellow competitors
#' 
#' df: dataframe
#' score_var: whether to create scores based on daily ranking or daily number of
#'            fellow competitors bested
#'            must be either "rank" or "num_people_beaten"
create_total_scores_by_pos_table <- function(crossword_scores_in, score_var) {
    
    stopifnot(score_var %in% c("rank", "num_people_beaten"))
    
    map_dfr(names_vec,
            function(name_in) {
                
                num_days <- crossword_scores_in %>%
                    .$date %>%
                    unique %>%
                    length
                
                crossword_scores_in %>%
                    
                    # calculating number of people beaten
                    # note that people who tied for last are given
                    # `num_people_beaten = 1`
                    mutate(num_people_beaten = n() - rank, .by = date) %>%
                    
                    filter(name == name_in) %>%
                    rename(score_col = !!score_var) %>%
                    .$score_col %>%
                    table %>% # frequency count
                    as_tibble %>%
                    rename(score_col = ".") %>%
                    mutate(score_col = as.integer(score_col)) %>%
                    
                    # fill in values the player never took on
                    # need to use num_solvers as opposed to max_ovr_rank
                    # as there may not be a single day where everyone
                    # completes the crossword
                    complete(score_col = full_seq(1:num_solvers - as.integer(score_var == "num_people_beaten"), period = 1),
                             fill = list("n" = 0)) %>%
                    
                    when(
                        score_var == "rank" ~ (.) %>%
                            mutate(total_score = n * (num_solvers - as.integer(score_col) + 1)),
                        score_var == "num_people_beaten" ~ (.) %>%
                            mutate(total_score = n * (as.integer(score_col) + 1))
                    ) %>%
                    
                    # this is needed due to next bind_rows() command
                    mutate(score_col = as.character(score_col)) %>%
                    
                    bind_rows(
                        tibble(score_col = "N/A",
                               n = num_days - sum(.$n),
                               total_score = 0)
                    ) %>%
                    mutate(name = name_in,
                           total_score_ovr = sum(total_score))
            }) %>%
        pivot_wider(id_cols = c(name, total_score_ovr),
                    names_from = score_col,
                    values_from = n) %>%
        arrange(desc(total_score_ovr)) %>%
        select(name, 
               if (score_var == "rank") {
                   as.character(1:num_solvers)
               } else {
                   as.character((num_solvers - 1):0)
               },
               "N/A", total_score_ovr) %>%
        rename(total_score = total_score_ovr)
}