#### Miscellaneous summary stats

# Summary stats for overall times
summary(crossword_scores$time)


# Frequency count for each person in each rank
map_dfr(names_vec,
        function(name_in) {
            
            num_days <- crossword_scores %>%
                .$date %>%
                unique %>%
                length
            
            crossword_scores %>%
                filter(name == name_in) %>%
                .$rank %>%
                table %>% # frequency count
                as_tibble %>%
                rename(rank = ".") %>%
                mutate(total_rank_score = n * (length(names_vec) - as.integer(rank) + 1)) %>%
                bind_rows(
                    tibble(rank = "N/A",
                           n = num_days - sum(.$n),
                           total_rank_score = 0)
                ) %>%
                mutate(name = name_in,
                       total_score = sum(total_rank_score))
        }) %>%
    pivot_wider(id_cols = c(name, total_score), names_from = rank, values_from = n) %>%
    replace(is.na(.), 0) %>%
    select(name, as.character(1:length(names_vec)), "N/A", total_score)


# Average time before and after a given date (date is inclusive in first range)
cutoff_date <- as_date("2021-12-28")

crossword_scores %>%
    mutate(pre_cutoff = ifelse(date <= cutoff_date, "pre_cutoff", "post_cutoff")) %>%
    group_by(name, pre_cutoff) %>%
    summarize(avg_time = mean(time),
              avg_time_z = mean(time_z),
              avg_rank = mean(rank),
              .groups = "drop") %>%
    pivot_wider(id_cols = name, names_from = pre_cutoff, values_from = starts_with("avg_")) %>%
    mutate(change_in_time = avg_time_post_cutoff - avg_time_pre_cutoff,
           change_in_time_z = avg_time_z_post_cutoff - avg_time_z_pre_cutoff,
           change_in_rank = avg_rank_post_cutoff - avg_rank_pre_cutoff) %>%
    select(name,
           avg_time_pre_cutoff, avg_time_post_cutoff, change_in_time,
           avg_time_z_pre_cutoff, avg_time_z_post_cutoff, change_in_time_z,
           avg_rank_pre_cutoff, avg_rank_post_cutoff, change_in_rank)

crossword_scores %>%
    filter(size > 25) %>%
    arrange(time_z)

crossword_scores %>%
    filter(size > 25) %>%
    arrange(time)
