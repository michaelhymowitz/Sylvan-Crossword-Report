#### Miscellaneous summary stats

# Summary stats for overall times
summary(crossword_scores$time)


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
