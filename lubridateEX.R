x2 <- diets.scat %>%
  pivot_longer(
    cols = krill_type:squid_type,
    names_to = "Species",
    values_to = "Presence"
  ) %>% 
  # arrange(collection_date, sample_num) %>% 
  group_by(season_name, collection_date) %>% 
  summarize(n_samples = n()) %>% 
  ungroup() %>% 
  mutate(c_date_str = paste(if_else(month(collection_date) > 7, 1999, 2000), 
                            month(collection_date), 
                            day(collection_date), 
                            sep = "-"), 
         c_date_new = ymd(c_date_str))

ggplot(x2, aes(c_date_new, n_samples)) +
  geom_point() + 
  geom_path() + 
  facet_wrap(vars(season_name))

