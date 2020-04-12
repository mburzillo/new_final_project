

rp_sub <- rp %>%
  filter(winner == 1) %>%
  filter(cityname %in% cities_to_keep) %>%
  filter(!(is.na(biggestsplit)))
