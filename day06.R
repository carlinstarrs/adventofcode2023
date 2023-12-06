library("tidyverse")
library("testthat")
library("progress")

input <- read_table("input/day06.txt", col_names = FALSE) %>% 
  select(matches("X[2-9]")) %>% 
  t() %>% 
  as_tibble() %>% 
  rename_with(~c("time", "dist"))


race_boats <- function(dat, part2 = FALSE){ 
  out <- dat %>% 
    {if(part2)  t(dat) %>% 
        as_tibble() %>% 
        unite(col = "val", everything(), sep = "") %>% 
        t() %>%
        as_tibble() %>% 
        rename_with(~c("time", "dist")) else .} %>% 
    rowid_to_column("race_id") %>% 
    rowwise() %>% 
    mutate(time1 = list(tibble("hold_time" = c(0:time), 
                               "travel_time" = c(time:0)))) %>% 
    ungroup() %>% 
    unnest(time1) %>% 
    mutate(across(c(time, dist, hold_time, travel_time), as.numeric)) %>% 
    mutate(dist_traveled = hold_time * travel_time, 
           check = dist_traveled > dist) %>% 
    group_by(race_id) %>% 
    summarise(wins = sum(check), .groups = "drop")

    return(prod(out$wins))
}

expect_equal(race_boats(tribble(~time, ~dist,
                                7, 9,
                                15,40,
                                30,200)), 288)

race_boats(input)

expect_equal(race_boats(tribble(~time, ~dist, 
                                7, 9,
                                15,40,
                                30,200), part2 = TRUE), 71503)

race_boats(input, part2 = TRUE)
