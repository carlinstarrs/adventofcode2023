library("tidyverse")
library("testthat")

input <- readLines("input/day03.txt")


find_missing_part <- function(dat){ 
  symbol_coords <- imap_dfr(dat, ~tibble(start =str_locate_all(.x, "[^a-z|\\d|\\.]")[[1]][,1], 
                                         end = str_locate_all(.x, "[^a-z|\\d|\\.]")[[1]][,2],
                                         value = str_match_all(.x, "[^a-z|\\d|\\.]")[[1]][,1], 
                                         y = .y)) %>% 
    mutate(symbol_id = 1:n())
  
  number_coords <- imap_dfr(dat, ~tibble(start =str_locate_all(.x, "\\d+")[[1]][,1], 
                                         end = str_locate_all(.x, "\\d+")[[1]][,2],
                                         value = str_match_all(.x, "\\d+")[[1]][,1], 
                                         y = .y)) %>% 
    mutate(number_id = 1:n()) %>% 
    rowwise() %>% 
    mutate(x_range = list(c((start - 1):(end + 1)))) %>% 
    unnest(cols = c(x_range)) %>% 
    rowwise() %>% 
    mutate(y_range = list(c((y - 1):(y + 1)))) %>% 
    unnest(y_range) %>% 
    full_join(symbol_coords, by = c("x_range" = "start", "y_range" = "y"), suffix = c("_num", "_sym")) %>% 
    filter(!is.na(value_sym)) 
  
  part_nums <- sum(as.numeric(number_coords$value_num))
 
  #A gear is any * symbol that is adjacent to exactly two part numbers. 
  #Its gear ratio is the result of multiplying those two numbers together.
  gear_ratios <- number_coords %>% 
    filter(value_sym == "*") %>% #only * are gears
    group_by(symbol_id, y_range, x_range) %>% 
    mutate(values_per_part = n_distinct(number_id)) %>% 
    filter(values_per_part == 2) %>% #adjacent to EXACTLY 2
    summarise(value_sym = unique(value_sym), 
              value_num1 = as.numeric(value_num[1]), 
              value_num2 = as.numeric(value_num[2]),
              gear_ratio = value_num1 * value_num2, .groups = "drop")
  
  gear_sums <- sum(gear_ratios$gear_ratio)
  
  list("parts" = part_nums, 
       "gears" = gear_sums)
}


expect_equal(find_missing_part(c("467..114..",
                                 "...*......",
                                 "..35..633.",
                                 "......#...",
                                 "617*......",
                                 ".....+.58.",
                                 "..592.....",
                                 "......755.",
                                 "...$.*....",
                                 ".664.598.."))$parts, 4361)


expect_equal(find_missing_part(c("467..114..",
                                 "...*......",
                                 "..35..633.",
                                 "......#...",
                                 "617*......",
                                 ".....+.58.",
                                 "..592.....",
                                 "......755.",
                                 "...$.*....",
                                 ".664.598.."))$gears, 467835)


find_missing_part(input)
