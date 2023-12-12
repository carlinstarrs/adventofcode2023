library("tidyverse")
library("testthat")
library("progress")

input <- "input/day11.txt"
test_input <- "input/day11_test.txt"

parse_input <- function(dir){
  dat <- read_table(dir, col_names = FALSE) %>% 
    separate(col = "X1", sep = "", into = paste0("X", 0:nchar(.[1,]))) %>% 
    select(-1)

  empty_rows <- which(apply(dat == ".", 1, function(x) sum(x) == nrow(dat)) == TRUE)
  empty_cols <- which(apply(dat == ".", 2, function(x) sum(x) == ncol(dat)) == TRUE)
  
  iwalk(empty_rows, function(x,i){
    addl <- as_tibble(matrix(data = rep(".", ncol(dat)), nrow = 1), 
                      .name_repair = ~paste0("X", 1:ncol(dat)))
    dat <<- dat %>% 
      add_row(addl, .after = x+i-1)
  })
  
  iwalk(empty_cols %>% c(use.names = FALSE), function(x,i){
    addl <- as_tibble(matrix(data = rep(".", nrow(dat)), ncol = 1))
    
    dat <<- dat %>% 
      add_column(addl, .after = x+i-1, .name_repair = "unique")
  })
  
  
  dat %>% 
    rename_with(~paste0("X", 1:ncol(dat))) %>% 
    rowid_to_column("y") %>% 
    pivot_longer(-y, 
                 names_pattern = "(\\d+)", 
                 names_to = "x") %>% 
    mutate(check = value == "#", 
           id = cumsum(check), 
           value = case_when(value == "#" ~ id, 
                             TRUE ~ 0)) %>% 
    select(value, x, y) %>% 
    mutate(across(everything(), as.numeric))
}

travel_the_galaxy <- function(dat, part2 = FALSE){
  pairs <- combn(unique(dat$value[dat$value > 0]),2) %>% unique()
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) eta: :eta", total = ncol(pairs))
  map2_dbl(pairs[1,], pairs[2,], function(p1, p2){
    pb$tick(1)
    abs(dat$x[dat$value == p1]-dat$x[dat$value == p2]) + abs(dat$y[dat$value == p1]-dat$y[dat$value == p2])
  }) %>% sum()
}

expect_equal(travel_the_galaxy(parse_input(test_input)), 374)

travel_the_galaxy(parse_input(input))
  travel_the_galaxy(parse_input(input), part2 = TRUE)
