library("tidyverse")
library("testthat")
library("progress")

test_input <- read_table("input/day05_test.txt", col_names = FALSE)
input <- read_table("input/day05.txt", col_names = FALSE)


produce_food <- function(dat, part2 = FALSE){
  seed_mapper <- function(x){ 
    print(unique(x$db))
    this_seeds <- seeds
    
    pmap(x, function(destination, source, range, ...){  
      dest <- c(destination, (destination + range - 1))
      srce <- c(source, (source + range - 1)) 

      pb <- progress_bar$new(format = "[:bar] :current/:total eta: :eta", total = length(seeds), width = 40)
      iwalk(seeds, function(seed, i){ 
        pb$tick()
        if(between(seed, srce[1], srce[2])){
          this_seeds[i] <<- (dest[2]-srce[2]) + seed
        }
      })
    })
    
    seeds <<- this_seeds

  dbs <- dat %>% 
    mutate(db = case_when(grepl("\\W", X1) ~ X1, 
                          TRUE ~ as.character(NA))) %>% 
    fill(db, .direction = "down") %>% 
    mutate(across(c(X1, X2), as.numeric), 
           id = consecutive_id(db)) 
  
  seeds <- dbs %>% 
    slice(1) %>% 
    select(-db, -id, -X1) %>% 
    unlist(use.names = FALSE)

  seed_db <- dbs %>% 
    slice(-1) %>% 
    select(X1, X2, X3, db, id) %>% 
    drop_na() %>% 
    rename_with(~c("destination", "source", "range", "db", "id"), .cols = everything()) 
    
    seed_db %>% 
    group_by(id) %>% 
    group_split() 
  
  seed_db %>% 
    map(seed_mapper)
  
  #get min destination for each table and the min value from the previous mapping
  # if(part2){
  #   min_seed <- c()
  #   these_seeds <- tibble(seed_start = seeds[seq(1,length(seeds), by = 2)],
  #                         seed_range = seeds[seq(2,length(seeds), by = 2)]) %>% 
  #     mutate(seed_id = 1:n()) %>% 
  #     group_by(seed_id) %>% 
  #     group_split() 
  # 
  #   pmap(these_seeds, function(seed_start, seed_range, ...){ 
  #     seeds <<- seed_start:(seed_start + seed_range - 1)
  # 
  #     this_min_seed <- seed_db %>% map(seed_mapper)
  # 
  #     min_seed <<- min(c(min_seed, min(seeds)))
  # 
  #   })
  #}
  
  
  if(!part2){
    part1 <- seed_db %>% 
      map(seed_mapper)
    return(min(seeds))
  }
  
}

expect_equal(produce_food(test_input), 35)

produce_food(input)

expect_equal(produce_food(test_input, part2 = TRUE), 46)
produce_food(input, part2 = TRUE)
