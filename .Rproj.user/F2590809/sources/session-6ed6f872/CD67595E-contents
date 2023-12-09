library("tidyverse")
library("testthat")

input <- read_table("input/day09.txt", col_names = FALSE) %>% pmap(function(...) c(...))

oasis <- function(dat, part2 = FALSE){ 
  diff_seq <- dat
  ds_tr <- list(diff_seq)
  while(!all(diff_seq == 0)){
    diff_seq <- map2_dbl(head(diff_seq, -1), tail(diff_seq, -1) , ~.y - .x)
    ds_tr <- c(ds_tr, list(diff_seq))
  }

  if(!part2){
    for(i in length(ds_tr):1){
      if(i == length(ds_tr)){ 
        ds_tr[[i]] <- c(ds_tr[[i]], 0)
      } else {
        ds_tr[[i]] <- c(ds_tr[[i]], tail(ds_tr[[i]],1) + tail(ds_tr[[i+1]],1))
      }
    } 
  } else {
    for(i in length(ds_tr):1){
      # A needs to be the result of increasing 3 (the value to its left) by 0 
      #(the value below it); this means A must be 3:
      if(i == length(ds_tr)){ 
        ds_tr[[i]] <- c(0, ds_tr[[i]])
      } else {
        ds_tr[[i]] <- c(head(ds_tr[[i]],1) - head(ds_tr[[i+1]],1), ds_tr[[i]])
      }
    } 
  }
  
  placeholders <- map_dbl(ds_tr, ~ifelse(!part2, tail(.x, 1), head(.x, 1)))
  return(placeholders[1])
}

expect_equal(map(list(c(0, 3, 6, 9, 12, 15),
                      c(1, 3, 6, 10, 15, 21),
                      c(10, 13, 16, 21, 30, 45)), oasis) %>% unlist() %>% sum(), 114)


map(input, oasis) %>% unlist() %>% sum()

expect_equal(map(list(c(0, 3, 6, 9, 12, 15),
                      c(1, 3, 6, 10, 15, 21),
                      c(10, 13, 16, 21, 30, 45)), ~oasis(.x, part2 = TRUE)) %>% unlist() %>% sum(), 2)


map(input, ~oasis(.x, part2 = TRUE)) %>% unlist() %>% sum()

