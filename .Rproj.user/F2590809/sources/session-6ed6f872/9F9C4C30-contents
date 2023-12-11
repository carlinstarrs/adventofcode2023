library("tidyverse")
library("testthat")
library("igraph")
library("future")
library("furrr")

input <- "input/day10.txt"
test1 <- "input/day10_test1.txt"
test2 <- "input/day10_test2.txt"
test3 <- "input/day10_test3.txt"
test4 <- "input/day10_test4.txt"
parse_input <- function(dir){
  dat <- readLines(dir)
  grid_width <- nchar(dat[1])
  
  dat %>% 
    tibble() %>% 
    separate(col = 1, sep = "", into = paste0("X", 1:(grid_width+1))) %>% 
    select(-1) %>% 
    rowid_to_column("y") %>% 
    pivot_longer(-y) %>% 
    mutate(x = as.numeric(str_extract(name, "\\d+"))-1) %>% 
    rowid_to_column("from") %>% 
    select(pipe = value, x, y, from) 
}

plan(multisession)

loop_pipes <- function(dat){
  start_val <- dat %>% filter(pipe == "S") %>% pull(from)
  
  out <- dat %>% 
    filter(pipe != ".") %>% 
    future_pmap_dfr(function(pipe, x, y, from){ 
      to <- case_when(pipe == "|" ~ list(list(c(x, y+1), c(x, y-1))), #south and north
                      pipe == "-" ~ list(list(c(x-1, y), c(x+1, y))), #west and east
                      pipe == "L" ~ list(list(c(x+1, y), c(x, y-1))), #east and north
                      pipe == "J" ~ list(list(c(x, y-1), c(x-1, y))), #north and west
                      pipe == "7" ~ list(list(c(x-1, y), c(x, y+1))), #west and south
                      pipe == "F" ~ list(list(c(x, y+1), c(x+1, y))), #south and east 
                      pipe == "S" ~ list(list(c(x+1, y), c(x-1, y), c(x, y+1), c(x, y-1))), #all directions
                      TRUE ~ list(pipe)) %>% 
        unlist(recursive = F) %>% 
        map(function(coord){
          next_coord <- dat %>% filter(x == coord[1], y == coord[2])
          if(nrow(next_coord) == 0) return(NULL)
          if(next_coord$pipe == ".") return(NULL)
          return(next_coord$from)
        }) %>% 
        compact()
      
      
      out <- tibble(pipe, x, y, from, to) %>% 
        unnest(to)
      
      dat %>% filter(from %in% out$to)
      
      return(out)
    }) 
  
  #remove invalid directions starting from start
  s_vals <- out %>% 
    filter(pipe == "S") %>% 
    mutate(CHECK = map2_lgl(from, to, function(from, to, ...){
      nrow(out[out$to == from & out$from == to,]) > 0
    })) %>% 
    filter(CHECK == TRUE) %>% 
    select(-CHECK)
  
  out <- out %>% 
    filter(pipe != "S") %>% 
    bind_rows(s_vals)
  
  #assign unique ids
  vids <- dat %>%
    select(pipe, from) %>%
    mutate(pipe_name = paste0(pipe, "_", from))
  
  out <- out %>%
    left_join(vids %>% select(pipe_from = pipe_name, from), by = c("from")) %>%
    left_join(vids %>% select(pipe_to = pipe_name, to = from), by = c("to"))
 
  #make graph
  g <- igraph::make_graph(edges = pmap(out, function(pipe_from, pipe_to, ...) c(pipe_from, pipe_to)) %>% unlist(recursive = F), directed = TRUE)
  
  #get neighbors of S so we know which ones have ended in a loop
  neighbors <- ego(g, order = 1, nodes = V(g)[names(V(g)) == paste0("S_", start_val)]) %>% 
    unlist() %>% 
    names()
  
  paths <- all_simple_paths(g, 
                            from = V(g)[names(V(g)) == paste0("S_", start_val)], 
                            to = V(g)[names(V(g)) %in% neighbors])
  
  #only paths with length > 2 and that end at a neighbor count
  valid_paths <- paths %>% 
    map(function(path){ 
      this_path <- path %>% unlist() %>% names() 
      if(length(this_path) > 2 & tail(this_path,1) %in% neighbors){
        return(this_path)
      }
    }) %>% 
    compact()
  
  sub_graphs <- lapply(valid_paths, function(vs) induced_subgraph(g, vs))
  
  aa <- as_long_data_frame(sub_graphs[[1]])
  plot(sub_graphs[[1]])
  
  plot(valid_paths[[1]])
  browser()
  return(max(map_dbl(valid_paths, length))/2)
}

expect_equal(loop_pipes(parse_input(test1)), 4)
expect_equal(loop_pipes(parse_input(test3)), 4)
expect_equal(loop_pipes(parse_input(test2)), 8)
expect_equal(loop_pipes(parse_input(test4)), 8)

loop_pipes(parse_input(input))
