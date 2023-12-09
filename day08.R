library("tidyverse")
library("testthat")

input <- "input/day08.txt"
test_input <- "input/day08_test_input.txt"
test_input2 <- "input/day08_test_input2.txt"

parse_input <- function(path){
  dirs <- readLines(path, n = 1) %>% str_split("") %>% .[[1]]
  dirs[dirs == "R"] <- 2
  dirs[dirs == "L"] <- 1
  
  nodes <- readLines(path) %>% tail(-2)
  nodes <- str_extract_all(nodes, "(\\d+|)[A-Z]+")
  nodes <- map(nodes, ~.x[2:3]) %>% setNames(map_chr(nodes, ~.x[[1]]))
  
  return(list("dirs" = dirs, 
              "nodes" = nodes))
}

navigate_desert <- function(dat){
  dirs <- dat$dirs %>% unlist()
  nodes <- dat$nodes
  
  steps <- c()
  dt <- 1
  while(!any(steps == "ZZZ")){
    for(i in seq_along(dirs)){
      if(i == 1 & dt == 1){ 
        node_name <- nodes$AAA[as.numeric(dirs[i])]
      } else {
        node_name <- nodes[node_name][[1]][as.numeric(dirs[i])]
      }
      steps <- c(steps, node_name)
    }
    
    dt <- dt + 1
  }
  
  return(length(steps))
}


expect_equal(navigate_desert(parse_input(test_input)), 2)
expect_equal(navigate_desert(parse_input(test_input2)), 6)

navigate_desert(parse_input(input))

navigate_desert_simultaneously <- function(dat){
  dirs <- dat$dirs %>% unlist()
  nodes <- dat$nodes

  steps_to_z <- function(start_node){
    steps <- list(list())
    dt <- 1
    while(!any(grepl("Z$", unlist(steps)))){
      for(i in seq_along(dirs)){
        if(i == 1 & dt == 1){ 
          node_names <- nodes[names(nodes) == start_node][[1]][as.numeric(dirs[i])]
        } else {
          node_names <- map(nodes[names(nodes) %in% node_names], ~.x[as.numeric(dirs[i])]) %>% unlist(use.names = FALSE)
        }
        steps <- c(steps, list(node_names))
      }
      
      dt <- dt + 1
    }
    
    return(which(grepl("Z$", unlist(steps))))
  }

  a_nodes <- names(nodes)[grepl("A", names(nodes))]
  all_steps <- map_dbl(a_nodes, steps_to_z)
  reduce(all_steps, pracma::Lcm) 
}

part2_test <- "input/day08_test_input3.txt"
navigate_desert_simultaneously(parse_input(part2_test))
navigate_desert_simultaneously(parse_input(input))
