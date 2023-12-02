library("tidyverse")
library("testthat")

input <- readLines("input/day02.txt")

#put them back in the bag]
bag_games <- function(games, pop){
  get_colors <- function(x){
    map(c("red", "green", "blue"), ~str_extract_all(x, paste0("\\d+(?= ", .x, "(cubes|))"))) %>% setNames(c("red", "green", "blue"))
  }
  cubes <- get_colors(pop)

  possible_games <- imap_dfr(games, function(game, game_id){
    id <- str_match(game, "\\d+(?=:)")
    sets <- game %>% 
      str_remove(".+: ") %>% 
      strsplit(";") %>% 
      .[[1]] %>%
      map_dfr(function(x){
        map_dbl(get_colors(x), function(y){
          out <- y %>% unlist() %>% as.numeric() %>% sum()
        })
      }) %>% 
      mutate(game_id = game_id)
  }) %>% 
    group_by(game_id) %>% 
    filter(!any(red > cubes$red) & !any(green > cubes$green) & !any(blue > cubes$blue)) 
  
  sum(unique(possible_games$game_id))
}

expect_equal(bag_games(games = c("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                                 "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                                 "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                                 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                                 "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"), 
                       pop = "12 red cubes, 13 green cubes, and 14 blue cubes"), 
             8)

bag_games(input, "12 red cubes, 13 green cubes, and 14 blue cubes")

smol_bag_games <- function(games){
  get_colors <- function(x){
    map(c("red", "green", "blue"), ~str_extract_all(x, paste0("\\d+(?= ", .x, "(cubes|))"))) %>% setNames(c("red", "green", "blue"))
  }
  
  possible_games <- imap_dfr(games, function(game, game_id){
    id <- str_match(game, "\\d+(?=:)")
    sets <- game %>% 
      str_remove(".+: ") %>% 
      strsplit(";") %>% 
      .[[1]] %>%
      map_dfr(function(x){
        map_dbl(get_colors(x), function(y){
          out <- y %>% 
            unlist() %>% 
            as.numeric() %>% 
            sum()
        })
      }) %>% 
      mutate(game_id = game_id)
  }) %>% 
    group_by(game_id) %>% 
    summarise(across(c(red, blue, green), max)) %>% 
    mutate(power = red*blue*green)
  
  sum(possible_games$power)
}


expect_equal(smol_bag_games(games = c("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                                 "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                                 "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                                 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                                 "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")), 
             2286)

smol_bag_games(input)
