library("tidyverse")
library("testthat")

input <- readLines("input/day04.txt")

scratchcard <- function(cards){ 
  out <- tibble(V1 = cards) %>% 
    separate(col = V1, into = c("p1", "p2"), sep = " \\| ") %>% 
    mutate(card = str_extract(p1, "Card( +)\\d+") %>% str_remove("Card ") %>% as.numeric(), 
           p1 = str_remove(p1, "Card( +)\\d+: +")) %>% 
    separate_rows(-card, -p2, sep = " +") %>% 
    separate_rows(p2, sep = " +") %>% 
    mutate(across(c(p1, p2), as.numeric)) %>% 
    filter(p1 == p2) %>% 
    group_by(card) %>% 
    mutate(points = rep(1, n()), 
           points = reduce(points, ~.x * 2)) %>% 
    select(card, points) %>% 
    unique()
  
  sum(out$points)
  
}

expect_equal(scratchcard(c("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
                           "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
                           "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
                           "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
                           "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
                           "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")), 13)


scratchcard(input)

infinite_scratchcards <- function(cards){
  #Specifically, you win copies of the scratchcards below the winning card equal to the number of matches
  all_cards <- tibble(V1 = cards) %>% 
    separate(col = V1, into = c("p1", "p2"), sep = " +\\| +") %>% 
    mutate(card = str_extract(p1, "Card( +)\\d+") %>% str_remove("Card ") %>% as.numeric(), 
           p1 = str_remove(p1, "Card( +)\\d+: +")) %>% 
    separate_rows(-card, -p2, sep = " +") %>% 
    separate_rows(p2, sep = " +") %>% 
    mutate(across(c(p1, p2), as.numeric)) %>% 
    group_by(card) %>% 
    mutate(matches = p1 == p2) %>% 
    summarise(total_matches = sum(matches)) %>%
    select(card, total_matches) 

  card_dict <- as.list(rep(0, nrow(all_cards)))
  card_tally <- as.list(rep(1, nrow(all_cards)))
  
  pwalk(all_cards, function(card, total_matches){
    card_dict[[as.numeric(card)]] <<- total_matches
  })
  
  
  for(i in seq_along(card_dict)){ 
    if(card_dict[[i]] != 0){
      next_cards <- (i+1):(i + card_dict[[i]])
    } else {
      next_cards <- 0
    } 
    
    how_many <- card_tally[[i]]
    card_tally[next_cards] <- map(card_tally[next_cards], ~.x + how_many)
  }
  

  #This process repeats until none of the copies cause you to win any more cards
  sum(card_tally %>% unlist())
  
  
}

expect_equal(infinite_scratchcards(c("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
                                     "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
                                     "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
                                     "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
                                     "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
                                     "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")), 30)


infinite_scratchcards(input)

