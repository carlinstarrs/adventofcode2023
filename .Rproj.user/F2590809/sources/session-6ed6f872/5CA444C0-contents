library("tidyverse")
library("testthat")
library("progress")

input <- read_table("input/day07.txt", col_names = FALSE) %>% 
  rename_with(~c("cards", "bet"))

score_camel_cards <- function(x){ 
  tally <- map_dbl(strsplit(x, "")[[1]], ~ str_count(x, .x))
  pair_tally <- table(tally)
  case_when(any(names(pair_tally) == 3) & any(names(pair_tally) == 2) ~ "full house", 
            all(tally == 1) ~ "high card",
            any(pair_tally == 4) & !any(tally == 4) ~ "two pair",
            pair_tally[1] == 3 & pair_tally[2] == 2 ~ "one pair",
            TRUE ~ paste(max(tally), "of a kind" ))
}

expect_equal(score_camel_cards("AAAAA"), "5 of a kind")
expect_equal(score_camel_cards("AA8AA"), "4 of a kind")
expect_equal(score_camel_cards("23332"), "full house")
expect_equal(score_camel_cards("TTT98"), "3 of a kind")
expect_equal(score_camel_cards("23432"), "two pair")
expect_equal(score_camel_cards("A23A4"), "one pair")
expect_equal(score_camel_cards("23456"), "high card")

play_camel_cards <- function(dat, part2 = FALSE){
  card_levels <- rev(c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"))
  type_levels <- rev(c("5 of a kind", "4 of a kind", "full house", "3 of a kind", 
                       "two pair", "one pair", "high card"))

  out <- dat %>% 
    rowid_to_column("hand") %>% 
    rowwise() %>% 
    mutate(type =  score_camel_cards(cards), 
           type = factor(type, levels = type_levels), 
           type2 = as.numeric(type)) %>% 
    ungroup() %>% 
    arrange(desc(type2)) %>% 
    separate(col = cards, sep = "", into = c(paste0("card", 0:5)), remove = FALSE) %>% 
    select(-card0) %>%
    mutate(across(matches("^card\\d"), ~as.numeric(factor(.x, levels = card_levels)))) %>% 
    arrange(desc(type2), desc(card1), desc(card2), desc(card3), desc(card4), desc(card5)) %>% 
    mutate(rank = n():1, 
           score = rank*as.numeric(bet))
  
  unique(out$type)
  
  return(sum(out$score))
}

expect_equal(play_camel_cards(tribble(~cards, ~bet, 
                                      "32T3K", 765,
                                      "T55J5", 684,
                                      "KK677", 28,
                                      "KTJJT", 220,
                                      "QQQJA", 483)), 6440)


play_camel_cards(input)

play_camel_wildcards <- function(dat){ 
  card_levels <- rev(c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"))
  type_levels <- rev(c("5 of a kind", "4 of a kind", "full house", "3 of a kind", 
                       "two pair", "one pair", "high card"))
  
  score_joker <- function(hand){
    tibble(cards = map_chr(card_levels[!grepl("J", card_levels)], ~str_replace_all(hand, "J", .x)),
           joker = card_levels[!grepl("J", card_levels)]) %>% 
      mutate(joker = factor(joker, levels = card_levels)) %>% 
      rowwise() %>% 
      mutate(type = score_camel_cards(cards), 
             type = factor(type, levels = type_levels), 
             type2 = as.numeric(type)) %>% 
      ungroup() %>% 
      arrange(desc(type2), desc(joker)) %>% 
      slice(1) %>% 
      pull(type)
  }
  
  out <- dat %>% 
    rowid_to_column("hand") %>% 
    rowwise() %>% 
    mutate(type = case_when(!grepl("J", cards) ~ score_camel_cards(cards), 
                            TRUE ~ score_joker(cards)), 
           type = factor(type, levels = type_levels), 
           type2 = as.numeric(type)) %>% 
    ungroup() %>% 
    arrange(desc(type2)) %>% 
    separate(col = cards, sep = "", into = c(paste0("card", 0:5)), remove = FALSE) %>% 
    select(-card0) %>%
    mutate(across(matches("^card\\d"), ~as.numeric(factor(.x, levels = card_levels)))) %>% 
    arrange(desc(type2), desc(card1), desc(card2), desc(card3), desc(card4), desc(card5)) %>% 
    mutate(rank = n():1, 
           score = rank*as.numeric(bet))
  
  return(sum(out$score))
}


expect_equal(play_camel_wildcards(tribble(~cards, ~bet, 
                                      "32T3K", 765,
                                      "T55J5", 684,
                                      "KK677", 28,
                                      "KTJJT", 220,
                                      "QQQJA", 483)), 5905)


play_camel_wildcards(input)
