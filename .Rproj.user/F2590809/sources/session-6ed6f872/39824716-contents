library("tidyverse")
library("testthat")

input <- readLines("input/day01.txt")


calibration_value <- function(x){
  map(x, ~str_extract_all(.x, "\\d{1}")[[1]]) %>% 
    map(~paste0(c(head(.x, 1), tail(.x, 1)), collapse = "")) %>% 
    unlist() %>% 
    as.numeric() %>% 
    sum()
  
}

expect_equal(calibration_value(c("1abc2",
                                 "pqr3stu8vwx",
                                 "a1b2c3d4e5f",
                                 "treb7uchet")), 142)

calibration_value(input)




calibration_value2 <- function(x){ 
  patterns <- c("\\d{1}", 
                "(one)", 
                "(two)", 
                "(three)", 
                "(four)", 
                "(five)", 
                "(six)", 
                "(seven)", 
                "(eight)", 
                "(nine)")

  out <- x %>% 
    imap_dfr(function(line, i){
      print(i)
      matches <- patterns %>% 
        map_dfr(function(y){
          str_locate_all(line, y) %>% 
            data.frame() %>% 
            mutate(val = str_extract_all(line, y)[[1]]) %>% 
            select(start, val)
        }) %>% 
        mutate(id = i)
    }) %>%
    group_by(id) %>% 
    arrange(id, start) %>% 
    filter(start %in% range(start)) %>%
    ungroup() %>%
    mutate(val = case_when(val == "one" ~ 1,
                           val == "two" ~ 2,
                           val == "three" ~ 3,
                           val == "four" ~ 4,
                           val == "five" ~ 5,
                           val == "six" ~ 6,
                           val == "seven" ~ 7,
                           val == "eight" ~ 8,
                           val == "nine" ~ 9,
                           TRUE ~ suppressWarnings(as.numeric(val)))) %>% 
    group_by(id) %>% 
    summarise(val = paste0(val, collapse = ""), .groups = "drop") %>% 
    mutate(val = as.numeric(val), 
           val = case_when(nchar(val) == 1 ~ as.numeric(paste0(val, val)), 
                           TRUE ~ val)) 
  
  sum(out$val)
}
  

calibration_value3 <- function(x){ browser()
  out <- tibble(x = x) %>% 
    mutate(first = str_match(x, "\\d|one|two|three|four|five|six|seven|eight|nine"),
           last = str_match(x, ".*(\\d|one|two|three|four|five|six|seven|eight|nine)")[,2], 
           across(c(first, last),
                  ~case_when(.x == "one" ~ 1,
                             .x == "two" ~ 2,
                             .x == "three" ~ 3,
                             .x == "four" ~ 4,
                             .x == "five" ~ 5,
                             .x == "six" ~ 6,
                             .x == "seven" ~ 7,
                             .x == "eight" ~ 8,
                             .x == "nine" ~ 9,
                             TRUE ~ suppressWarnings(as.numeric(.x))))) %>% 
    unite("val", first, last, sep = "") %>% 
    mutate(val = as.numeric(val))
  
  sum(out$val)
}


expect_equal(calibration_value2(c("two1nine",
                                  "eightwothree",
                                  "abcone2threexyz",
                                  "xtwone3four",
                                  "4nineeightseven2",
                                  "zoneight234",
                                  "7pqrstsixteen")), 281)

calibration_value2(input)
calibration_value3(input)

num <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
tibble(x = c("two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen")) %>% 
  extract(x, "first", "(\\d|one|two|three|four|five|six|seven|eight|nine)", remove = FALSE) %>% 
  extract(x, "last", ".*(\\d|one|two|three|four|five|six|seven|eight|nine)", remove = FALSE) %>% 
  mutate(first = coalesce(as.numeric(first), match(first, num)))
