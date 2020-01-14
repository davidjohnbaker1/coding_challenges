# Code Challenge | Jan 14 2020 | dailycodingchallenge.com 

# Given a list of numbers and number k, return if two numbers from the list add up to k

# For example given [10.15.3.7] if you say 17, get true

jan_14_2020 <- function(x, k){
  df <- tibble(x)
  df$key <- "key"
  df_2 <- tibble(x) %>% rename(y = x)
  df_2$key <- "key"
  joined <- full_join(df, df_2, by = "key")
  joined <- joined %>% mutate(total = x + y) %>% select(-key) %>% mutate(k = k)
  joined %>% 
    filter(total == k) -> solution
  if (nrow(solution) == 0){
    print("No Numbers Add Up!")
  } 
  else{
    print("Some Numbers add up!")
  }
}

jan_14_2020(foo, -5)