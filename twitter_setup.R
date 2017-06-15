
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages("httpuv")
#RESTART R session!

library(devtools)
#install_github("twitteR", username="geoffjentry")
library(twitteR)
library(httpuv)


api_key <- "VjCdstxpKRzZkD4arLUI8mc9o"
api_secret <- "OpUVRvue9ZPOISXvv2NJABuw0OTFzBPEazcwZsi4yxVW65uBL3"
access_token <- "	567537377-FiJpfjMk3ejMi005kqmEFjrj6Cm5Soavqz3MHjQP"
access_token_secret <- "nFnXnh3O5F4UPbFz7kRXY7M7ZFAU0NF8dgBGox0sT3x7c"

setup_twitter_oauth(api_key,api_secret)


### test - IT'S WORKING!!!!! ####
searchTwitter("#rstats")

#### rtweet ####

install.packages("rtweet")
library(rtweet)


twitter_token <- create_token(
  app = "R-play",
  consumer_key = api_key,
  consumer_secret = api_secret)

### getting #rstats users
?search_users

r_users <- search_users("#rstats", n = 1000)
page <- next_cursor(r_users)
r_users_ii <- search_users("#rstats", n = 1000, page = page)
r_users <- c(unlist(r_users), unlist(r_users_ii))


str(r_users)

colbert_nation <- get_followers("stephenathome", n = 18000)

## get even more by using the next_cursor function
page <- next_cursor(colbert_nation)

## use the page object to continue where you left off
colbert_nation_ii <- get_followers("stephenathome", n = 18000, page = page)
colbert_nation <- c(unlist(colbert_nation), unlist(colbert_nation_ii))



library(stringr)
library(dplyr)


### only 527 users
r_users %>% summarise(n_users = n_distinct(screen_name))

r_users_names <- r_users$screen_name %>% unique()
str(r_users_names)

r_users_info <- lookup_users(r_users_names)
str(r_users_info)






### popularity/activity score: 

r_users_info %>% select(contains("count")) %>% names()



r_users_score <- r_users_info %>%
  select(screen_name, followers_count, friends_count, statuses_count, listed_count, favourites_count) %>% 
  unique() %>% 
  mutate(followers_centile = ecdf(followers_count)(followers_count),
         friends_centile = ecdf(friends_count)(friends_count),
         listed_centile = ecdf(listed_count)(listed_count),
         favourites_centile = ecdf(favourites_count)(favourites_count),
         statuses_centile = ecdf(statuses_count)(statuses_count)
         ) %>% 
  arrange(screen_name) 

head(r_users_score)
str(r_users_score)


r_users_ranking = r_users_score %>% 
  group_by(screen_name) %>% 
  summarise(top_score = followers_centile + friends_centile + listed_centile + favourites_centile + statuses_centile)

top_10 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(10)
top_100 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(100)
bottom_10 <- r_users_ranking %>% arrange(desc(top_score)) %>% tail(10)

r_users_score %>% 
  filter(screen_name %in% top_10$screen_name)

r_users_score %>% 
  filter(screen_name %in% bottom_10$screen_name)

top_100_users <- top_100$screen_name
str(top_100_users)

### getting followers -> write a function!!
test_followers <- get_followers("drob", n = 18000)
str(test_followers)

test_page <- next_cursor(test_followers)
str(test_page)


??lower
r_users %>% dplyr::select(screen_name) %>% arrange(screen_name)
