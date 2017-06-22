
#install.packages(c("devtools", "rjson", "bit64", "httr"))
#install.packages("httpuv")
#RESTART R session!


rm(list = ls())

library(devtools)
#install_github("twitteR", username="geoffjentry")
library(twitteR)
library(httpuv)

update_packages("httr")
library(httr)

#### twitter issues - demo ####

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = api_key,
                   secret = api_secret
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))
stop_for_status(req)
content(req)



api_key <- "9aMsBdeRf16FR0F14JHX8ySfE"
api_secret <- "IED6n8nnBnxfa5rZoKn6ZCXdy2w2ARNDfY8a3Ry0GXhAwOJByd"
access_token <- "567537377-hMRh7424BRezcuu03bgsVAqYlqsHbsI8QtRVkGQF"
access_token_secret <- "na3LB2iWhZuxPGDtaukkRFoatYiJPJIk0PRTdgaxiBBAB"

setup_twitter_oauth(api_key,api_secret)


### test - IT'S WORKING!!!!! ####
searchTwitter("#rstats")

#### rtweet ####

#install.packages("rtweet")
library(rtweet)

### log in ####

twitter_token <- create_token(
  app = "R-play",
  consumer_key = api_key,
  consumer_secret = api_secret)

### getting #rstats users ####
?search_users

r_users <- search_users("#rstats", n = 1000)

#page <- next_cursor(r_users)
#r_users_ii <- search_users("#rstats", n = 1000, page = page)
#r_users <- c(unlist(r_users), unlist(r_users_ii))


str(r_users)



library(stringr)
library(dplyr)


### only 527 users
r_users %>% summarise(n_users = n_distinct(screen_name))

r_users_names <- r_users$screen_name %>% unique()
str(r_users_names)

sort(r_users_names)


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
tail(r_users_score)
str(r_users_score)


r_users_ranking = r_users_score %>% 
  group_by(screen_name) %>% 
  summarise(top_score = followers_centile + friends_centile + listed_centile + favourites_centile + statuses_centile)

top_10 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(10)
top_100 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(100)
bottom_10 <- r_users_ranking %>% arrange(desc(top_score)) %>% tail(10)

top_100 %>% as.data.frame()


r_users_score %>% 
  filter(screen_name %in% top_10$screen_name)


top10_ids <- r_users_info %>%
  filter(screen_name %in% top_10$screen_name) %>% 
  select(screen_name, user_id)

id <- top10_ids$user_id


#### twitteR hack to get followers of multiple users #####

library(twitteR)

?getUser

users <- top10_ids$user_id #List of usernames
userrelations <- list() #Create an empty list to populate

for (i in 1:length(users)){
  start <- getUser(users[i])
  friends_object <- lookupUsers(start$getFriendIDs())
  followers_object <- lookupUsers(start$getFollowerIDs())
  friends <- sapply(1:length(friends_object), 
                    function(x) name(friends_object[[x]]))
  followers <- sapply(1:length(followers_object), 
                      function(x) name(followers_object[[x]]))
  userrelations[[i]] <- merge(data.frame(User=users[i],followers=friends),
                              data.frame(User=followers, followers=users[i]), 
                              all=TRUE)
}



user_el <- do.call("rbind",userrelations)

save("twitter_rstats.RData")


r_users_score %>% 
  filter(screen_name %in% bottom_10$screen_name)

top_100_users <- top_100$screen_name
str(top_100_users)


top_10_users <- top_10$screen_name
str(top_10_users)

### getting followers -> write a function!!


test_followers <- get_followers("drob", n = 18000)
str(test_followers)


library(purrr)
token<-create_token(app="R-play",api_key,api_secret) 
test_append <- map(top_10_users, get_followers)
rate_limit(token)


Users <- usrs$user_id
goodUsers <- substring(list.files(),1,nchar(list.files())-11)
Users <- setdiff(Users,goodUsers)

while(length(Users)>200){
  for(i in 80:length(Users)){
    
    a<-tryCatch({get_timeline(Users[i],usr=FALSE)},
                error=function(e){message(e)}
    )
    tryCatch({save_as_csv(a,Users[i])
      goodUsers <- append(goodUsers,Users[i])},
      error=function(e){message(e)}
    )
    
    
  }
  Users <- setdiff(Users,goodUsers)
  Sys.sleep(900)
}

length(Users)
length(goodUsers)




?rate_limit
?search_tweets
vignette("rtweet")



test_page <- next_cursor(test_followers)
str(test_page)


??lower
r_users %>% dplyr::select(screen_name) %>% arrange(screen_name)
