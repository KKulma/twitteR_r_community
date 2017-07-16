
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

#twitter_token <- create_token(
#  app = "R-play",
#  consumer_key = api_key,
#  consumer_secret = api_secret)

### getting #rstats users ####
?search_users

r_users <- search_users("#rstats", n = 1000)

#page <- next_cursor(r_users)
#r_users_ii <- search_users("#rstats", n = 1000, page = page)
#r_users <- c(unlist(r_users), unlist(r_users_ii))

?str
str(r_users, max.level = 1)
head(r_users)


library(stringr)
library(dplyr)


### only 553 users
r_users %>% summarise(n_users = n_distinct(screen_name))

r_users_names <- r_users$screen_name %>% unique()
str(r_users_names)

sort(r_users_names)


r_users_info <- lookup_users(r_users_names)
str(r_users_info)
str(r_users)


##### r_users' frequency of #rstats ####


r_users_tweets<-map(r_users$screen_name, userTimeline,100)



### popularity/activity score: 

r_users_info %>% select(dplyr::contains("count")) %>% head()

## you can look up definitions on https://dev.twitter.com/overview/api/users

r_users_score <- r_users_info %>%
  filter(protected == FALSE) %>% 
  select(screen_name, dplyr::contains("count")) %>% 
  unique() %>% 
  mutate(followers_percentile = ecdf(followers_count)(followers_count),
         friends_percentile = ecdf(friends_count)(friends_count),
         listed_percentile = ecdf(listed_count)(listed_count),
         favourites_percentile = ecdf(favourites_count)(favourites_count),
         statuses_percentile = ecdf(statuses_count)(statuses_count)
         ) %>% 
  arrange(screen_name) 

head(r_users_score)
tail(r_users_score)
str(r_users_score)

### it does not say how many of those tweets were actual #rstats tweets!

r_users_ranking = r_users_score %>% 
  group_by(screen_name) %>% 
  summarise(top_score = followers_percentile + friends_percentile + listed_percentile + favourites_percentile + statuses_percentile)

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


#### get followers 
friends1 <- get_friends("hspter")
friends_test <- map(top_10$screen_name, get_friends)

top10_friends_info1<-lookup_users(friends_test[1])



followers_test1 <- get_followers(top_10$screen_name[1])
followers_test2 <- get_followers(top_10$screen_name[2]) 
followers_test3 <- get_followers(top_10$screen_name[3]) 
followers_test4 <- get_followers(top_10$screen_name[4]) 
followers_test5 <- get_followers(top_10$screen_name[5]) 
followers_test6 <- get_followers(top_10$screen_name[6]) 
followers_test7 <- get_followers(top_10$screen_name[7]) 
followers_test8 <- get_followers(top_10$screen_name[8]) 
followers_test9 <- get_followers(top_10$screen_name[9]) 
followers_test10 <- get_followers(top_10$screen_name[10]) 


followers_test1B <- data.frame(r_user = rep(top_10$screen_name[1], length(followers_test1)), follower_id = followers_test1)
followers_test2B <- data.frame(r_user = rep(top_10$screen_name[2], length(followers_test2)), follower_id = followers_test2)
followers_test3B <- data.frame(r_user = rep(top_10$screen_name[3], length(followers_test3)), follower_id = followers_test3)
followers_test4B <- data.frame(r_user = rep(top_10$screen_name[4], length(followers_test4)), follower_id = followers_test4)
followers_test5B <- data.frame(r_user = rep(top_10$screen_name[5], length(followers_test5)), follower_id = followers_test5)
followers_test6B <- data.frame(r_user = rep(top_10$screen_name[6], length(followers_test6)), follower_id = followers_test6)
followers_test7B <- data.frame(r_user = rep(top_10$screen_name[7], length(followers_test7)), follower_id = followers_test7)
followers_test8B <- data.frame(r_user = rep(top_10$screen_name[8], length(followers_test8)), follower_id = followers_test8)
followers_test9B <- data.frame(r_user = rep(top_10$screen_name[9], length(followers_test9)), follower_id = followers_test9)
followers_test10B <- data.frame(r_user = rep(top_10$screen_name[10], length(followers_test10)), follower_id = followers_test10)

final_followers <- data.frame(rbind(followers_test1B, followers_test2B, followers_test3B, followers_test4B, followers_test5B,
                                    followers_test6B, followers_test7B, followers_test8B, followers_test9B, followers_test10B))
  
str(final_followers)

list_test <- list(followers_test1, followers_test2, followers_test3, followers_test4, followers_test5,
                  followers_test6, followers_test7, followers_test8, followers_test9, followers_test10)
str(final_followers)
summary(final_followers)

head(final_followers)
tail(final_followers)

d1 <- graph_from_data_frame(final_followers, directed = TRUE, vertices = NULL)


g1 <- create.igraph(data)
mdslayout <- layout.mds(g1, dist = distance_of_your_nodes)
plot.igraph(g1, layout = mdslayout)


graph<-graph_from_data_frame(dummy_data2[1:100, ])


ggraph(d1) + 
  geom_edge_link() + 
  geom_node_point()


geom_edge_link(aes(colour = factor(season)))+
  geom_node_point(aes(size=ifelse(V(graph)$name %in% n.names,1,degree)),
                  colour=ifelse(V(graph)$name %in% n.names,'#363636','#ffffff'),
                  show.legend = F)





str(list_test_names)

  set_names(map
            _chr(top_10, "screen_name"))

obama2 <- get_friends("BarackObama", page = next_cursor(obama1))




colbert_nation <- get_followers("stephenathome", n = 18000)

get_followers(top_10$screen_name[1], n = 18000)


follower_test <- map(top_10$screen_name, get_followers, n = 15000)


n_test1 <- get_followers(top_10$screen_name[1], n = 5000)
n_test2 <- get_followers(top_10$screen_name[2], n = 18000)
n_test3 <- get_followers(top_10$screen_name[3], n = 18000)
n_test4 <- get_followers(top_10$screen_name[4], n = 18000)
n_test5 <- get_followers(top_10$screen_name[5], n = 18000)
n_test6 <- get_followers(top_10$screen_name[6], n = 18000)
n_test7 <- get_followers(top_10$screen_name[7], n = 18000)
n_test8 <- get_followers(top_10$screen_name[8], n = 18000)
n_test9 <- get_followers(top_10$screen_name[9], n = 18000)
n_test10 <- get_followers(top_10$screen_name[10], n = 18000)











?getUser

str(top_10)
top_10$screen_name

users <- top10_ids$user_id #List of user ids
users_names <- top10_ids$screen_name #List of usernames
userrelations <- list() #Create an empty list to populate


#### dummy network ####

?expand.grid

users_names
str(users_names)
dummy_data <- expand.grid(from = users_names, to = users_names) %>% as.data.frame()
dummy_data$status <- sample(c("follower", "friend"), 100, replace =  TRUE) %>% as.data.frame()
dummy_data2 <- as.data.frame(user_el)
str(dummy_data)  


update.packages(checkBuilt = TRUE)
install.packages("ggplot2")

library(ggplot2)
library(ggraph)
library(igraph)
library(dplyr)

graph<-graph_from_data_frame(dummy_data2[1:100, ])

ggraph(graph) + 
  geom_edge_link() + 
  geom_node_point()
  
  
  geom_edge_link(aes(colour = factor(season)))+
  geom_node_point(aes(size=ifelse(V(graph)$name %in% n.names,1,degree)),
                  colour=ifelse(V(graph)$name %in% n.names,'#363636','#ffffff'),
                  show.legend = F)

devtools::install_github("briatte/ggnet")
library(ggnet)


installed.packages("network")
installed.packages("sna")
#remove.packages("scales")
install.packages("scales")
library(scales)
library(network)
library(sna)
library(ggplot2)

net = network(dummy_data, directed = TRUE)







library(purrr)
library(twitteR)


str(top_10)


?userTimeline
top10_tweets <- map(top_10$screen_name, userTimeline,100)


# EXTRACT each tweet text for screen name and count number of those containing #rstats hash
head(top10_tweets)

test2 <- list(top10_tweets[[2]])
str(test2)
test2B <-  as.data.frame(test2)

test2[[1]][[2]]$created
test2[[1]][[2]]$screenName
test2[[1]][[2]]$text



str(test2[[2]])
top10_tweets[[1]][[2]]

test_all <- as.data.frame(matrix(unlist(top10_tweets))) 
str(test_all)
head(test_all)


start <- getUser(users[1])
friends_object <- lookupUsers(start$getFriendIDs())
followers_object <- lookupUsers(start$getFollowerIDs())

friends <- sapply(1:length(friends_object), 
                  function(x) name(friends_object[[x]]))
followers <- sapply(1:length(followers_object), 
                    function(x) name(followers_object[[x]]))
userrelations[[i]] <- merge(data.frame(User=users[i],followers=friends),
                            data.frame(User=followers, followers=users[i]), 
                            all=TRUE)

load("/Users/katarzynakulma/projects/animated_maps/twitter_rstats.RData")



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
str(user_el)
head(user_el)
tail(user_el)

save.image(file="twitter_rstats.RData")


summary(user_el)

table(user_el$User)
table(user_el$followers)

user_el %>% n_distinct(users)
user_el %>% n_distinct(followers)



r_users_score %>% 
  filter(screen_name %in% bottom_10$screen_name)

top_100_users <- top_100$screen_name
str(top_100_users)


top_10_users <- top_10$screen_name
str(top_10_users)




##### STUFF I TRIED ####





#### twitteR hack to get followers of multiple users #####

### single example ###

# friends object
start <- getUser("@camharvey")
friends_object <- lookupUsers(start$getFriendIDs())
friendsCount(start)

# followers object 
followers_object <- lookupUsers(start$getFollowerIDs())
followers_object
followersCount(start)


friends <- sapply(friends_object[1:117],name)
followers <- sapply(followers_object[1:1033],name)

# Merge both lists into a data frame to create an edge file from followers and friends

relations <- merge(data.frame(User='@camharvey',followers=friends), data.frame(User=followers, followers='@camharvey'), all=TRUE)
a <- data.frame(User='@camharvey',followers=friends)
str(a)
head(a)

b <- data.frame(User=followers, followers='@camharvey')
str(b)
head(b)





### getting followers -> write a function!!


test_followers <- get_followers("drob", n = 18000)
str(test_followers)

top_10_users

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
