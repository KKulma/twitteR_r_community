
#install.packages(c("devtools", "rjson", "bit64", "httr"))
#install.packages("httpuv")
#RESTART R session!
#install.packages("dplyr")
install.packages("DT")
install.packages("brocks")
devtools::install_github("brendan-R/brocks")

rm(list = ls())

library(devtools)
#install_github("twitteR", username="geoffjentry")
library(twitteR)
library(httpuv)
library(rtweet)
library(stringr)
library(dplyr)


update_packages("httr")
library(httr)
library(ggraph)
library(igraph)
library(purrr)

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

setwd("/Users/katarzynakulma/projects/twitter_rstats_community")
source("config.R")


setup_twitter_oauth(api_key,api_secret)


### test - IT'S WORKING!!!!! ####
searchTwitter("#rstats")

#### rtweet ####

#install.packages("rtweet")


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


str(r_users, max.level = 1)
head(r_users)



### only 553 users
r_users %>% summarise(n_users = n_distinct(screen_name))

r_users_names <- r_users$screen_name %>% unique()
str(r_users_names)

sort(r_users_names)


r_users_info <- lookup_users(r_users_names)
str(r_users_info)
str(r_users)


##### r_users' frequency of #rstats ####


#r_users_tweets<-map(r_users$screen_name, userTimeline,100)



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

##### ranking vis #####

r_users_score %>% 
  ggplot(aes(followers_count)) +
  geom_histogram() +
  stat_bin(bins = 1000)

r_users_score %>% 
  ggplot(aes(friends_count)) +
  geom_histogram() +
  stat_bin(bins = 1000)

r_users_score %>% 
  ggplot(aes(listed_count)) +
  geom_histogram() +
  stat_bin(bins = 1000)

r_users_score %>% 
  ggplot(aes(favourites_count)) +
  geom_histogram() +
  stat_bin(bins = 1000)


r_users_score %>% 
  ggplot(aes(statuses_count)) +
  geom_histogram() +
  stat_bin(bins = 1000)


### it does not say how many of those tweets were actual #rstats tweets!

r_users_ranking = r_users_score %>% 
  group_by(screen_name) %>% 
  summarise(top_score = followers_percentile + friends_percentile + listed_percentile + favourites_percentile + statuses_percentile)

top_10 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(10)
top_20 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(20)
top_30 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(30)
top_100 <- r_users_ranking %>% arrange(desc(top_score)) %>% head(100)
bottom_10 <- r_users_ranking %>% arrange(desc(top_score)) %>% tail(10)

top_100 %>% as.data.frame()

final_summary <- r_users_score %>%
  inner_join(r_users_ranking) %>% 
  mutate(ranking = rank(-top_score))

#?rank

final_summary %>% 
  filter(screen_name %in% top_10$screen_name) %>%
  arrange(desc(friends_count))


top10_lookup <- r_users_info %>%
  filter(screen_name %in% top_10$screen_name) %>% 
  select(screen_name, user_id)

top20_lookup <- r_users_info %>%
  filter(screen_name %in% top_20$screen_name) %>% 
  select(screen_name, user_id)

top30_lookup <- r_users_info %>%
  filter(screen_name %in% top_30$screen_name) %>% 
  select(screen_name, user_id)

top30_lookup$gender <- c("M", "F", "F", "F", "F",
                         "M", "M", "M", "F", "F", 
                         "F", "M", "M", "M", "F", 
                         "F", "M", "M", "M", "M", 
                         "M", "M", "M", "U", "M",
                         "M", "M", "M", "M", "M")

table(top30_lookup$gender)

top100_lookup <- r_users_info %>%
  filter(screen_name %in% top_100$screen_name) %>% 
  select(screen_name, user_id)

#### get friends ####

?get_friends
top_10_userids <- top10_lookup$user_id
top_10_usernames <- as_vector(top10_lookup$screen_name)

top_20_userids <- top20_lookup$user_id
top_20_usernames <- as_vector(top20_lookup$screen_name)

top_30_userids <- top30_lookup$user_id
top_30_usernames <- as_vector(top30_lookup$screen_name)

top_100_userids <- top100_lookup$user_id
top_100_usernames <- as_vector(top100_lookup$screen_name)

str(top_10_userids)
str(top_10_usernames)

str(top_20_userids)
str(top_20_usernames)

library(purrr)
friends_test <- map(top_10_usernames, get_friends)
SJ_friends <- get_followers("TheSmartJokes")

friends_test20 <- map(top_20_usernames, get_friends)
str(friends_test20)


SJ_friends <- get_followers("TheSmartJokes")


friends_test30a <- map(top_30_usernames[1:15 ], get_friends)
friends_test30b <- map(top_30_usernames[16:30], get_friends)

str(friends_test30a)
str(friends_test30b)

#friends_test100 <- map(top_100_usernames, get_friends)

str(friends_test20)
str(SJ_friends)
head(SJ_friends)

SJ_friends <-SJ_friends %>% 
  rename(friend_id = user_id) %>% 
  mutate(twitter_top_user = "TheSmartJokes") %>% 
  select(twitter_top_user, friend_id)


friends_test2 = friends_test
names(friends_test2) <- top_10_usernames
names(friends_test2[1])

friends_test22 = friends_test20
names(friends_test22) <- top_20_usernames
names(friends_test2[1])
str(friends_test22)

friends_test32a = friends_test30a
friends_test32b = friends_test30b

names(friends_test32a) <- top_30_usernames[1:15]
names(friends_test32b) <- top_30_usernames[16:30]

str(friends_test32a)
str(friends_test32b)


friends_test32 <- append(friends_test32a, friends_test32b)
str(friends_test32)
names(friends_test32) <- top_30_usernames

friends_test3333 <- map2_df(friends_test32, names(friends_test32), ~ mutate(.x, twitter_top_user = .y)) %>% 
  rename(friend_id = user_id) %>% select(twitter_top_user, friend_id)

str(friends_test3333)
str(friends_test33)

identical(friends_test33, friends_test3333)

head(friends_test3333)
head(friends_test33)

tail(friends_test3333)
tail(friends_test33)

#friends_test32 <-rbind(friends_test32a, friends_test32b)
#str(friends_test32)



friends_test102 = friends_test100
names(friends_test102) <- top_100_usernames
names(friends_test102[1])
str(friends_test102)




friends_test3 <- map2_df(friends_test2, names(friends_test2), ~ mutate(.x, twitter_top_user = .y)) %>% 
  rename(friend_id = user_id) %>% select(twitter_top_user, friend_id)

friends_test23 <- map2_df(friends_test22, names(friends_test22), ~ mutate(.x, twitter_top_user = .y)) %>% 
  rename(friend_id = user_id) %>% select(twitter_top_user, friend_id)

friends_test33a <- map2_df(friends_test32a, names(friends_test32a), ~ mutate(.x, twitter_top_user = .y)) %>% 
  rename(friend_id = user_id) %>% select(twitter_top_user, friend_id)

friends_test33b <- map2_df(friends_test32b, names(friends_test32b), ~ mutate(.x, twitter_top_user = .y)) %>% 
  rename(friend_id = user_id) %>% select(twitter_top_user, friend_id)

str(friends_test33a)
str(friends_test33b)

friends_test33 <- rbind(friends_test33a, friends_test33b)
str(friends_test33)



str(friends_test23)
head(friends_test23)
tail(friends_test23)

str(friends_test33)
head(friends_test33)
tail(friends_test33)

friends_test23 %>% summarize(dist = n_distinct(twitter_top_user))
friends_test33 %>% summarize(dist = n_distinct(twitter_top_user))


####  checking TheSmartJokes ####

load("/Users/katarzynakulma/projects/kkulma.github.io/blog_prep/twitter_image.RData")


r_users_info %>% filter(screen_name ==  "TheSmartJokes") %>% select(screen_name, dplyr::contains("count")) 
str(SJ_friends)


friends_test24 <- friends_test23 %>% 
  filter(twitter_top_user != "TheSmartJokes") %>% 
  rbind(SJ_friends) 

friends_test34 <- friends_test33 %>% 
  filter(twitter_top_user != "TheSmartJokes") %>% 
  rbind(SJ_friends) 


friends_test25 <- friends_test24 %>% 
  filter(friend_id %in% top20_lookup$user_id)

friends_test35 <- friends_test34 %>% 
  filter(friend_id %in% top30_lookup$user_id)


friends_test25$friend_name <- top20_lookup$screen_name[match(friends_test25$friend_id, top20_lookup$user_id)]
friends_test35$friend_name <- top30_lookup$screen_name[match(friends_test35$friend_id, top30_lookup$user_id)]


friends_test35$user_gender <- top30_lookup$gender[match(friends_test35$twitter_top_user, top30_lookup$screen_name)]
friends_test35$friend_gender <- top30_lookup$gender[match(friends_test35$friend_name, top30_lookup$screen_name)]

str(friends_test25)

final_test2 <- friends_test25 %>% select(-friend_id)
final_test3 <- friends_test35 %>% select(-friend_id)
str(final_test3)
str(final_summary)

final_test$from_ranking = final_summary$ranking[match(final_test$twitter_top_user, final_summary$screen_name)]
final_test$from_top_score = final_summary$top_score[match(final_test$twitter_top_user, final_summary$screen_name)]

final_test$to_ranking = final_summary$ranking[match(final_test$follower_name, final_summary$screen_name)]
final_test$to_top_score = final_summary$top_score[match(final_test$follower_name, final_summary$screen_name)]

final_test$ranking <- NULL
final_test$top_score <- NULL

getwd()
setwd("/Users/katarzynakulma/projects/kkulma.github.io/blog_prep")
save.image(file = "twitter_image.RData")

#### plot friendships ####
??graph_from_data_frame


final_test
f1 <- graph_from_data_frame(final_test3, directed = TRUE, vertices = NULL)
#V(f1)$degree<-degree(f1)
V(f1)$Popularity <- degree(f1, mode = 'in')
V(f1)$name

### good background colours
# dodgerblue4
# darkslateblue
# darkslategray
# dimgrey

ggraph(f1, layout='kk') + 
  #geom_edge_link(aes(colour = factor(season)))+
  #geom_edge_link() + 
  geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(size = Popularity)) +
#  geom_node_point() +
  geom_node_text(aes(label = name, fontface='bold'), 
                 color = 'white', size = 4) +
  theme_graph(background = 'dimgray', text_colour = 'white',title_size = 30) 
#  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

### user gender 
ggraph(f1, layout='kk') + 
  #geom_edge_link(aes(colour = factor(season)))+
  #geom_edge_link() + 
  geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(size = Popularity)) +
  #  geom_node_point() +
  #geom_node_text(aes(label = name, fontface='bold'), 
  #               color = 'white', size = 4) +
  #theme_graph(background = 'gray', text_colour = 'white',title_size = 30) 
  theme_graph( fg_text_colour = 'black') +
  facet_edges(~user_gender)


### friend gender
ggraph(f1, layout='kk') + 
  #geom_edge_link(aes(colour = factor(season)))+
  #geom_edge_link() + 
  geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(size = Popularity)) +
  #  geom_node_point() +
  #geom_node_text(aes(label = name, fontface='bold'), 
  #               color = 'white', size = 4) +
  #theme_graph(background = 'gray', text_colour = 'white',title_size = 30) 
  theme_graph( fg_text_colour = 'black') +
  facet_edges(~friend_gender)



#### pure graph
### friend gender
ggraph(f1, layout='kk') + 
  #geom_edge_link(aes(colour = factor(season)))+
  #geom_edge_link() + 
  geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(size = Popularity)) +
  #  geom_node_point() +
  #geom_node_text(aes(label = name, fontface='bold'), 
  #               color = 'white', size = 4) +
  #theme_graph(background = 'gray', text_colour = 'white',title_size = 30) 
  theme_graph( fg_text_colour = 'black') 




  labs(title='Batman Villains',subtitle='Plotting 37 Batman villains across 3 seasons with\nnode ends representing season & episode number',
       caption='ggraph walkthroughs available at: http://www.data-imaginist.com/\n Data from: http://mentalfloss.com/article/60213/visual-guide-all-37-villains-batman-tv-series')
#dev.off() 
  
  
  ggraph(f1, layout = 'kk') + 
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) + 
    geom_node_point(aes(size = Popularity)) + 
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
    geom_node_text(aes(label = name, fontface='bold'), color = 'dodgerblue4', size = 4)
  
  









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


followers_test1B <- data.frame(r_user = rep(top_10$screen_name[1], length(followers_test1)), friend_id = followers_test1)
followers_test2B <- data.frame(r_user = rep(top_10$screen_name[2], length(followers_test2)), friend_id = followers_test2)
followers_test3B <- data.frame(r_user = rep(top_10$screen_name[3], length(followers_test3)), friend_id = followers_test3)
followers_test4B <- data.frame(r_user = rep(top_10$screen_name[4], length(followers_test4)), friend_id = followers_test4)
followers_test5B <- data.frame(r_user = rep(top_10$screen_name[5], length(followers_test5)), friend_id = followers_test5)
followers_test6B <- data.frame(r_user = rep(top_10$screen_name[6], length(followers_test6)), friend_id = followers_test6)
followers_test7B <- data.frame(r_user = rep(top_10$screen_name[7], length(followers_test7)), friend_id = followers_test7)
followers_test8B <- data.frame(r_user = rep(top_10$screen_name[8], length(followers_test8)), friend_id = followers_test8)
followers_test9B <- data.frame(r_user = rep(top_10$screen_name[9], length(followers_test9)), friend_id = followers_test9)
followers_test10B <- data.frame(r_user = rep(top_10$screen_name[10], length(followers_test10)), friend_id = followers_test10)

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
d0 <- graph_from_data_frame(final_followers[1:1000], directed = TRUE, vertices = NULL)

getwd()

pdf("SampleGraph.pdf",width=7,height=5)
ggraph(d0,layout='fr') + 
  #geom_edge_link(aes(colour = factor(season)))+
   geom_edge_link() + 
  geom_node_point()
dev.off() 

library(igraph)
??create.igraph

pdf("SampleGraph_vick.pdf",width=7,height=5)
mdslayout <- layout.mds(d0)
plot.igraph(d0, layout = mdslayout)
dev.off() 

graph<-graph_from_data_frame(dummy_data2[1:100, ])





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


load("/Users/katarzynakulma/twitter_rstats.RData")
getwd()
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
