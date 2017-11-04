#sessionInfo()
#load all required package
#https://rpubs.com/vitidN/203264

library(arules)
library(dplyr)
library(reshape2)
library(Matrix)
library(stringr)
library(stringdist)
library(ggplot2)
#install.packages("stringdist")

setwd("~/EBAC/SEM 2/EB5202 Web Analytics/Movielens")

movies = read.csv("movies.DAT", 
                  #colClasses = c("integer","character","character"),
                  sep = "|",
                  stringsAsFactors = FALSE, header = FALSE)

head(movies)

movies$year = as.numeric(str_sub( str_trim(movies$V2) ,start = -5,end = -2))
movies$year
head(movies)
colnames(movies) <- c("MovieID", "MovieTitle", "Genres","Year")
head(movies)

# Discard Movie ID which does not ahving a Year
discard_movie_id = which(is.na(movies$Year))
discard_movie_id
#display discarded movies
movies$MovieTitle[discard_movie_id]
head(movies)
#movies = movies[-discard_movie_id,]
head(movies)


# Removing the year from  Movie Title
movies$MovieTitle = str_sub( str_trim(movies$MovieTitle) ,start = 1,end = -8)
movies$MovieTitle
head(movies$MovieTitle)

# Preaparing Unique Genres

all_genres = unique(unlist(str_split(movies$Genres,"\\,")))

all_genres

head(movies)

# Filtering the Movie ID which doesn not have Genres
movies %>% filter(str_detect(all_genres,"(no genres listed)") ) %>% nrow()

for(genre in all_genres){
  movies[str_c("genre_",genre)] = ifelse(( str_detect(movies$Genres,genre) | str_detect(movies$Genres,"no genres") ) , 1 , 0)
}
head(movies)

# Data loaded with Movei ID , Movie Title along with Genres
write.csv(movies, file = "MovieLensData.csv")


#check the result
head(movies)
tail(movies)
#Distinct years :: unique(movies$Year)
ggplot(movies,aes(x=Year))  + geom_bar() + ggtitle("Number of Movies")+ xlim(1920,2000) 

##############################
#Ratins
##############################
# Clean ratings
genre_dist = colSums(movies[,5:21])
genre_dist_df = data.frame(genre = names(genre_dist),count = genre_dist)
genre_dist_df$genre = factor(genre_dist_df$genre,levels = names(sort(genre_dist,decreasing = FALSE)))

ggplot(genre_dist_df,aes(x=genre,y=count,fill=genre)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  ggtitle("Genre Distribution") + 
  theme(legend.position = "none")


#Construct Association Rules from Rating Data
ratings = read.csv("ratings.dat",
                   colClasses = c("integer","integer","integer","NULL"),
                   sep=",",
                   stringsAsFactors = FALSE)

head(ratings)
ratings = ratings %>% filter(! MovieID %in% discard_movie_id )
dim(ratings)[1]
#convert rating-per-row dataframe into sparse User-Item matrix
user_item_matrix <- as(split(ratings[,"MovieID"], ratings[,"userId"]), "transactions")
head(user_item_matrix)

write.csv(user_item_matrix, file = "user_item_matrix.csv") 

#Construct User data 
users = read.csv("users.dat",
                 colClasses = c("integer","character","integer","integer","character"),
                 sep="|",
                 stringsAsFactors = FALSE)

head(users)

#investigate the User-Item matrix
#transactions (rows) -> number of raters
#items (columns) -> number of movies
user_item_matrix

# Setting the Rule Paramter for Support Confidence 
rule_param = list(
  supp = 0.001,
  conf = 0.7,
  maxlen = 2
)

# Assocaition Rules using Apriori
assoc_rules = apriori(user_item_matrix,parameter = rule_param)
summary(assoc_rules)

?apriori

head(user_item_matrix)
assoc_rules = subset(assoc_rules, lift >= 4.323)

summary(assoc_rules)

# We cast assoc_rules to data.frame and look at some of the data
assoc_rules = as(assoc_rules,"data.frame")

head(assoc_rules)
write.csv(assoc_rules, file = "assoc_rules.csv") 

# The rules still contain movieId. We split movies in both sides to a new column

assoc_rules$common_genre = apply(assoc_rules,1,function(x){
  sum(as.numeric(x[6:23]) & as.numeric(x[26:43]))
})

assoc_rules$common_genre




#############################
#What were the best movies based on users' ratings)?
install.packages("registerDoMC")
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(XML)
library(tidytext)
library(wordcloud)
library(doMC)
registerDoMC()
set.seed(1234)


summary(ratings)

# average rating for a movie
movie_rating <- ratings %>%
  inner_join(movies, by = "MovieID") %>%
  na.omit() %>%
  select(MovieID, MovieTitle, rating, Year) %>%
  group_by(MovieID, MovieTitle, Year) %>%
  summarise(count = n(), mean = mean(rating)) %>%
  ungroup() %>%
  arrange(desc(mean))


knitr::kable(head(movie_rating, 10))
write.csv(avg_rating, file = "movie_rating.csv") 



# Effect of occupation On Ratings
occupation_ratings <- ratings %>%
  inner_join(users, by = "userId") %>%
  na.omit() %>%
  select(userId, Gender, Occupation, MovieID, rating) %>%
  group_by(Occupation) %>%
  summarise(count = n(), mean = mean(rating)) %>%
  ungroup() %>%
  arrange(order(count))

knitr::kable(head(occupation_ratings, 10))

write.csv(occupation_ratings, file = "occupation_ratings.csv")

# Plot Effect of occupation On Ratings
ggplot(occupation_ratings,aes(x=Occupation,y=count,fill=Occupation)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  #axis(side=2, at=seq(0,600,by=100)) +
  ggtitle("Occupation Demographic Distribution") + 
  theme(legend.position = "right")

# Effect of Age on ratings

age_ratings <- users %>%
  inner_join(ratings, by = "userId") %>%
  na.omit() %>%
  select(userId, Age, Occupation, MovieID, rating)  %>%
  group_by(Age) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(order(Age))

knitr::kable(head(age_ratings, 10))

# Plot age and rating counts
ggplot(age_ratings,aes(x=Age,y=count,fill=Age)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  # ylim(20000,200000) +
  ggtitle("Age Distribution") + 
  theme(legend.position = "none")



  age_ratings <- ratings %>%
  inner_join(users, by = "userId") %>%
  inner_join(movies, by = "MovieID") %>%
  na.omit() %>%
  select(userId, Age, Occupation, MovieID, MovieTitle, Genres,rating)  %>%
  group_by(Age,MovieID, movie_rating) %>%
  summarise(count = n()) %>%
  #ungroup() %>%
  arrange(order(Age,MovieTitle)) 
    
    summary(age_ratings)
    
    knitr::kable(head(age_ratings, 10))
    
    user_rating_join <- merge(ratings, users, by="userId")
    user_rating_movies_join <- merge(user_rating_join, movies, by="MovieID") 
  
head(user_rating_movies_join)
write.csv(user_rating_movies_join, file = "user_rating_movies_join.csv")

age_ratings <- user_rating_movies_join %>%
   select( Age, MovieID, rating)  %>%
  group_by(Age,MovieID, rating) %>%
  summarise(count = n()) %>%
  #ungroup() %>%
  arrange(desc(count))

knitr::kable(head(age_ratings, 10))

age_ratings <- user_rating_movies_join %>%
  select( Age, MovieTitle, Genres, rating, )  %>%
  group_by(Age,MovieTitle, rating) %>%
  summarise(count = n()) %>%
  #ungroup() %>%
  arrange(desc(count))

knitr::kable(head(age_ratings, 10))

user_item_matrix1 <- as(split(user_rating_movies_join[,"MovieTitle"], user_rating_movies_join[,"Age"]),  "transactions")
head(user_item_matrix1)

rule_param1= list(
  supp = 0.1,
  conf = 0.1,
  maxlen = 7
)

# Assocaition Rules using Apriori
assoc_rules1 = apriori(user_item_matrix1,parameter = rule_param1)
summary(assoc_rules1)
rule_param
summary(user_item_matrix1)

write.csv(assoc_rules, file = "assoc_rules1.csv")
