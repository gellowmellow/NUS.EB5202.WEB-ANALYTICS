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


###########################  This is grouped by Age and Ratings ##########

  age_ratings <- ratings %>%
  inner_join(users, by = "userId") %>%
  inner_join(movies, by = "MovieID") %>%
  na.omit() %>%
  select(userId, Age, Occupation, MovieID, MovieTitle, Genres,rating)  %>%
  group_by(Age,MovieID) %>%
  summarise(count = n(),mean = mean(rating)) %>%
  #ungroup() %>%
  arrange(order(MovieID)) 
    
    summary(age_ratings)
    
    knitr::kable(head(age_ratings, 10))
    
    write.csv(age_ratings, file = "age_ratings.csv")
    
    
  
 ###########################  This is grouped by Oocupation and Ratings ##########
    
    occupation_ratings <- ratings %>%
      inner_join(users, by = "userId") %>%
      inner_join(movies, by = "MovieID") %>%
      na.omit() %>%
      select(userId, Age, Occupation, MovieID, MovieTitle, Genres,rating)  %>%
      group_by(Occupation,MovieID) %>%
      summarise(count = n(),mean = mean(rating)) %>%
      #ungroup() %>%
      arrange(order(MovieID)) 
    
    summary(occupation_ratings)
    
    knitr::kable(head(occupation_ratings, 10))
    
    write.csv(occupation_ratings, file = "occupation_ratings.csv")
  
