
edx = readRDS("../input/movielens/edx.rds")  #reading the data
test = readRDS("../input/movielens/validation.rds")  #reading the data
#importing the libraries
library(tidyverse)
library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(parallel)
library(doParallel)
fitControl <- trainControl(allowParallel = TRUE) 

head(edx)

#checking of there is any null
anyNA(edx)

str(edx) # takin a look at the data

n_distinct(edx$userId) # how many unique value do we have 

n_distinct(edx$movieId) #so how many movie do we have

#Let's see the distribution of it and compare it with the test set
hist(edx$rating, col = 'yellow2',main = 'Rating distibution in the train') # plot the target in the train data
hist(test$rating , col = 'yellow2',main = 'Rating distibution in the test') # plot the target in the test data

round(table(edx$rating)/nrow(edx) , 2) # what is the ratio of every ratnig group

boxplot(edx$rating ,main = 'rating spread', col = 'yellow2')

sum(edx$rating < 1) / nrow(edx) #how many movie with rate less that 1 ?

as.POSIXct(min(edx$timestamp),origin = "1970-01-01",tz = "UTC")
as.POSIXct(max(edx$timestamp),origin = "1970-01-01",tz = "UTC")

head(edx)$title

n_distinct(edx$genres) # how many unique genres do we have

user_gender_prefrences <- edx %>% filter(userId == 3) %>% group_by(userId,genres) %>% summarise(rating = mean(rating)) %>% print ## this grouped data will be used in ggplot to represet the relation between the two variables 

options(repr.plot.width = 20, repr.plot.height = 8)
ggplot(data = user_gender_prefrences , aes(x = genres , y = rating , fill = genres)) +
  geom_col() +
  ggtitle("ratings of the genres for a one user") +
  theme(plot.title = element_text(hjust = 0.5)) 

edx <- edx %>% mutate(releaseYear = as.numeric(str_sub(title,-5,-2))) # the year is from the second to the fifth place from the end

options(repr.plot.width = 20, repr.plot.height = 8)
edx %>% group_by(releaseYear) %>%
summarize(rating = mean(rating)) %>%
ggplot(aes(releaseYear, rating, fill = releaseYear)) +
 geom_point() + geom_smooth(method = "lm" ) +
  ggtitle("raing over years") +
   theme(plot.title = element_text(hjust = 0.5)) 


set.seed(1)
val_index <- createDataPartition(edx$rating, p = 0.9, list=FALSE ) # taking 10% for validation to be similar to the test set
temp <- edx[-val_index,]
edx <- edx[val_index,]

print(nrow(edx))

val <- temp %>%
semi_join(edx, by = "movieId") %>%
semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, val)
edx <- rbind(edx, removed)

print(nrow(edx))

str(val) # this data set will be for experimentation

# making the evaluation function
RMSE <- function(actual , prediction){
sqrt(mean((actual - prediction)^2))
}

mean_rating = mean(edx$rating)
print(c('The RMSE of global mean is : ', RMSE(val$rating, mean_rating)))

options(repr.plot.width = 20, repr.plot.height = 8)
movies_average_ratings <- edx %>%
group_by(movieId) %>%
summarize(movies_effect = mean(rating - mean_rating)) # how the movie average rating affecting the global mean ?
 ggplot(data = movies_average_ratings , aes(x = movies_effect)  ) + geom_histogram(color="darkblue", fill="lightblue") +
  ggtitle("movies effect on the global mean of rating") +
   theme(plot.title = element_text(hjust = 0.5))

# so for each movie add the movie effect to the global mean so that we capture the movie bais
movie_effect <- val %>% left_join(movies_average_ratings, by = 'movieId') %>% mutate(prediction = mean_rating + movies_effect)
print(c('RMSE after adding the movie effect is: ', RMSE(val$rating , movie_effect$prediction)))

options(repr.plot.width = 20, repr.plot.height = 8)

users_average_ratings <- edx %>%
left_join(movies_average_ratings, by='movieId') %>%
group_by(userId) %>%
summarize(users_effect = mean(rating - mean_rating - movies_effect)) # so we seeing here how the effect of both the user and movie on the global mean
ggplot(data = users_average_ratings , aes(x = users_effect )  ) + geom_histogram(color="darkblue", fill="lightblue")+
  ggtitle("users and movies effect on the global mean of rating") +
   theme(plot.title = element_text(hjust = 0.5))

movie_user_effect <- val %>% 
left_join(movies_average_ratings, by = 'movieId') %>%
left_join(users_average_ratings, by = 'userId') %>% # at this point we have the user and movie effect columns
mutate(prediction = mean_rating + movies_effect + users_effect) # add both columns to the global mean

print(c('RMSE after adding the user-movie effect is: ', RMSE(val$rating , movie_user_effect$prediction)))

users_reviews_count <- edx %>% group_by(userId) %>% summarise(count = n()) # how much movie reviews every user has made
summary(users_reviews_count$count) 

movies_reviews_count <- edx %>% group_by(movieId) %>% summarise(count = n()) # how much movie reviews every user has made
summary(movies_reviews_count$count) 

# hyperparamter tuning for lambda
x <- c() 
for (lambda in seq(0,6,0.25))  {
    movies_average_ratings_regu <- edx %>% group_by(movieId) %>% summarise(movies_effect = sum(rating - mean_rating)/(n()+lambda))
    users_average_ratings_regu <- edx %>% left_join(movies_average_ratings_regu, by='movieId') %>% group_by(userId) %>%
    summarize(users_effect = sum(rating - mean_rating - movies_effect)/(n()+lambda))

    movie_user_effect_regu <- val %>% 
    left_join(movies_average_ratings_regu, by = 'movieId') %>%
    left_join(users_average_ratings_regu, by = 'userId') %>% # at this point we have the user and movie effect columns
    mutate(prediction = mean_rating + movies_effect + users_effect) # add both columns to the global mean

    x <- c(x,RMSE(val$rating , movie_user_effect_regu$prediction))

        }

options(repr.plot.width = 20, repr.plot.height = 8) #plot lambda different values vs RMSE for each
qplot(seq(0,6,0.25),x)

edx_full = readRDS("../input/movielens/edx.rds") 
test = readRDS("../input/movielens/validation.rds") 

lambda = 5.25
movies_average_ratings_regu <- edx_full %>% group_by(movieId) %>% summarise(movies_effect = sum(rating - mean_rating)/(n()+lambda))
users_average_ratings_regu <- edx_full %>% left_join(movies_average_ratings_regu, by='movieId') %>% group_by(userId) %>%
summarize(users_effect = sum(rating - mean_rating - movies_effect)/(n()+lambda))

movie_user_effect_regu <- test %>% 
left_join(movies_average_ratings_regu, by = 'movieId') %>%
left_join(users_average_ratings_regu, by = 'userId') %>% # at this point we have the user and movie effect columns
mutate(prediction = mean_rating + movies_effect + users_effect) # add both columns to the global mean

print(c('RMSE after adding the user-movie effect is: ', RMSE(test$rating , movie_user_effect_regu$prediction)))

set.seed(1) 
print(nrow(edx_full))
val_index <- createDataPartition(edx_full$rating, p = 0.9, list=FALSE) # taking 10% for validation to be similar to the test set
temp <- edx_full[-val_index,] #take 0.1
edx <- edx_full[val_index,]# take 0.9

print(nrow(edx))

val <- temp %>%
semi_join(edx, by = "movieId") %>%
semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, val)
edx <- rbind(edx, removed)

print(nrow(edx))

as.numeric(factor(c('a','b','a'))) # how to convert from charachter to numerical

# converting title and genres with label encoding
edx['title_encoded'] = as.numeric(factor(edx$title)) 
val['title_encoded'] = as.numeric(factor(val$title)) 
test['title_encoded'] = as.numeric(factor(test$title)) 

edx['genres_encoded'] = as.numeric(factor(edx$genres)) 
val['genres_encoded'] = as.numeric(factor(val$genres)) 
test['genres_encoded'] = as.numeric(factor(test$genres)) 

head(edx)

#making movie release year
edx <- edx %>%
mutate(releaseYear = as.numeric(str_sub(title,-5,-2))) # the year is from the second to the fifth place from the end

val <- val %>%
mutate(releaseYear = as.numeric(str_sub(title,-5,-2))) # applying the same to the test

test <- test %>%
mutate(releaseYear = as.numeric(str_sub(title,-5,-2))) # applying the same to the test

edx = subset(edx, select = -c(title,genres) ) # we don't need them any more
val = subset(val, select = -c(title,genres) )
test = subset(test, select = -c(title,genres) )


set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit1 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit1, val)
print(c('The Linear model RMSE :', RMSE(prediction, val$rating)))

set.seed(1) 
fitControl <- trainControl(allowParallel = TRUE)
tree = train(rating ~ .,method="rpart",data= edx,trControl = fitControl ) 
prediction = predict(tree, test)
saveRDS(tree, "./tree.rds") # saving the model
print(c('The tree model RMSE :',' ', RMSE(prediction, test$rating)))

as.integer(substr("1970-01-01",0,4) , 16) # how to convert the year to number

#converting the time stamp to date and then extract the year and month from it
edx['date'] = as.POSIXct(edx$timestamp,origin = "1970-01-01",tz = "UTC")
val['date'] = as.POSIXct(val$timestamp,origin = "1970-01-01",tz = "UTC")
test['date'] = as.POSIXct(test$timestamp,origin = "1970-01-01",tz = "UTC")

#this is the review year
edx['year'] = as.integer(substr(edx$date,0,4),16)
val['year'] = as.integer(substr(val$date,0,4),16)
test['year'] = as.integer(substr(test$date,0,4),16)

#this is the review month
edx['month'] = as.integer(substr(edx$date,6,7),16)
val['month'] = as.integer(substr(val$date,6,7),16)
test['month'] = as.integer(substr(test$date,6,7),16)


anyNA(test)

lambda = 5.25
movies_average_ratings_regu <- edx %>% group_by(movieId) %>% summarise(movies_effect = sum(rating - mean_rating)/(n()+lambda))
users_average_ratings_regu <- edx %>% left_join(movies_average_ratings_regu, by='movieId') %>% group_by(userId) %>%
summarize(users_effect = sum(rating - mean_rating - movies_effect)/(n()+lambda))

edx <- edx %>% 
left_join(movies_average_ratings_regu, by = 'movieId') %>%
left_join(users_average_ratings_regu, by = 'userId') %>% # at this point we have the user and movie effect columns
mutate(prediction = mean_rating + movies_effect + users_effect) # add both columns to the global mean

val <- val %>% 
left_join(movies_average_ratings_regu, by = 'movieId') %>%
left_join(users_average_ratings_regu, by = 'userId') %>% # at this point we have the user and movie effect columns
mutate(prediction = mean_rating + movies_effect + users_effect) # add both columns to the global mean

test <- test %>% 
left_join(movies_average_ratings_regu, by = 'movieId') %>%
left_join(users_average_ratings_regu, by = 'userId') %>% # at this point we have the user and movie effect columns
mutate(prediction = mean_rating + movies_effect + users_effect) # add both columns to the global mean


set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit2 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit2, val)
print(c('The Linear model RMSE :', RMSE(prediction, val$rating)))

# mean encode the genres
edx_genres_mean_encoded = edx %>% group_by(genres_encoded) %>% summarise(genres_mean_encoded = mean(rating))

edx <- edx %>% left_join(edx_genres_mean_encoded , by = 'genres_encoded')
val <- val %>% left_join(edx_genres_mean_encoded , by = 'genres_encoded')
test <- test %>% left_join(edx_genres_mean_encoded , by = 'genres_encoded')

set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit3 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit3, val)
print(c('The Linear model RMSE :', RMSE(prediction, val$rating)))

edx$genres_mean_encoded <- NULL 
val$genres_mean_encoded <- NULL 
test$genres_mean_encoded <- NULL 


# mean encode the title
edx_title_mean_encoded = edx %>% group_by(title_encoded) %>% summarise(title_mean_encoded = mean(rating))

edx <- edx %>% left_join(edx_title_mean_encoded , by = 'title_encoded')
val <- val %>% left_join(edx_title_mean_encoded , by = 'title_encoded')
test <- test %>% left_join(edx_title_mean_encoded , by = 'title_encoded')

set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit4 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit4, val)
print(c('The Linear model RMSE :', RMSE(prediction, val$rating)))

edx$title_mean_encoded <- NULL 
val$title_mean_encoded <- NULL 
test$title_mean_encoded <- NULL 


val <- val %>% group_by(movieId) %>% mutate(first_movie_review_time =  min(date) , time_since_first_review = as.integer(date - first_movie_review_time))  

edx <- edx %>% group_by(movieId) %>% mutate(first_movie_review_time =  min(date) , time_since_first_review = as.integer(date - first_movie_review_time)) 

test <- test %>% group_by(movieId) %>% mutate(first_movie_review_time =  min(date) , time_since_first_review = as.integer(date - first_movie_review_time)) 


set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit5 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit5, val)
print(c('The Linear model RMSE :', RMSE(prediction, val$rating)))

# what is the average rating for  every movie
movies_average = edx %>% group_by(movieId) %>% summarise(movie_mean_rating = mean(rating))
edx = edx %>% left_join(movies_average , by = 'movieId')
val = val %>% left_join(movies_average , by = 'movieId')


set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit6 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit6, val)
print(c('The Linear model RMSE :', RMSE(prediction, val$rating)))

edx$movie_mean_rating = NULL
val$movie_mean_rating = NULL

prediction = predict(lm_fit5, test)
print(c('The Linear model RMSE :', RMSE(prediction, test$rating)))

colnames(test)
colnames(edx)

# removing date, timestamp and title_encoded as they are all redundent

edx$title_encoded <- NULL
edx$date <- NULL
edx$timestamp <- NULL

test$title_encoded <- NULL
test$date <- NULL
test$timestamp <- NULL

set.seed(1) # fixing the seed in order to ensure reproducibility
lm_fit7 <- lm(rating ~ . , data = edx )
prediction = predict(lm_fit7, test)
print(c('The Linear model RMSE :', RMSE(prediction, test$rating)))
