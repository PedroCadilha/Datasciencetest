##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Data preliminary exploration
summary(edx)
dim(edx)
head(edx)

#Number of rating by movie
edx%>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10()+
  ggtitle("Rating Number Per Movie")

#Number of rating by user
edx%>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10()+
  ggtitle("Number of rating Per User")

#Even top movies have missing votes
keep <- edx %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- edx %>%
  filter(userId %in% c(1:10)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

#Sample of matrix userId vs. movieId (which user rate which movie)
if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)
users <- sample(unique(edx$userId), 100)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

#Number of unique users and movies in the dataset
edx%>%summarize(n_users=n_distinct(userId),n_movies=n_distinct(movieId))

#If each user voted all the movies
n_distinct(edx$userId)*n_distinct(edx$movieId)

#Fraction of votes done by the users
dim(edx)[1]/(n_distinct(edx$userId)*n_distinct(edx$movieId))

#Number of distinct genre combinations
n_distinct(edx$genres)
edx%>%group_by(genres)%>%summarize(count=n())%>%arrange(desc(count))
edx%>%group_by(genres)%>%summarize(count=n())%>%arrange(count)

#Plot average rating vs. genre (categories with more than 100000 ratings)
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Movies with more ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Ratings distribution
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


#separate year from movie name
edx<-edx%>%extract(title, c("title", "year_released"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)%>%
  mutate(year_released=as.numeric(year_released))
validation<-validation%>%extract(title, c("title", "year_released"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)%>%
  mutate(year_released=as.numeric(year_released))


edx %>% 
  filter(year_released >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year_released),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#Plot of average rating versus ratings per year with an estimate of the trend.
edx%>% 
  filter(year_released >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year_released),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#Time effect, rating vs. year_released
edx %>% group_by(year_released) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_released, rating)) +
  geom_point() +
  geom_smooth()


#Create a test set
set.seed(755, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Function to calculate our model perfomance
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Just the average
mu_hat <- mean(train_set$rating)
mu_hat
naive_rmse<- RMSE(test_set$rating, mu_hat)
naive_rmse

#Tibble to add our model results throught the analysis 
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)


#Movie Effect Model
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#Y hat calculations, our rating predictions
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

#Calculation of the RMSE
RMSE(predicted_ratings, test_set$rating)
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

#User Effect Model 
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#Error analysis
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)

#Movie effect regularized
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%                                  #movie effect regularization
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i ) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
model_3_rmse<-min(rmses)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effects Regularized",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#Movie + user effect regularized
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)                         #movie effect regularized
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>%                                 #user effect regularized
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)
model_4_rmse<-min(rmses)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Regularized",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

#Movie + user + genre effect regularized
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%                            #movie effect regularized
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>%                            #user effect regularized
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g<-train_set %>%                              #genre effect regularized
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId")%>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i- b_u - mu)/(n()+l))
  
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)
model_5_rmse<-min(rmses)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genres Effects Regularized",  
                                 RMSE = model_5_rmse ))
rmse_results %>% knitr::kable()


#Movie+user+genre+year released effects regularized
lambdas <- seq(0.5, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%                            #movie effect regularized
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>%                            #user effect regularized
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g<-train_set %>%                              #genre effect regularized
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId")%>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i- b_u - mu)/(n()+l))
  
  b_y<-train_set %>%                              #year effect regularized
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId")%>%
    left_join(b_g, by="genres")%>%
    group_by(year_released) %>%
    summarize(b_y = sum(rating - b_i- b_u - b_g - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year_released") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)
model_6_rmse<-min(rmses)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genres + Year Released Effects Regularized",  
                                 RMSE = model_6_rmse ))
rmse_results %>% knitr::kable()


#Movie + user + genre + year effect regularized optimal lambda
#Aplication on Validation set

l <- lambda
mu <- mean(edx$rating)

b_i <- edx %>%                                            #movie effect regularized
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>%                                            #user effect regularized
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_g<- edx %>%                                             #genre effect regularized
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId")%>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i- b_u - mu)/(n()+l))

b_y<-edx %>%                                              #year effect regularized
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId")%>%
  left_join(b_g, by="genres")%>%
  group_by(year_released) %>%
  summarize(b_y = sum(rating - b_i- b_u - b_g - mu)/(n()+l))


predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year_released") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

RMSE_Validation_Set<-RMSE(predicted_ratings, validation$rating)
RMSE_Validation_Set

rmse_results <- bind_rows(rmse_results,
                          tibble(method="RMSE_Validation_Set",  
                                 RMSE = RMSE_Validation_Set))
rmse_results %>% knitr::kable()



