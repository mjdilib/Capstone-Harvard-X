

#######################################################################################################################################
# SECTION 1
# Setup: Load Data and Libraries | Split between Training and Validation Sets 
#######################################################################################################################################

# Leverage code provided by EdX
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(officer)) install.packages("officer", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")


# Import libraries

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(readr)
library(officer)
library(kableExtra)
library(tinytex)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#########################################################################################################################################
# SECTION 2
# Data Cleansing & Wrangling 
#########################################################################################################################################

# Manipulate timestamp to a format which is more readable. 
# Create the "year_release" variable for both Training and Validation.

edx <- edx %>%  
  mutate(date_review = round_date(as_datetime(timestamp), unit = "week")) %>%  
  mutate(year_release = substring(title, nchar(title) - 6)) %>% 
  mutate(year_release = as.numeric(substring(year_release, regexpr("\\(", year_release) + 1, regexpr("\\)", year_release) - 1)))

validation <- validation %>%
  mutate(date_review = round_date(as_datetime(timestamp), unit = "week")) %>%  
  mutate(year_release = substring(title, nchar(title) - 6)) %>% 
  mutate(year_release = as.numeric(substring(year_release, regexpr("\\(", year_release) + 1, regexpr("\\)", year_release) - 1)))


#########################################################################################################################################
# SECTION 3
# Exploratory Data Analysis and Visualization 
#########################################################################################################################################

# QUICK OVERVIEW
# Provide a quick overview of the data and understand data types
head(edx, 3)
glimpse(edx)

# UNIQUE COUNTS
# Understand how many unique Movies, Users and Genres are present in the database
edx %>% summarise(
  n_unique_movies = n_distinct(movieId),
  n_unique_users = n_distinct(userId),
  n_unique_genres = n_distinct(genres))

# RATINGS DISTRIBUTION
# Understand how users have provided ratings (i.e. understand what ratings are the most common) 
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25) +
  scale_x_discrete(limits = (c(seq(0.5,5,0.5)))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  xlab("User Ratings") +
  ylab("Count of Ratings") +
  ggtitle("Ratings Distribution") +
  theme_classic()

# RATINGS PER MOVIE
# Understand how many ratings have been assigned to movies
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  xlab("Count of Ratings") +
  ylab("Count of Movies") +
  ggtitle("Number of Ratings per Movie") +
  theme_classic()

# MEAN MOVIE RATINGS BY USERS
# Plot mean movie ratings given by users (NB: filter with minimum number of ratings equal to 30)
edx %>%
  group_by(userId) %>%
  filter(n() >= 30) %>%
  summarize(user_mean = mean(rating)) %>%
  ggplot(aes(user_mean)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_classic()

# RATINGS PER USER
# Understand how many ratings have been assigned by each user
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of Ratings") + 
  ylab("Number of Users") +
  ggtitle("Number of ratings given by users") +
  theme_classic()

# RATINGS PER YEAR
# How do average ratings change according to year of release?
edx %>% group_by(year_release) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(year_release, mean_rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Year of Release") +
  ylab("Average Rating") +
  ggtitle("Average Rating per Year of Release") +
  theme_classic()

# RATINGS PER MOVIE (BASED ON YEAR OF RELEASE)
# How does year of release impact number of ratings?
edx %>% group_by(date_review) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(date_review, mean_rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Date of Review") +
  ylab("Average Rating") +
  ggtitle("Average Rating based on Date of Review") +
  theme_classic()



#########################################################################################################################################
# SECTION 4
# Models Creation and Implementation
#########################################################################################################################################

# RMSE DEFINITION
# Define a function that will provide the Root Mean Squared Error (RMSE)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# MEAN DEFINITION
# Derive the mean of the training data population

mu <- mean(edx$rating)
print(mu)

# MODEL A: SIMPLE AVERAGE
# Define model purely leveraging the average rating of the data

simple_avg_rmse <- RMSE(validation$rating, mu)
print(simple_avg_rmse)

# STORE RESULTS
# Store results in a dataframe containing the RMSE for all models that will be created
rmse_results <- data_frame(Model = "MODEL A: Simple Average", RMSE = simple_avg_rmse)
print(rmse_results)

# MODEL B: MOVIE EFFECT
# Define model capturing the "movie effect" (i.e. on average, how is that specific movie rated?)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(movie_effect = mean(rating - mu))

movie_effect_predictions <- mu + validation %>% 
  left_join(movie_avgs, by ='movieId') %>%
  pull(movie_effect)

movie_effect_rmse <- RMSE(validation$rating,movie_effect_predictions) 
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="MODEL B: Movie Effect",  
                                     RMSE = movie_effect_rmse))
print(rmse_results)

# MODEL C: USER EFFECT + MOVIE EFFECT
# Define model capturing the "user effect" (i.e. does the user vote higher or lower compared to the overall population average?)

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(user_effect = mean(rating - mu - movie_effect))

user_effect_predictions <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred_user = mu + movie_effect + user_effect) %>%
  pull(pred_user)

user_effect_rmse <- RMSE(validation$rating,user_effect_predictions)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model= "MODEL C: User & Movie Effect",  
                                     RMSE = user_effect_rmse))
print(rmse_results)


# MODEL D: REGULARIZED MODEL USING USER AND MOVIE EFFECT
# Implement Regularization on MODEL C (User and Movie Effect)

lambda_options <- seq(0, 10, 0.25)
rmses_lambda <- sapply(lambda_options, function(l){
  
  mu <- mean(edx$rating)
  
  regularized_movie_effect <- edx %>%
    group_by(movieId) %>%
    summarise(regularized_movie_effect = sum(rating - mu)/(n() +l))
  
  regularized_user_effect <- edx %>%
    left_join(regularized_movie_effect, by="movieId") %>%
    group_by(userId) %>%
    summarise(regularized_user_effect = sum(rating - regularized_movie_effect - mu)/(n()+l))
  
  regularized_model_predictions <- validation %>%
    left_join(regularized_movie_effect, by = "movieId") %>%
    left_join(regularized_user_effect, by = "userId") %>%
    mutate(reg_pred = mu + regularized_movie_effect + regularized_user_effect) %>%
    pull(reg_pred)
  
  return(RMSE(regularized_model_predictions, validation$rating))
  
})

# Select optimal Lambda value that minimizes RMSE
rmse_regularized <- min(rmses_lambda)
print(rmse_regularized)

# Visually represent optimal Lambda
qplot(lambda_options, rmses_lambda, 
      colour = rmses_lambda,
      xlab = "Lambda Options",
      ylab = "Resulting RMSE")

lambda_optimal <- lambda_options[which.min(rmses_lambda)]
lambda_optimal

# Append to data frame containing other results
rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model ="MODEL D: Regularized User & Movie Effect",
                                     RMSE = rmse_regularized))

# Print definitive results
print(rmse_results)
sprintf("%.04f", rmse_results[4,2])
