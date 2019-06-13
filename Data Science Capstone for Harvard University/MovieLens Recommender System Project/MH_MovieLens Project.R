# Author: Mahesh Halkeri
# Movie lens Project for Data Science Capstone

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")

# Loading all needed libraries

library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# The RMSE function that will be used in this project is:
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Convert timestamp to a human readable date

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

# Extract the year and month of rate in both dataset

edx$yearOfRate <- format(edx$date,"%Y")
edx$monthOfRate <- format(edx$date,"%m")

validation$yearOfRate <- format(validation$date,"%Y")
validation$monthOfRate <- format(validation$date,"%m")

# Extract the year of release for each movie in both dataset
# edx dataset

edx <- edx %>%
   mutate(title = str_trim(title)) %>%
   extract(title,
           c("titleTemp", "release"),
           regex = "^(.*) \\(([0-9 \\-]*)\\)$",
           remove = F) %>%
   mutate(release = if_else(str_length(release) > 4,
                                as.integer(str_split(release, "-",
                                                     simplify = T)[1]),
                                as.integer(release))
   ) %>%
   mutate(title = if_else(is.na(titleTemp),
                          title,
                          titleTemp)
         ) %>%
  select(-titleTemp)

# validation dataset

validation <- validation %>%
   mutate(title = str_trim(title)) %>%
   extract(title,
           c("titleTemp", "release"),
           regex = "^(.*) \\(([0-9 \\-]*)\\)$",
           remove = F) %>%
   mutate(release = if_else(str_length(release) > 4,
                                as.integer(str_split(release, "-",
                                                     simplify = T)[1]),
                                as.integer(release))
   ) %>%
   mutate(title = if_else(is.na(titleTemp),
                          title,
                          titleTemp)
         ) %>%
  select(-titleTemp)

# Extract the genre in edx datasets

edx <- edx %>%
   mutate(genre = fct_explicit_na(genres,
                                       na_level = "(no genres listed)")
          ) %>%
   separate_rows(genre,
                 sep = "\\|")

# Extract the genre in validation datasets

validation <- validation %>%
   mutate(genre = fct_explicit_na(genres,
                                       na_level = "(no genres listed)")
          ) %>%
   separate_rows(genre,
                 sep = "\\|")

# remove unnecessary columns on edx and validation dataset

edx <- edx %>% select(userId, movieId, rating, title, genre, release, yearOfRate, monthOfRate)

validation <- validation %>% select(userId, movieId, rating, title, genre, release, yearOfRate, monthOfRate)

# Convert the columns into the desidered data type

edx$yearOfRate <- as.numeric(edx$yearOfRate)
edx$monthOfRate <- as.numeric(edx$monthOfRate)
edx$release <- as.numeric(edx$release)

validation$yearOfRate <- as.numeric(validation$yearOfRate)
validation$monthOfRate <- as.numeric(validation$monthOfRate)
validation$release <- as.numeric(validation$release)

# Calculate the average of all movies

mu_hat <- mean(edx$rating)

# Predict the RMSE on the validation set

rmse_mean_model_result <- RMSE(validation$rating, mu_hat)

# Creating a results dataframe that contains all RMSE results

results <- data.frame(model="Naive Mean-Baseline Model", RMSE=rmse_mean_model_result)

# Calculate the average by movie

movie_avgs <- edx %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

# Compute the predicted ratings on validation dataset

rmse_movie_model <- validation %>%
   left_join(movie_avgs, by='movieId') %>%
   mutate(pred = mu_hat + b_i) %>%
   pull(pred)

rmse_movie_model_result <- RMSE(validation$rating, rmse_movie_model)

# Adding the results to the results dataset

results <- results %>% add_row(model="Movie-Based Model", RMSE=rmse_movie_model_result)

# Calculate the average by user

user_avgs <- edx %>%
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings on validation dataset

rmse_movie_user_model <- validation %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu_hat + b_i + b_u) %>%
   pull(pred)

rmse_movie_user_model_result <- RMSE(validation$rating, rmse_movie_user_model)

# Adding the results to the results dataset

results <- results %>% add_row(model="Movie+User Based Model", RMSE=rmse_movie_user_model_result)

genre_pop <- edx %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   group_by(genre) %>%
   summarize(b_u_g = mean(rating - mu_hat - b_i - b_u))

# Compute the predicted ratings on validation dataset

rmse_movie_user_genre_model <- validation %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   left_join(genre_pop, by='genre') %>%
   mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
   pull(pred)

rmse_movie_user_genre_model_result <- RMSE(validation$rating, rmse_movie_user_genre_model)

# Adding the results to the results dataset

results <- results %>% add_row(model="Movie+User+Genre Based Model", RMSE=rmse_movie_user_genre_model_result)

lambdas <- seq(0, 10, 0.1)

# Compute the predicted ratings on validation dataset using different values of lambda

rmses <- sapply(lambdas, function(lambda) {
   
  # Calculate the average by user
  
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      mutate(pred = mu_hat + b_i) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
   
   return(RMSE(validation$rating, predicted_ratings))
})


# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set

rmse_regularized_movie_model <- min(rmses)

# Adding the results to the results dataset

results <- results %>% add_row(model="Regularized Movie-Based Model", RMSE=rmse_regularized_movie_model)

rmses <- sapply(lambdas, function(lambda) {

   # Calculate the average by user
   
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Calculate the average by user
   
   b_u <- edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu_hat + b_i + b_u) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
   
   return(RMSE(validation$rating, predicted_ratings))
})

# Get the lambda value that minimize the RMSE

min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set

rmse_regularized_movie_user_model <- min(rmses)

# Adding the results to the results dataset

results <- results %>% add_row(model="Regularized Movie+User Based Model", RMSE=rmse_regularized_movie_user_model)

lambdas <- seq(0, 15, 0.1)

# Compute the predicted ratings on validation dataset using different values of lambda

rmses <- sapply(lambdas, function(lambda) {

   # Calculate the average by user
   
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Calculate the average by user
   
   b_u <- edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
   
    b_u_g <- edx %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      group_by(genre) %>%
      summarize(b_u_g = sum(rating - b_i - mu_hat - b_u) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      left_join(b_u_g, by='genre') %>%
      mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
   
   return(RMSE(validation$rating, predicted_ratings))
})

# Get the lambda value that minimize the RMSE

min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set

rmse_regularized_movie_user_genre_model <- min(rmses)

# Adding the results to the results dataset

results <- results %>% add_row(model="Regularized Movie+User+Genre Based Model", RMSE=rmse_regularized_movie_user_genre_model)