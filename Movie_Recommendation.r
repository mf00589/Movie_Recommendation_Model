#    PRACTICAL BUSINESS ANALYTICS
#    GROUP: Hmm Aja
#    Movie Recommendation System

# Clear objects in "global environment"
rm(list=ls())

#**************START_OF_SCRIPT******************
MOVIES_DATASET_FILENAME <- "movies.csv"
RATINGS_DATASET_FILENAME <- "ratings.csv"

#   Define Constants here

TYPE_NUMERIC      <-"NUMERIC"
TYPE_ORDINAL      <-"ORDINAL"
TYPE_IGNORE       <-"IGNORE" 
TYPE_SYMBOLIC     <-"SYMBOLIC" 
TYPE_DISCREET     <-"DISCREET"
CUTOFF_DISCREET   <- 5
SPLIT_DATASET     <- .75
KMEANS_DATASET_SPLIT <- .1
ANGLE_VALUE <- 90
K_MEANS_SIZE <- 0.05
CLUSTER_SIZE<-16
USER_ID<-62
ZERO<-0
TRAIN_PERCENT<-0.75
TEN<-10


set.seed(101)



#Define and load the R libraries for the project
MYLIBRARIES<-c("ggplot2",
               "data.table",
               "reshape2",
               "recommenderlab",
               "caret",
               "tidyverse",
               "PerformanceAnalytics",
               "pacman",
               "outliers",
               "corrplot",
               "MASS",
               "formattable",
               "tidyr",
               "dplyr",
               "lubridate",
               "stringr",
               "rvest",
               "XML",
               "tidytext",
               "wordcloud")



#Lab functions

# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
# ************************************************
# Uses histogram
# Plots histogram for visulisation
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      scaled_column<-Nrescale(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      #Bar chart helps visulisation. Type of field is the chart name
      barplot(bins, main=paste(graphTitle,field_types[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")
      
    } #endif numeric types
  } #endof for
  return(field_types)
}

# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}

# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
NPREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}

# NPREPROCESSING_initialFieldType() :
#
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
NPREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}

# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
# ************************************************
# Uses histogram
# Plots histogram for visulisation
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      scaled_column<-Nrescale(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      #Bar chart helps visulisation. Type of field is the chart name
      barplot(bins, main=paste(graphTitle,field_types[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")
      
    } #endif numeric types
  } #endof for
  return(field_types)
}



#***********************Own functions*********************

# Hist_for_user_rating()  :
# function which plots a histogram of number of ratings from users
# by having a subgroup for users from 1 to 600
#
# INPUT   : Datasetcoloumn of users Id
#
# OUTPUT  : A Histogram 

Hist_for_user_ratings<- function(dataset){
  return(hist(dataset,
              col = 'yellow'))
}

# Histfunction_For_ratings()  :
# function which plots a histogram of the ratins recieved for all movies
#
# INPUT   : Datasetcoloumn of rating
#
# OUTPUT  : A Histogram 
Histfunction_For_ratings<- function(dataset){
  return( hist(dataset,
               col = 'darkmagenta',
               breaks = c(0,1,2,3,4,5),
               xlim=c(0,5),
               freq=TRUE,
               xlab="Rating",
  ))
}

# createEncoding()  :
# function which takes the genres coloumn 
# and splits the themes into one hot encoding 
#
# INPUT   : Movies Dataset
#
# OUTPUT  : One hot encoding of Genres 
createEncoding <- function(movies){
  #creating one-hot-encoding for genres
  #get the genres column for movies data set
  genres_from_movies <- as.data.frame(movies$genres, stringsAsFactors = FALSE)
  #each row in genres column consist of multiple genres so split the genres for each row
  split_genres_from_movies <- as.data.frame(tstrsplit(genres_from_movies[,1], '[|]',type.convert = TRUE),stringAsFactors=FALSE)
  colnames(split_genres_from_movies) <- c(1:10)
  #list of all the genres
  genres <- c("Action", "Adventure", "Animation", "Children", 
              "Comedy", "Crime","Documentary", "Drama", "Fantasy",
              "Film-Noir", "Horror", "Musical", "Mystery","Romance",
              "Sci-Fi", "Thriller", "War", "Western")
  #create an empty matrix to store genres
  matrix <- matrix(0,10330,18)
  matrix[1,] <- genres
  colnames(matrix) <- genres
  
  #iterates through the split genres dataset an adds 1 to the matrix if the genre is present in the movies row
  for (index in 1:nrow(split_genres_from_movies)) {
    for (col in 1:ncol(split_genres_from_movies)) {
      genrate_col = which(matrix[1,] == split_genres_from_movies[index,col]) 
      matrix[index+1,genrate_col] <- 1
    }
  }
  second_matrix <- as.data.frame(matrix[-1,], stringAsFactors= FALSE)
  for (col in 1:ncol(second_matrix)) {
    second_matrix[,col] <- as.integer(second_matrix[,col]) #convert from characters to integers
  } 
  
  
  return(second_matrix)# returns the encoded genres which are split for every movie 
  
  
}

# NORMALIZE()  :
# function which normalises the rating value from 0-5 to 0-1
# INPUT   : Ratings Coulumn
#
# OUTPUT  : Normalised version of ratings
NORMALIZE <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Randomise_dataset()  :
# Function which randomises the dataset for modelling stage
# This insure model is not biased
# INPUT   : Dataset
#
# OUTPUT  : A Dandomised Dataset
Randomise_dataset <- function(dataset){
  set.seed(42)
  rows <- sample(nrow(dataset))
  return(dataset[rows, ])
  
}


# ibcf_preprocessing() :
#
# Returns a processed matrix ready to be fit into the IBCF model (rec_model)
#
# INPUT: initial pre-processed (and normalised) dataset
#
# OUTPUT : matrix of the required type - realRatingMatrix
# ************************************************
ibcf_preproccesing <- function(movies_db) {
  
  # selecting only the required columns from the dataset
  ratings_ibcf <- subset(movies_db, select=c(userId, movieId, rating))
  
  # transforming the dataset so that users are rows, movieIds are columns and ratings are the cell values
  ratings_matrix <- dcast(ratings_ibcf, userId~movieId, value.var = "rating", na.rm=FALSE)
  
  # transforming to a matrix and removing the userId column
  ratings_matrix <- as.matrix(ratings_matrix[,-1])
  
  # transforming to a realRatingMatrix - the required type for the IBCF recommendation model
  movie_ratings <- as(ratings_matrix, "realRatingMatrix")
  
  # only select users who have rated a movie at least 40 times and movies that have received at least 40 ratings
  movie_ratings = movie_ratings[rowCounts(movie_ratings) > 40,colCounts(movie_ratings) > 40]
  
  return(movie_ratings) 
}

# ibcf_model() :
#
# Returns the object containing the list of N recommended movies to users
#
# INPUT: matrix of type realRatingMatrix
#
# OUTPUT : top N predicted recommendations 
# ************************************************
ibcf_model <- function(movie_ratings) {
  
  # creating the training and testing sample
  sample <- sample.int(n = nrow(movie_ratings), size = floor(SPLIT_DATASET*nrow(movie_ratings)), replace = F)
  # training dataset
  trainset <- movie_ratings[sample, ]
  # testing dataset
  testset  <- movie_ratings[-sample, ]
  
  # Create the recommendation model
  rec_model <- Recommender(data = trainset,
                           method = "IBCF",
                           parameter = list(k = 30, method = "Cosine"))
  
  recs <- 10 # number of movies to recommend
  
  # predicting movies for users
  predictions <- predict(object = rec_model, newdata = testset, n = recs, type=c("topNList"))
  
  return(predictions)
}

# ibcf_accuracy() :
#
# Evaluates accuracy (RMSE, MAE and MSe) for IBCF on unseen data
#
# INPUT: movie_ratings - realMatrix of movie_ratings
#        training_part - int - 
#
# OUTPUT : top N predicted recommendations 
# ************************************************
ibcf_accuracy <- function(movie_ratings, training_part){
  # Creating training and testing data
  good_rating <- 3      # rating threshold
  recs_number <- 10     # number of recommendations to be made
  eval_n <-1
  
  sample_data<- evaluationScheme(data = movie_ratings, method="split", train=training_part, 
                                 given=10, goodRating=good_rating, k=eval_n)
  
  # Create the recommendation model
  rec_model <- Recommender(data = getData(sample_data, "train"),
                           method = "IBCF",
                           parameter = list(k = 30, method = "Cosine"))
  
  recs <- 10 # number of movies to recommend
  
  # predicting movies for users using "ratings" type
  rpredictions <- predict(object = rec_model, newdata = getData(sample_data, "known"), n = recs, type="ratings")
  
  # Get the evaluation
  ratings_eval_accuracy<-calcPredictionAccuracy(
    x = rpredictions, data = getData(sample_data, "unknown")
  )
  
  return(ratings_eval_accuracy)
}

# recommendations_ibcf() :
#
# Return 10 Movie Recommendations for the i-th User
#
# INPUT: User Id
#
# OUTPUT : list of recommended movie titles
# ************************************************
ibcf_recommendations <- function(user_number) {
  
  # creating a dataframe of the initial, unprocessed movies dataset & removing the genres column
  movies_df <- read.csv(MOVIES_DATASET_FILENAME)
  movies_df <- subset(movies_df, select=-c(genres))
  
  print(sprintf("Movie recommendations for user %s", user_number))
  
  # finds the topN list for the inputted userId
  rec <- predictions@items[[user_number]]
  rec1 <- predictions@itemLabels[rec]
  rec2 <- rec1
  # the 'predicted' object contains recommended movies as movieIds (integers)
  # need to map the movieIds in the list to their respective titles using the movies_df
  for (index in 1:10){
    rec2[index] <- as.character(subset(movies_df, movies_df$movieId == rec1[index])$title)
  }
  return(rec2)
}


# rec_ibcf() :
#
# Return 10 Movie Recommendations for the i-th User in the form of movie ID
#
# INPUT: User Id
#
# OUTPUT : list of recommended movie titles
# ************************************************
rec_ibcf <- function(user_number) {
  
  # creating a dataframe of the initial, unprocessed movies dataset & removing the genres column
  movies_df <- read.csv(MOVIES_DATASET_FILENAME)
  movies_df <- subset(movies_df, select=-c(genres))
  
  userId <- which(userIds_ibcf_testset == user_number)
  
  # finds the topN list for the inputted userId
  rec <- hitrate_predictions@items[[userId]]
  rec1 <- hitrate_predictions@itemLabels[rec]
  
  return(rec1)
}

#If a movie that the user has actually rated exists in the top N recommendation.
#it means that it's a hit

#************************************************
#*getHitRate():
#*Calculates the hit rate for IBCF
#*
#*INPUT: ratings - ratings dataframe 
#*
#*OUTPUT: double - hit rate of model
#************************************************
getHitRate<-function(ratings){
  usersId<-userIds_ibcf_testset
  
  hits <- 0
  total<- 0
  totalUser<-0
  for(user in 1:length(usersId)){
    #call the function topNRecommendation to get the top n recommendation
    #for each user
    userTopNId <- usersId[user]
    topN<-rec_ibcf(userTopNId)
    if (length(topN) > 0 ){
      for(i in 1:length(topN)){
        #Gets the movies that the user has actually rated
        userRatedMovies <- ratings[(ratings$userId == userTopNId),]
        if (nrow(userRatedMovies) == 0){
          print(paste("ERROR at ", user))
        } else {
          for(j in 1:nrow(userRatedMovies)){
            #Checks if any of the movies in the top n recommendation list is a movie
            #that the user has rated before
            if (topN[i] == userRatedMovies$movieId[j]){
              if(userRatedMovies$rating[j] >= 3){
                #hits increase by 1 when it returns true
                hits <- hits + 1
              }
            }
            total <- total + 1
          }
        }
        
    }
    }
  }
  
  print(paste("Hits: ", hits))
  print(paste("Total Users: ", total))
  
  #Gets the hit rate which is the number of hits divided by the amount of users
  hit_rate <- hits/total
  return(hit_rate)
} #end of getHitRate()


#************************************************
#*precision_recall():
#*Gets precision and recall for each user for IBCF model
#*
#*INPUT: ratings - dataframe - ratings dataframe 
#*       training part - double - size of training in percentage
#*
#*OUTPUT: double - hit rate of model
#************************************************
precision_recall <- function(movie_ratings, training_part){
  good_rating <- 3      # rating threshold
  recs_number <- 10     # number of recommendations to be made
  eval_n <-1
  
  sample_data<- evaluationScheme(data = movie_ratings, method="split", train=training_part, given=10, goodRating=good_rating, k=eval_n)
  
  # Create the recommendation model
  rec_model <- Recommender(data = getData(sample_data, "train"),
                           method = "IBCF",
                           parameter = list(k = 30, method = "Cosine"))
  
  recs <- 10 # number of movies to recommend
  
  # predicting movies for users using "topNList" type
  topNpredictions <- predict(object = rec_model, newdata = getData(sample_data, "known"), n = recs, type=c("topNList"))
  
  # Get the evaluation
  # predicted, good_rating, and recs_number taken from modelling part
  topN_eval_accuracy <- calcPredictionAccuracy(
    x = topNpredictions, data = getData(sample_data, "unknown"), goodRating = good_rating, given = recs_number, byUser = TRUE
  )
  
  # Format results as a data frame
  topN_eval_df <- as.data.frame(topN_eval_accuracy)
  
  return(topN_eval_df)
}

# ************************************************
# precisionAtK() :
#
# Calculate the precision at cut-off k 
#
# INPUT   :   integer         - k               - cut-off for prediction to be evaluated
#             vector integer  - actual          - list of movies a user like (movie ID)
#             vector integer  - recommendations - ordered predictions for the user (movie ID)
#
# OUTPUT  :   double          - precision at k of prediction
#
# ************************************************
precisionAtK <- function(k, actual, recommendations){
  
  # Cut off the recommendations at k
  cutoff <- head(recommendations, k)
  #print(cutoff)
  # Number of correct recommendations (true positive)
  TP <- 0
  # Calculate the number of true positives
  if (length(cutoff) > 0){
    for(movie in 1:length(cutoff)){
      if(cutoff[movie] %in% actual && actual >= 3){
        TP <- TP + 1
      }
    }
  }
  #cat("TP:", TP , "/" , k, "\n")
  # Return the precision of the recommendations 
  return(TP/k)
  
}

# ************************************************
# recallAtK() :
#
# Calculate the recall at cut-off k 
#
# INPUT   :   integer         - k               - cut-off for prediction to be evaluated
#             vector integer  - actual          - list of movies a user like (movie ID)
#             vector integer  - recommendations - ordered predictions for the user (movie ID)
#
# OUTPUT  :   double          - recall at k of prediction
#
# ************************************************
recallAtK <- function(k, actual, recommendations){
  
  # Cut off the recommendations at k
  cutoff <- head(recommendations, k)
  #print(cutoff)
  # Number of correct recommendations (true positive)
  TP <- 0
  # Calculate the number of true positives
  if (length(cutoff) > 0){
    for(movie in 1:length(cutoff)){
      if(cutoff[movie] %in% actual&& actual >= 3){
        TP <- TP + 1
      }
    }
  }
  liked <- actual >= 3
  #cat("TP:", TP , "/" , length(actual), "\n")
  # Return the precision of the recommendations 
  return(TP/length(liked))
  
}

# ************************************************
# meanPrecisionAtK() :
#
# Calculate the mean of precision at cut-off k of all users
#
# INPUT   :   integer         - k               - cut-off for prediction to be evaluated
#             data frame      - actual          - Liked movies for all users
#             data frame      - recommendations - Movie predictions for all users (movie ID)
#
# OUTPUT  :   double          - mean of precision at k of model
#
# ************************************************
meanPrecisionAtK <- function(k, userIds, actual, recommendations){
  
  # Sum of precision for all users
  sum <- 0.0
  # Loop through all users
  for(user in userIds){
    # Get recommendations for the user
    recMovies <- (subset(recommendations, userId == user))$movieId
    # Get the movie IDs the user likes
    likedMovies <- (subset(actual, userId == user))$movieId
    # Get the precision@k for the user
    p <- precisionAtK(k, likedMovies, recMovies)
    cat("User: ", user, " Precision: ", p, "\n")
    # Add the precision to the total sum
    sum <- sum + p
  }
  # Divide the sum of the precisions by the number of the users
  return(sum/length(userIds))
  
}

# ************************************************
# meanPrecisionAtK() :
#
# Calculate the mean of precision at cut-off k of all users
#
# INPUT   :   integer         - k               - cut-off for prediction to be evaluated
#             data frame      - actual          - Liked movies for all users
#             data frame      - recommendations - Movie predictions for all users (movie ID)
#
# OUTPUT  :   double          - mean of precision at k of model
#
# ************************************************
meanRecallAtK <- function(k, userIds, actual, recommendations){
  
  # Sum of precision for all users
  sum <- 0.0
  # Loop through all users
  for(user in userIds){
    # Get recommendations for the user
    recMovies <- (subset(recommendations, userId == user))$movieId
    # Get the movie IDs the user likes
    likedMovies <- (subset(actual, userId == user))$movieId
    # Get the precision@k for the user
    p <- recallAtK(k, likedMovies, recMovies)
    cat("User: ", user, " Recall: ", p, "\n")
    # Add the precision to the total sum
    sum <- sum + p
  }
  # Divide the sum of the precisions by the number of the users
  return(sum/length(userIds))
  
}

# ************************************************
# avgPrecisionAtK() :
#
# Calculate the average Precision at cut-off k 
#
# INPUT   :   integer         - k               - cut-off for prediction to be evaluated
#             vector integer  - actual          - list of movies user likes (movie ID)
#             vector integer  - recommendations - ordered predictions for the user (movieID)
#
# OUTPUT  :   double          - precision at k of prediction
#
# ************************************************
avgPrecisionAtK <- function(k, actual, recommendations){
  
  # Cut off the recommendations at k
  cutoff <- head(recommendations, k)
  # Number of correct recommendations (true positive)
  TP <- 0
  # Weighted TP sum
  wTPSum <- 0.0
  # Loop through the movies
  for(movie in 1:length(cutoff)){
    # Check if true positive
    if(cutoff[movie] %in% actual){
      TP <- TP+1
      # Divide the TP by the movie ranking to get get weighted score 
      # Higher ranking higher score
      wTP <- TP/movie
      # Add the score to the total sum
      wTPSum <- wTPSum + wTP
    }
  }
  # Return the average precision at cut-off k
  return(wTPSum/k)
  
}

# ************************************************
# meanAveragePrecisionAtK() :
#
# Calculate the mean average precision at cut-off k of all users
#
# INPUT   :   integer         - k               - cut-off for prediction to be evaluated
#             data frame      - actual          - Liked movies for all users
#             data frame      - recommendations - Movie predictions for all users (movie ID)
#
# OUTPUT  :   double          - mean of average precision at k of model
#
# ************************************************
meanAveragePrecisionAtK <- function(k, userIds, actual, recommendations){
  
  # Sum of precision for all users
  sum <- 0.0
  # Loop through all users
  for(user in userIds){
    # Get recommendations for the user
    recMovies <- (subset(recommendations, userId == user))$movieId
    # Get the movie IDs the user likes
    likedMovies <- (subset(actual, userId == user))$movieId
    # Get the precision@k for the user
    p <- avgPrecisionAtK(k, likedMovies, recMovies)
    cat("User: ", user, " Average Precision: ", p, "\n")
    # Add the precision to the total sum
    sum <- sum + p
  }
  # Divide the sum of the precisions by the number of the users
  return(sum/length(userIds))
  
}


# main()  :
# main entry point to the script
#
# INPUT   : None
#
# OUTPUT  : None  
main<- function(){
  print("Inside main function")
  print(MOVIES_DATASET_FILENAME)
  print(RATINGS_DATASET_FILENAME)
  
  
#***********ERROR CHECKING WHEN IMPORTING THE DATASET*****************
  #Tries to read the movies.csv file
  Read_Movies <- try(read.csv(MOVIES_DATASET_FILENAME, stringsAsFactors = FALSE))

  #checks if this gives any errors 
if (class(Read_Movies) != "try-error") {
  movies<- read.csv(MOVIES_DATASET_FILENAME, stringsAsFactors = FALSE)
} else {
  #if file not found output the following message
  message("Movies File doesn't exist, please check")
}
  #Tries to read the movies.csv file
  Read_Ratings <- try(read.csv((RATINGS_DATASET_FILENAME)))
  
  #checks if this gives any errors 
  if (class(Read_Ratings) != "try-error") {
    ratings<- read.csv((RATINGS_DATASET_FILENAME))
  } else {
    #checks if this gives any errors 
    message("Ratings File doesn't exist, please check")
  }
  
  #ratings dataframe for hit rate
  ratings_db <-read.csv((RATINGS_DATASET_FILENAME))
  # remove timestamp column
  ratings_db = as.data.frame(ratings_db)
  
  ratings_hitrate <-read.csv((RATINGS_DATASET_FILENAME))
  # remove timestamp column
  ratings_hitrate <<- as.data.frame(ratings_db)
  
  
  #*******************DATA Analysis**********************#(Mark)
  #Ignore warning messages
  options(warn=-1)
  
  #extracts date from the title column in the movies dataset
  mutate(movies,title = str_trim(title))
  movies_new<-extract(movies,title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)
  
  #omits any records which does not have a year speciifed in the title coloumn 
  na.omit(movies_new) 
  #converts the years coloum from characters to numbers 
  movies_new$year <- as.numeric(movies_new$year)
  na.omit(movies_new) 
  
  #groups the movies into years from 1902 to 2015
  yearly_movies_count <- movies_new %>%
    na.omit() %>% 
    dplyr::select(movieId, year) %>%
    group_by(year) %>% 
    summarise(count = n())  %>% 
    arrange(year)
  
  #prints the years and the number of movies 
  print(yearly_movies_count)
  
  
  #plots a graph which show how many movies were relased for each year
  #movies_per_year %>%
  # ggplot(aes(x = year, y = count)) +
  #geom_line(color="blue")
  plot<-ggplot(yearly_movies_count,aes(x = year, y = count)) + 
    ggtitle("Number of movies Released For each Year")+
    geom_line(color='Red')
  
  print(plot)
  
  #splits the genres from the genres table as multiple genres are present for each row
  genres_list <- movies %>%
    separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    summarise(number = n()) %>%
    arrange(desc(number))
  
  print(genres_list)
  
  #plots the graph which show which the most popluar genre for movies 
  genre<-ggplot(data = genres_list, aes(x = genres, y = number, group = 1)) +
    geom_line(color='blue')+
    ggtitle("Number of movies Released For each Year")+
    theme(axis.text.x = element_text(angle = ANGLE_VALUE))+
    geom_point()
  
  print(genre)
  
  #Analyse the rating dataset to see the distrubtion of ratins from 0-5
  pdf("Rplot.pdf")# saves the graph to a pdf file  
  Histfunction_For_ratings(ratings$rating)
  dev.off() 
  
  #Analyse the distrubtion of the users who have rated movies  
  pdf("Rplot.pdf") 
  Hist_for_user_ratings(ratings$userId)
  dev.off() 
  
  
  #check for any missing values that might be present withing the rating and movies dataset
  missing_values_ratings<-ratings[!complete.cases(ratings),]
  missing_values_movies<-movies[!complete.cases(movies),]
  print(missing_values_ratings)
  print(missing_values_movies)
  
  
  #*****************Preprocessing*************************#(Mark)
  
  #Determine if fields are NUMERIC OR SYMBOLIC FOR BOTH MOVIES AND RATINGS
  Movie_fields<-NPREPROCESSING_initialFieldType(movies)
  Rating_fields<-NPREPROCESSING_initialFieldType(ratings)
  print(Movie_fields)
  print(Rating_fields)
  
  #FOR NUMERIC FIELDS DETERMINE IF THEY ARE ORDINAL OR DISCREET
  Movie_fields_types<-NPREPROCESSING_discreetNumeric(movies,Movie_fields,5)
  Rating_fields_types<-NPREPROCESSING_discreetNumeric(ratings,Rating_fields,5)
  print(Movie_fields_types)
  print(Rating_fields_types)
  
  #MAKE A DATAFRAME OF THE RESULTS FOR ABBOVE FOR BOTH TABLES
  results_movies<-data.frame(Field=names(movies),Initial=Movie_fields,Types=Movie_fields_types)
  print(formattable::formattable(results_movies))
  
  results_ratings<-data.frame(Field=names(ratings),Initial=Rating_fields,Types=Rating_fields_types)
  print(formattable::formattable(results_ratings))
  
  #Outliers in the ratings can be checked through the pretty dataset function as the min should be 0 and max should be 5
  NPREPROCESSING_prettyDataset(ratings)
 
  
  #normalise the rating field from a range of (0.5-5) to (0-1)
  ratings$rating<-NORMALIZE(ratings$rating)
  NPREPROCESSING_prettyDataset(ratings_db)
  NPREPROCESSING_prettyDataset(ratings)
  
  #remove timestamp field from the data frame
  DROP<-c("timestamp")
  ratings = ratings[,!(names(ratings)%in% DROP)]
  NPREPROCESSING_prettyDataset(ratings)
  
  #Field Encoding of genres coloumn in movies table
  encoded_genre<-createEncoding(movies)
  
  #combines the encoded genres to the initial movies data set 
  movies_genre <- cbind(movies[,1:2], encoded_genre)
  
  #joins the rating and user table together 
  movies_db <- left_join(ratings,movies_genre, by = "movieId")
  ratings_db <- left_join(ratings_db, movies_genre, by = "movieId")
  NPREPROCESSING_prettyDataset(movies_db)
  
  #randomise the dataset
  movies_db<-Randomise_dataset(movies_db)
  ratings_db<-Randomise_dataset(ratings_db)
  
  
  #*******************The following data set is preprocessed and can be used in the modeling stage 
  
  # Slpit the dataset into testing and training
  sample <- sample.int(n = nrow(movies_db), size = floor(SPLIT_DATASET*nrow(movies_db)), replace = F)
  train <- movies_db[sample, ]
  test  <- movies_db[-sample, ]
  
  # data set is reduced as earlier it exceeded available memory
  sample_size <- floor(KMEANS_DATASET_SPLIT * nrow(movies_db))
  train_ind <- sample(1: nrow(movies_db), size = sample_size)
  reducedData <- movies_db[train_ind, ]
  copytrain <- movies_db[train_ind, ]
  ntest <- movies_db[-train_ind,]
  
  sample_size_test <- floor(K_MEANS_SIZE  * nrow(movies_db))
  test_ind <- sample(1: nrow(ntest), size = sample_size_test)
  new_test <- ntest[test_ind,]
  
  drops <- c("title")
  reducedData <- reducedData[ , !(names(reducedData) %in% drops)]
  train <- train[ , !(names(train) %in% drops)]
  test <- test[, !(names(test) %in% drops)]
  
  # Finds the optimal k 
  # To find optimal K the silhouette method is used
  # What scale does is allow for the use of all variables
  optimalK<-factoextra::fviz_nbclust(scale(reducedData), kmeans, method = "silhouette",k.max = 20)
  print(optimalK)
  
  # Using K-means we can cluster according to user ratings
  kmeansClusters<-CLUSTER_SIZE
  modelKmeans <- kmeans(scale(train), kmeansClusters, iter.max = 100)
  
  # Stores k-means clustered data into data frame 
  out <- cbind(train, clusterNum = modelKmeans$cluster)
  head(out) 
  
  #print(out)
  
  # Fills in unrated movies with mean from column
  out %>%
    group_by(movieId, clusterNum) %>%
    mutate_at("rating",
              function(x) replace(
                x,
                is.na(x),
                mean(x, na.rm = TRUE)
              ))
  
  # Sorts data in terms of userId and ratings
  ordered_out <<- arrange(out, desc(userId), desc(rating))
  
  out_sorted <<- arrange(ordered_out, desc(userId), desc(movieId))
  
  #print(ordered_out)
  
  # Gets all unrated movies into a dataset 
  predMovies <- copytrain %>%
    filter(rating == 0)
  
  predMovies <- arrange(predMovies, desc(userId), desc(rating))
  
  #Train set for checking Accuracy
  new_unratedmovies <- copytrain %>%
    filter(rating == 0)
  new_unratedmovies <- arrange(new_unratedmovies, desc(userId), desc(rating))
  
  # # Sorts data in terms of userId and ratings
  # reducedData<- copytrain %>% 
  #   filter(rating > 0.4)
  # reducedData <- arrange(reducedData, desc(userId), desc(rating))
  # #print(head(reducedData))
  
  # ordered_out <- arrange(out, desc(userId), desc(rating))
  # #print(ordered_out)
  
  # updates ratings to reflect predicted ratings
  predMovies$rating[predMovies$movieId %in% ordered_out$movieId] <- ordered_out$rating
  
  new_unratedmovies <<- arrange(new_unratedmovies, desc(userId), desc(movieId))
  predMovies <<- arrange(predMovies, desc(userId), desc(movieId))
  
  Kmeans_test <- new_test
  Kmeans_test <<- arrange(Kmeans_test, desc(userId), desc(movieId))
  
  Kmeans_test$rating[Kmeans_test$movieId %in% ordered_out$movieId] <- ordered_out$rating
  
  new_test <<- arrange(new_test, desc(userId), desc(movieId))
  Kmeans_test <<- arrange(Kmeans_test, desc(userId), desc(movieId))
  
  #print("KMEANS TEST")
  #print(head(Kmeans_test))
  
  #print("NEW TEST")
  #print(head(new_test))
  # # Saves recommendation values to new data frame
  # predMovies <- copytrain %>% 
  #   filter(rating > 0.4)
  # 
  # 
  # 
  # # updates ratings to reflect predicted ratings
  # 
  # predMovies$rating[predMovies$movieId %in% ordered_out$movieId] <- ordered_out$rating
  
  # Remove all unneeded columns
  drop <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
            "Film-Noir", "Horror", "Musical", "Mystery", "Western", "Romance", "Sci-Fi", "Thriller", "War")
  predMovies <- predMovies[ , !(names(predMovies) %in% drop)]

  #print("USER IDs IN predMovies")
  #print(unique(predMovies$userId))
  
  # print(predMovies)
  
  # here is a user which we will find their top 5 recommended movies are
  user_Id <- USER_ID
  
  # this is the top 5 predictions for all users in our reducedData data set
  top5predictions <-predMovies %>%
    group_by(userId) %>%
    arrange(desc(rating)) %>%
    slice(1:5)
  
  # Here we get the top 5 predictions for user_Id
  userPred <- top5predictions %>%
    group_by(userId) %>%
    filter(userId == user_Id) %>%
    arrange(desc(rating))
  
  
  # here is a function that will get users top 5 recommended movies 
  # getUserPredictions <- function(userID) {
  #   userPred <- top10predictions %>%
  #     group_by(userId) %>%
  #     filter(userId == userID)
  #   
  #   print(userPred)
  # }
  
  # Printing out the values
  print(top5predictions)
  # getUserPredictions(13)
  print(userPred)
 
  # ********* IBCF MODEL CODE **************** (Val)
  
  # calling the functions and assigning them to variables 
  #sample <- sample.int(n = nrow(movies_db), size = floor(SPLIT_DATASET*nrow(movies_db)), replace = F)
  
  movie_ratings_normalized <- ibcf_preproccesing(movies_db)
  movie_ratings <- ibcf_preproccesing(ratings_db)
  
  sample <- sample.int(n = nrow(movie_ratings_normalized), size = floor(SPLIT_DATASET*nrow(movie_ratings_normalized)), replace = F)
  train_ibcf <- movie_ratings_normalized[sample, ]
  test_ibcf <<- movie_ratings_normalized[-sample, ]
  
  predictions <<- ibcf_model(movie_ratings_normalized)
  print(predictions)
  
  # Create the recommendation model
  rec_model <- Recommender(data = train_ibcf,
                           method = "IBCF",
                           parameter = list(k = 30, method = "Cosine"))
  
  # predicting movies for users
  hitrate_predictions <<- predict(object = rec_model, newdata = test_ibcf, n = 10, type=c("topNList"))
  
  userIds_ibcf_testset <<- unique(test_ibcf@data@i)
  
  #print(rec_ibcf(1))
  print(ibcf_recommendations(1))
  
  # ********************** EVALUATION: ACCURACY AND HIT RATE ****************(Miko)
  
  # ********************** HIT RATE ***********************************************
  hits <- ZERO
  total<- ZERO
  totalUser<-ZERO
  usersId<-unique(new_test$userId)
  
  top10predictions <-Kmeans_test %>%
    group_by(userId) %>%
    arrange(desc(rating))
  
  for(user in 1:length(usersId)){
    #call the function topNRecommendation to get the top n recommendation
    #for each user
    userTopNId <- usersId[user]
    
    topN <- top10predictions %>%
      group_by(userId) %>%
      filter(userId == userTopNId) %>%
      arrange(desc(rating)) %>%
      slice(1:10)
    
    if(nrow(topN) > 0){
      for(i in 1:nrow(topN)){
        #Gets the movies that the user has actually rated
        userRatedMovies <- ratings_hitrate[(ratings_hitrate$userId == userTopNId),]
        for(j in 1:nrow(userRatedMovies)){
          #Checks if any of the movies in the top n recommendation list is a movie
          #that the user has rated before
          if (topN$movieId[i] == userRatedMovies$movieId[j]){
            if (userRatedMovies$rating[j] >= 3){
              #hits increase by 1 when it returns true
              hits <- hits + 1
            }
          }
          total <- total + 1
        }
      }
    } else {
      print("none for this user")
    }
  }
  
  #Gets the hit rate which is the number of hits divided by the amount of users
  KMeans_hitrate <- hits/total
  print(paste("Hit rate for K-Means is: ", (KMeans_hitrate)))
  
  #IBCF
  training_part <- TRAIN_PERCENT  # percent of data allocated for training
  
  ibcf_hitrate <- getHitRate(ratings_hitrate)
  
  print(paste("Hit rate for IBCF is: ", ibcf_hitrate))
  
  # Create data
  data_hitrate <<- data.frame(
    models=c("K-Means", "IBCF") ,  
    value=c(format(KMeans_hitrate, nsmall = 6), format(ibcf_hitrate, nsmall = 6))
  )
  
  # Barplot
  hitrate_barchart <- ggplot(data_hitrate, aes(x=models, y=value)) + geom_bar(stat = "identity") +
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=0)
  
  # Add titles
  hitrate_barchart <- hitrate_barchart + labs(title = "K-Means vs IBCF (Hit Rate)",subtitle = "Higher is better")
  
  png("hitrate.png")
  print(hitrate_barchart)     # accuracy_barchart 1 --> in the first page of PDF
  dev.off() 
  
  
  # ********* ACCURACY: RMSE, MAE, MSE *********************************************
  # ********* K-MEANS *********************************************
  
  #Print length of ordered_out and train to make sure they have the same dimension
  #Kmeans_test is used because it has the predicted rating from the movies that are
  #in the new_test dataframe
  print(paste("Length of Kmeans_test: ", nrow(Kmeans_test)))
  print(paste("Length of new_test: ", nrow(new_test)))
  
  #Calculating accuracy by using train and Kmeans_test dataframe
  #new_test has the actual rating
  #Kmeans_test has the predicted rating
  KMAE<-mean(abs(new_test$rating - Kmeans_test$rating))
  KMSE<-mean((new_test$rating - Kmeans_test$rating)^2)
  KRMSE<-sqrt(mean((new_test$rating - Kmeans_test$rating)^2))
  
  #Prints Accuracy evaluation for K-Means on unseen data
  print("K-Means Accuracy:")
  print(paste("MAE: ", KMAE, ", MSE: ", KMSE, ", RMSE: ", KRMSE))
  
  #**************************** IBCF *********************************************
  
  #calls ibcf_accuracy() function to get the accuracy for IBCF
  ibcfa<-ibcf_accuracy(movie_ratings_normalized, training_part)
  
  #Prints Accuracy evaluation for IBCF
  print("IBCF Accuracy:")
  print(ibcfa)
  
  #Creating grouped bar chart for Accuracy
  models = c("K-Means", "IBCF")
  
  RMSE <- c(format(round(KRMSE, 2), nsmall = 2), format(round(ibcfa[1], 2), nsmall = 2)) 
  MSE <- c(format(round(KMSE, 2), nsmall = 2), format(round(ibcfa[2], 2), nsmall = 2))
  MAE <- c(format(round(KMAE, 2), nsmall = 2), format(round(ibcfa[3], 2), nsmall = 2))
  
  data_accuracy <<- data.frame(models,RMSE,MSE,MAE)
  
  data.m<-melt(data_accuracy, id="models")
  
  # Grouped bar chart for accuracy
  
  accuracy_barchart <- ggplot(data.m, aes(variable, value)) + 
    geom_bar(aes(fill = models), width = 0.4, position = position_dodge(width=0.5), stat="identity") +
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.5)
  
  # Add titles
  accuracy_barchart <- accuracy_barchart + labs(title = "K-Means vs IBCF (Accuracy)",
                                                subtitle = "Lower is better")
  
  png("accuracy.png")
  print(accuracy_barchart)     # accuracy_barchart 1 --> in the first page of PDF
  dev.off()  
  
  # ********* PRECISION AND RECALL *********************************************
  #K-MEANS
  #Add evaluation for K-Means
  precAndRec <- data.frame(userId=NA, precision=NA, recall=NA)[numeric(0), ]
  
  for(user in 1:length(usersId)){
    #Gets user id for each loop
    userTopNId <- usersId[user]
    
    #Get top N recommendation for each user in the test data set
    topN <- top10predictions %>%
      group_by(userId) %>%
      filter(userId == userTopNId) %>%
      arrange(desc(rating)) %>%
      slice(1:10)
    
    #Gets the movies that the user has actually rated
    userRatedMovies <- ratings_hitrate[(ratings_hitrate$userId == userTopNId),]
    
    #Gets precision@k
    precatk <- precisionAtK(TEN, userRatedMovies$movieId, topN$movieId)
    
    #Gets mean recall@k
    recatk <- recallAtK(TEN, userRatedMovies$movieId, topN$movieId)
    
    #Append user id and their respective precision and recall to a dataframe
    precAndRec[user, ] <- c(userTopNId, precatk, recatk)
  }
  
  # Precision@k of all the users
  kmeansPrecisionk <- precAndRec$precision
  
  # Show histogram of the distribution of precision@k
  kmeans_hist <- kmeansPrecisionk*TEN  
  png("kmeans_prec.png")
  pkkmeans_histogram <- hist(kmeans_hist, main="Distribution of Recommendation Relevance", labels = TRUE, xlim=c(0,10), xlab="# of relevant recommendations")
  dev.off()
  
  # Recall@k of all the users
  kmeansRecallk <- precAndRec$recall
  
  # Get the mean of the precision@k for all users
  kmeansMeanPrecisionk <- mean(kmeansPrecisionk)
  # Get the mean of the precision@k for all users
  kmeansMeanRecallk <- mean(kmeansRecallk)
  
  cat("Printing Measures for K-Means \n**********************************\n")
  # Print the mean of precision@k
  cat("The average of the precision@k for all users is: ", kmeansMeanPrecisionk, "\n")
  # Print the mean of recall@k
  cat("The average of the recall@k for all users is: ", kmeansMeanRecallk, "\n")
  
  #IBCF
  eval_df <- precision_recall(movie_ratings, training_part)
  
  # Precision@k of all the users
  precisionk <- eval_df$precision
  
  # Show histogram of the distribution of precision@k 
  ibcf_hist <- precisionk*TEN
  png("ibcf_prec.png")
  pkibcf_histogram <- hist(ibcf_hist, main="Distribution of Recommendation Relevance", labels = TRUE, xlim=c(0,10), xlab="# of relevant recommendations")
  dev.off()
  
  # Recall@k of all the users
  recallk <- eval_df$recall
  
  # Get the mean of the precision@k for all users
  meanPk <- mean(precisionk)
  # Get the mean of the precision@k for all users
  meanRk <- mean(recallk)
  
  cat("Printing Measures for IBCF \n**********************************\n")
  # Print the mean of precision@k
  cat("The average of the precision@k for all users is: ", meanPk, "\n")
  # Print the mean of recall@k
  cat("The average of the recall@k for all users is: ", meanRk, "\n")
  
  # Grouped bar chart for MAP@K and MAR@K
  MeanPatK <- c(format(round(kmeansMeanPrecisionk, 2), nsmall = 2), format(round(meanPk, 2), nsmall = 2)) 
  MeanRatK <- c(format(round(kmeansMeanRecallk, 2), nsmall = 2), format(round(meanRk, 2), nsmall = 2))
  
  data_precisionrecall <<- data.frame(models, PrecisinatK = MeanPatK, RecallatK = MeanRatK)
  
  data.m <-melt(data_precisionrecall, id="models")
  
  precisionandrecall_barchart <- ggplot(data.m, aes(variable, value)) + 
    geom_bar(aes(fill = models), width = 0.4, position = position_dodge(width=0.5), stat="identity") +
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.5)
  
  # Add titles
  precisionandrecall_barchart <- precisionandrecall_barchart + labs(title = "K-Means vs IBCF (Mean Precision and Recall at K)",
                                                                    subtitle = "Higher is  better for both Recall and Precision")
  
  png("precisionandrecall_barchart.png")
  print(precisionandrecall_barchart)
  dev.off()
  
  print("****Finished****")
}

# clears the console 
cat("\014")


library(pacman)
pacman::p_load(char=MYLIBRARIES, install = TRUE, character.only = TRUE)

manualTypes <- data.frame()

main()




