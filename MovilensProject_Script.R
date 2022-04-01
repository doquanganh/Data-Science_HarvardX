##Installing Packages
# List of packages for session
.packages = c("tidyverse",       #tidy alvvays and forever!
              "corrplot",        #correlation plots
              "cowplot",         #solve x-axis misalignment when plotting, and better-looking defaults for ggplots
              "gridExtra",       #combine plots
              "knitr",           #report output
              "kableExtra",      #nice tables
              "lubridate",       #date math!
              "reshape2",        #acast to create matrix
              "scales",          #get rid of scientific notation in charts
              "splitstackshape",  #explode pipe-delimited data to one-hot encoded dummy variables
              "dplyr",
              "tm",
              "tmap",
              "wordcloud",
              "knitr",
              "tinytex"
              
)
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Customize knitr output
#Set Thousands Separator for inline output
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ prettyNum(round(x,2), big.mark=",") } })
#we've already set the graphic device to "png" in the RMD options. the default device for pdfs draws every point of a scatterplot, creatinvg *very* big files.
#But png is not as crisp, so we will set a higher resolution for pdf output of plots. 
knitr::opts_chunk$set(dpi=150)
#Create Kable wrapper function for thousands separator in table output, and nice formating with kableExtra
niceKable = function(...) {
  knitr::kable(..., format.args = list(decimal.mark = '.', big.mark = ",")) %>% kable_styling()
}
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


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

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
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
saveRDS(edx, file = "edx.rds")
saveRDS(validation, file = "validation.rds")

#Load the Data
edx <- readRDS("edx.rds", refhook = NULL)
validation <- readRDS("validation.rds", refhook = NULL)


#Check Dimensions of both test and train set
dim(edx)
dim(validation)

tribble(
  ~"Dataset",     ~"Number of Rows",    ~"Number of Columns",
  #--             |--                   |----
  "edx",          nrow(edx),            ncol(edx),
  "validation",   nrow(validation),     ncol(validation)
)%>%niceKable

sapply(edx, {function(x) any(is.na(x))})%>% niceKable

### Genres

genre_count <- edx %>% separate_rows(genres, sep="\\|") %>% group_by(genres) %>% summarise(number = n()) %>% arrange(desc(number))

edx %>%
  distinct(genres) %>%
  mutate(genreCount = str_count(genres,'\\|')) %>%
  arrange(desc(genreCount)) %>%
  top_n(2) %>%
  niceKable

### Best-Rated Genres

top10kgenres <- edx %>%
  group_by(genres) %>%
  summarize(n=n(), sd=sd(rating), se = sd/sqrt(n), avg = mean(rating)) %>%
  filter(n>10000) %>%
  top_n(10,avg) %>%
  mutate(genres=reorder(genres,avg))

top10Kplot <- top10kgenres %>% ggplot(aes(x=genres, y=avg))+
  geom_point()+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se, width=0.4, colour="red",alpha=0.4, size=1.3))+
  theme(axis.text.x = element_text(angle=60, hjust = 1))+
  ggtitle("Top Genres > 10,000 Ratings")+
  ylim(3.5,4.3)
top100Kgenres <- edx %>%
  group_by(genres) %>%
  summarize(  n = n(),sd = sd(rating) ,se  = sd/sqrt(n) ,avg = mean(rating)) %>%
  filter(n > 100000) %>%
  top_n(10, avg) %>%
	mutate(genres = reorder(genres, avg))

top100Kplot <- top100Kgenres %>% ggplot (aes(x=genres, y=avg)) +
    geom_point() +
    geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=0.4, colour="red", alpha=0.4, size=1.3) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    ggtitle("Top Genres > 100,000 Ratings") +
    ylim(3.4, 4.3)  


#align x-axes of both plots
topplots <- align_plots(top10Kplot, top100Kplot, align = "hv")
grid.arrange(topplots[[1]],topplots[[2]], nrow=1)


### Genre prevalence

genre_count %>% 
  ggplot(aes(reorder(genres, number), number)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

### Reviews 
#Which films have the most popular(most rating)?
edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  top_n(20, count) %>%
  ggplot(aes(count, reorder(title, count))) +
  geom_bar(color = "black", fill = "deepskyblue2", stat = "identity") +
  geom_text(aes(label=count), vjust="top", color="black", size=3) + #add counts to bars themselves...
  xlab("Count") +
  ylab(NULL) +
  theme_bw()

### Rating Frequency

edx %>%
  group_by(rating) %>%
  summarize(count = n(),probability = count/nrow(edx)) %>%
  ggplot(aes(rating, count))+
  geom_bar(stat="identity", fill ="deepskyblue2")+
  geom_text(aes(label=count), vjust="top", color="white", size=3)+
  theme_minimal() +
  theme(axis.title.y=element_blank(),                                 #.,. and hide y-axis labels
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Part of the reason for fewer half-star ratings is likely that half-stars weren't used in the *Movielens* rating system prior to 2003.  Here are the distributions of the ratings before and after half stars were introduced.

before <- edx %>%
  group_by(rating) %>%
  mutate(reviewdate = year(floor_date(as_datetime(timestamp),"day")) ) %>%
  filter(reviewdate < 2003) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(rating, count)) +
  ggtitle("Rating from before 2003") +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
after <- edx %>%
  group_by(rating) %>%
  mutate(reviewdate = year(floor_date(as_datetime(timestamp),"day")) ) %>%
  filter(reviewdate >= 2003) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(rating, count)) +
  ggtitle("Rating from 2003 and late") +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
grid.arrange(before,after, nrow = 1)


overall_mean <- mean(edx$rating)

### Average Rating By Year of Release 

index <- sample(1:nrow(edx), 200000)
age <- edx[index, ]


age <- edx %>% 
  mutate(
         temp = str_extract(title, regex(   "\\((\\d{4})\\)"   )),   #extract the year of release in brackets
         release_yr = str_extract(temp, regex(   "(\\d{4})"   )),     #remove the brackets and...
         release_yr = as.numeric(release_yr)                          #...convert to a number
          ) %>%
  select(-everything(), rating, release_yr)

age %>%
  group_by(release_yr) %>%
  summarize(  n = n(), 				sd = sd(rating) ,		se  = sd/sqrt(n) , 		avg = mean(rating) 				) %>%

  
    ggplot (aes(x=release_yr, y=avg)) +
    geom_point() +
    geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=0.4, colour="red", alpha=0.8, size=1.3) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    geom_hline(yintercept = overall_mean)
  
 

age %>%
  group_by(release_yr) %>%
  summarize(  ratings = n()	) %>%
  ggplot ( aes(release_yr, ratings) ) +
  geom_bar(stat = "identity", fill="steelblue") +
  scale_y_continuous(labels = comma) +
  theme_minimal()


edx %>%
summarize(
  First_Quart = quantile(rating,0.25), 
  Mean_Rating = mean(rating) , 
  Median_Rating = median(rating), 
  Third_Quart = quantile(rating,0.75)
  )  %>% niceKable

# Results
## Naive Prediction

naive_rmse <- RMSE(validation$rating, overall_mean)

rmse_results <- tibble(Method = "Mean Rating (Naive)", RMSE = naive_rmse) 
rmse_results %>% niceKable

## Feature Engineering
#Split genres into a dummy variable and one-hot encode them

edxClean <- edx
edxClean <- edxClean %>%
  mutate(
    userId = factor(userId),
    movieId = factor(movieId),
    
    reviewdate = floor_date(as_datetime(timestamp),"day"), #Convert timestamp to a useful date,
    temp = str_extract(title, regex("\\((\\d{4})\\)")), #extract the year of release in brackets
    release_yr = str_extract(temp, regex(   "(\\d{4})"   )),#remove the brackets and...
    release_yr = as.numeric(release_yr),
    review_dly =                                                  #and time between release and review
      as.numeric(year(reviewdate)) - as.numeric(release_yr),  
    
    
  )%>%                    #now remove the useless columns
  select(-timestamp, 
         -temp, 
         -reviewdate, 
         -title,
         -genres
         
  )
#and check our work          
head(edxClean) %>% kable %>% kable_styling()

#...and do the same for the validation set:

validationClean <- validation
validationClean <- validationClean %>% 
  mutate(
    
    #since train and test weren't factored before splitting, we have to use the same levesl from the 
    #training set to define the test set. (Since the test set doesn't have all movies and userIds)
    
    userId  = factor(userId, levels = levels(edxClean$userId)),   #Factor user ID using levels in train, 
    movieId = factor(movieId, levels = levels(edxClean$movieId)), #...   movie ID,
    
    reviewdate = floor_date(as_datetime(timestamp), "day"),       #Convert timestamp to a useful date,     
    temp = str_extract(title, regex(   "\\((\\d{4})\\)"   )),     #extract the year of release in brackets
    release_yr = str_extract(temp, regex(   "(\\d{4})"   )),      #remove the brackets and...
    release_yr = as.numeric(release_yr),                      #...convert to a number
    
    review_dly =                                          #and time between release and review
      as.numeric(year(reviewdate)) - as.numeric(release_yr),    
  ) %>%
  select(-timestamp, 
         -temp, 
         -reviewdate, 
         -title,
         -genres
  )
#and check our work         
head(validationClean) %>% kable %>%kable_styling()

#Check work
names(edxClean)
names(validationClean)


#Movie Effect - determine bias for each movie (mean rating of a movie compared to overall mean) 
movie_avgs <- edxClean %>% 
  group_by(movieId) %>% 
  select(-everything(), movieId, rating) %>%
  summarise(moviemean =    mean( as.numeric(rating) ),
            moviebias =    moviemean - overall_mean 
  ) %>%
  mutate(movieId = factor(movieId)) %>%
  select(-moviemean)

#Add moviebias column to edx
edxClean_bias <- edxClean  %>% left_join(movie_avgs, by='movieId')


#User Effect - determine bias for each user (mean rating of a user compared to overall mean) 
user_avgs <- edxClean %>% 
  group_by(userId) %>% 
  select(-everything(), userId, rating) %>%
  summarise(usermean =    mean( as.numeric(rating) ),
            userbias =    usermean - overall_mean 
  ) %>%
  mutate(userId = factor(userId)) %>%
  select(-usermean)


#Add userbias column to edx
edxClean_bias <- edxClean_bias  %>% left_join(user_avgs, by='userId')

edxClean_bias <- edxClean_bias %>% 
  mutate(usermoviebias = overall_mean + moviebias + userbias)



#Join moviebias column to validation
validationClean_bias <- validationClean  %>% left_join(movie_avgs, by='movieId')

#Join userbias column to validation
validationClean_bias <- validationClean_bias  %>% left_join(user_avgs, by='userId')

validationClean_bias <- validationClean_bias %>% 
  mutate(usermoviebias = overall_mean + moviebias + userbias)


#Predict ratings based on movie effect
predicted_ratings <- overall_mean + validationClean %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$moviebias

#calculate RMSE for movie effects model

pred_me <-RMSE(validationClean$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie Effect Model",
                                 RMSE = pred_me ))
rmse_results %>% niceKable()


## Predict ratings based on user + movie effect

head(edxClean_bias) %>% kable %>% kable_styling()

#Predict ratings based on user + movie effect
predicted_ratings_both <- validationClean %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(usermoviebias = overall_mean + moviebias + userbias) %>%
     .$usermoviebias


#calculate RMSE for movie effects model
pred_usermovie <-RMSE(validationClean$rating, predicted_ratings_both)
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="User + Movie Effect Model",
                                 RMSE = pred_usermovie ))
rmse_results %>% niceKable()

### Correlation Plot


corr <- edxClean_bias %>% select(-movieId, -userId, -release_yr)
index <- sample(1:nrow(corr), 100000)
corr <- corr[index, ]

corrplot(cor(corr), method = "square", type="upper")

## Regularization
penalties <- seq(0, 60, 2)
m_rmses <- sapply(penalties, function(p){
  
  reg_movie_avgs <- edxClean %>% 
    group_by(movieId) %>%
    summarize(regmoviebias = sum(rating - overall_mean)/(n()+p))
  
  
  predicted_ratings <- 
    edxClean %>% 
    left_join(reg_movie_avgs, by = "movieId") %>%
    left_join(user_avgs, by = "userId") %>%
    mutate(regmoviebias = overall_mean + userbias + regmoviebias) %>%
    .$regmoviebias
  return(RMSE(predicted_ratings, edxClean$rating))
})



qplot(penalties, m_rmses)    #plot the penalties

moviepenalty_optimal <- penalties[which.min(m_rmses)]  #determine which is lowest

reg_movie_avgs <- edxClean %>% 
  group_by(movieId) %>%
  summarize(regmoviebias = sum(rating - overall_mean)/(n()+moviepenalty_optimal))


predicted_ratings <- 
  validationClean %>% 
  left_join(reg_movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(regmoviebias = overall_mean + userbias + regmoviebias) %>%
  .$regmoviebias


regularized_movieeffects <- RMSE(predicted_ratings, validationClean$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(Method="User + Regularized Movie Effects",  
                                 RMSE = regularized_movieeffects ))
rmse_results %>% niceKable



#A little bit! Now let's do the same for user penalties.

penalties <- seq(0, 5, 0.25)
u_rmses <- sapply(penalties, function(p){
     reg_user_avgs <- edxClean %>% 
          left_join(reg_movie_avgs, by="movieId") %>%
          group_by(userId) %>%
          summarize(reguserbias = sum(rating - regmoviebias - overall_mean)/(n()+p))
     
     predicted_ratings <- 
          edxClean %>% 
          left_join(reg_movie_avgs, by = "movieId") %>%
          left_join(reg_user_avgs, by = "userId") %>%
          mutate(regusermoviebias = overall_mean + regmoviebias + reguserbias) %>%
          .$regusermoviebias
     return(RMSE(predicted_ratings, edxClean$rating))
})


     
qplot(penalties, u_rmses)    #plot the penalties

userpenalty_optimal <- penalties[which.min(u_rmses)]  #determine which is lowest


#build table with optimal penalty    
reg_user_avgs <- edxClean %>% 
  left_join(reg_movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(reguserbias = sum(rating - regmoviebias - overall_mean)/(n()+userpenalty_optimal))

reg_predicted_ratings <- 
  validationClean %>% 
  left_join(reg_movie_avgs, by = "movieId") %>%
  left_join(reg_user_avgs, by = "userId") %>%
  mutate(regusermovie = overall_mean + regmoviebias + reguserbias) %>%
  .$regusermovie

regularized_effects <- RMSE(reg_predicted_ratings, validationClean$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Regularized User + Regularized Movie Effects",  
                                 RMSE = regularized_effects ))
rmse_results %>% niceKable


plot_reg <- 
  edxClean %>% 
  group_by(movieId) %>% 
  summarize(moviebias = sum(rating - overall_mean)/(n()+moviepenalty_optimal), n = n())        

tibble(original = movie_avgs$moviebias, 
       regularized = plot_reg$moviebias, 
       n = plot_reg$n) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.2) +
  theme_minimal()

### Final Improvements 

tibble("< 0.5" = sum(reg_predicted_ratings < 0.5), " > 5" = sum(reg_predicted_ratings > 5) ) %>% niceKable


#These are outside of the valid range empirically present in the dataset. There aren't many, but could we get better by limiting these extreme values to the nearest valid rating?

reg_predicted_ratings_limit <- pmax(reg_predicted_ratings,     0.5)  #values lower than 0.5 to 0.5
reg_predicted_ratings_limit <- pmin(reg_predicted_ratings_limit, 5)  #values greater than 5 to 5

regularized_effects_limits <- RMSE(reg_predicted_ratings_limit, validationClean$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Reg. User + Reg. Movie Effects (Capped)",  
                                     RMSE = regularized_effects_limits ))
rmse_results %>% niceKable



#Let's see how our final model performs against the true values present in the data.

true_vs_predicted <- tibble(
  true = validationClean$rating, 
  pred = reg_predicted_ratings_limit)

true_vs_predicted %>%
  ggplot( aes( true, pred))  +
  geom_point(alpha = 0.005) +
  geom_smooth()+
  geom_hline(yintercept = overall_mean, colour = "orange") +
  geom_vline(xintercept = overall_mean, colour = "orange")


# Conclusion

rmse_results <- tibble(Method = "Final RMSE", RMSE = regularized_effects_limits) 
rmse_results %>% niceKable

## Future Considerations
##The model may be improved in the future by adjusting or changing the modeling approach altogether. Below are some of these opportunities that were not included in the final model. Some opportunities are mutually exclusive, as they would rely on entirely different methodologies.

### Matrix Factorization
##We could consider using matrix factorization to build a model. 
##We would create a matrix with movies in rows and users in columns (or vice versa), with the intersection being a given users's rating for a given film. Here is an example with the most commonly-rated films, with ratings given by the first 7 users.


topMovies <- edx %>% group_by(movieId) %>% summarise(n = n()) %>% arrange(desc(n)) %>% top_n(11) 

topMoviesRatings <- edx %>% filter(movieId %in% topMovies$movieId) %>% group_by(userId) %>% summarise(n = n()) %>%   slice(1:6)  #top_n(11) before the slice would give us the most prolific raters

matrix <- edx %>% filter(movieId %in% topMovies$movieId, userId %in% topMoviesRatings$userId)  %>% select(-timestamp, -genres)

acast(matrix, title~userId, value.var="rating") %>% niceKable


