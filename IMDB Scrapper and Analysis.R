# Step 1 : Install and load packages
#install.packages('dplyr')
#install.packages('rvest')
#install.packages('stringr')
#install.packages('ggplot2')

library(ggplot2)
library(rvest)
library(dplyr)
library(stringr)

## Note: Visualization skills are spread throughout the code in preparing the... 
##...data for power BI

#### Data Skill 1 - Data Collection ####

#Wrap this scraping protocol in a function that takes a url

page.scrape <- function(url){
  
  
  
  target.url <- url
  IMDB <- read_html(target.url)
  
  #Scrape the titles on the page
  
  movie.title <- IMDB %>%
    html_nodes('.lister-item-header a') %>%
    html_text()
  
  movie.title
  
  #Scrape the accompanying release year
  
  release.year <- IMDB %>%
    html_nodes('.lister-item-year.text-muted.unbold') %>%
    html_text()
  
  #Cleaning for the release year
  
  release.year <- str_extract_all(release.year, "[[:digit:]]+")
  
  release.year <- as.numeric(release.year)
  
  release.year
  
  #Scrape the genre tags on the page
  
  genre <- IMDB %>%
    html_nodes('.genre') %>%
    html_text()
  
  genre
  
  #Cleaning for genre tags
  
  genre <- str_replace_all(genre, "\n", "")
  genre <- str_replace_all(genre, " ", "")
  
  genre
  
  #Scrape the runtime of the movie
  
  runtime.minutes <- IMDB %>%
    html_nodes('.runtime') %>%
    html_text()
  
  runtime.minutes
  
  #Cleaning for runtime values
  
  runtime.minutes <- str_extract_all(runtime.minutes, "[[:digit:]]+")
  runtime.minutes <- as.numeric(runtime.minutes)
  runtime.minutes
  
  #Scrape the age rating
  
  rating <- IMDB %>%
    html_nodes('.certificate') %>%
    html_text()
  
  rating
  
  #Scrape the imdb score
  
  IMDB.score <- IMDB %>%
    html_nodes('.ipl-rating-star.small .ipl-rating-star__rating') %>%
    html_text()
  IMDB.score <- as.numeric(IMDB.score)
  
  IMDB.score
  
  #Scrape the number of votes from users
  
  votes <- IMDB %>%
    html_nodes('.text-muted.text-small') %>%
    html_text()
  
  votes
  
  #Cleaning for gross profit numbers
  
  votes <- votes[str_detect(votes, "Votes:")]
  
  votes <- str_extract_all(votes, "(?<=Votes:\n).*(?=\n)")
  
  votes <- str_replace_all(votes, " ", "")
  
  votes <- str_replace_all(votes, ",", "")
  
  votes <- as.numeric(votes)
  
  #Scrape the gross of each movie
  
  gross.profit <- IMDB %>%
    html_nodes('.text-muted.text-small') %>%
    html_text()
  
  gross.profit
  
  #Cleaning for gross profit numbers
  
  gross.profit <- gross.profit[str_detect(gross.profit, "Votes:")]
  
  gross.profit <- str_extract_all(gross.profit, "(?<=Gross:\n).*(?=\n)")
  
  gross.profit <- str_replace_all(gross.profit, " ", "")
  
  gross.profit[gross.profit == "character(0)"] <- NA
  
  gross.profit <- str_replace_all(gross.profit, "[$]", "")
  
  gross.profit <- str_replace_all(gross.profit, "M", "")
  
  gross.profit <- as.numeric(gross.profit)
  
  gross.profit
  
  
  #Create data frame that holds all scrapped elements
  
  page.scrape <- data.frame(movie.title, release.year, genre, runtime.minutes, IMDB.score, votes, gross.profit)
  
  return(page.scrape)
  
}

#Loop to scrape all 100 pages of the list

output.data <- list()

progress <- txtProgressBar(min = 0, max = 9)

pages <- c(1,2,3,4,5,6,7,9,10)

for(i in 1:9){
  output.data[[i]] <- page.scrape(url = paste0("https://www.imdb.com/list/ls006266261/?st_dt=&mode=detail&page=",pages[i],"&sort=user_rating,desc"))
  
  
  setTxtProgressBar(progress, i)
}

output.data

#Combines data scrapes from pages into a single data frame

output <- bind_rows(output.data)

output <- na.omit(output)


#Complete scraped movie data

output

nrow(output)

View(output)

#Generating dummy variable from genre tags

#Identify unique genre tags
genre.dum <- unique(unlist(strsplit(as.character(output$genre), ",")))

genre.dum

#Use unique tags to set up a data frama
dummy.frame <- data.frame(matrix(ncol = length(genre.dum), nrow = nrow(output)))
colnames(dummy.frame) <- genre.dum

#Use a loop to detect each genre string in output genre column, produces true and false vector
dummy.frame

for(i in 1:length(genre.dum)){
  genre.valid <- str_detect(output$genre, genre.dum[i])
  
  dummy.frame[,genre.dum[i]] <- genre.valid
  
}

dummy.frame

#Combine dummy variables with scrape data

master.output <- cbind(output, dummy.frame)

#convert gross profit to integer for ease of visualizations
master.output$gross.profit <- (master.output$gross.profit)*1000000

View(master.output)



#### Data Skill 2 - Data Analysis ####


                    #### QUESTION 1 ####

# Split character strings in genre column in master.output df into separate columns

out<-list(str_split_fixed(master.output$genre, ",", 3))
do.call(rbind, out)
master.output<-cbind(master.output, do.call(rbind, out))

# Rename new columns (Primary.Genre, Genre.2, Genre.3)
master.output<-
  master.output%>%rename(Primary.Genre=`1`, Genre.2=`2`,  Genre.3=`3`)
# Use Split() to split up the movies in master.output data frame by release year
# Creates list of data frames ordered by year,
# which each contain movies released that year
MoviesEachYear<-split(master.output, f = master.output$release.year)
MoviesEachYear[[1]]


#### Q1.1: "HOW HAS THE AVERAGE RUN-TIME OF MOVIES CHANGED OVER THE YEARS? ####
# Use Sapply() to apply mean function on all of the runtime of movies in a given year

MovieRuntime<-sapply(MoviesEachYear, function(x) mean(x$runtime.minutes))

# Apply which.max() for year with the highest run-time in minutes
# and then max() for the exact run-time in minutes 
which.max(MovieRuntime)##Movies released in 1962 the longest run-times
max(MovieRuntime)  ##Mean run-time in 1962 being 228 minutes or 3.8 hours

# same for minimum
which.min(MovieRuntime)##Movies released in 1940 the longest run-times
min(MovieRuntime)  ##Mean run-time in 1940 being 90 minutes or 1.5 hours

#### Q1.2: "HOW HAS PROFITABILITY CHANGED OVER THE YEARS?" (same method as above)####

MovieProfitability<-sapply(MoviesEachYear, function(x) mean(x$gross.profit))
#Apply which.max() for year with the highest gross profit
#and then max() for the exact gross profit amount
which.max(MovieProfitability) ##2018 was the highest grossing year 
max(MovieProfitability)   ##Grossing $226.4467 Million  

# same for lowest gross profits
which.min(MovieProfitability) ##1950 was the highest grossing year 
min(MovieProfitability)   ##Grossing $10,000  

#### Q1.3: "WHICH YEARS HAVE PRODUCED THE MOST TOP MOVIES?" ####
AnnualMovieCount<-sapply(MoviesEachYear, nrow)

AnnualMovieCount[which.max(AnnualMovieCount)] ## 37 movies produced in 2011

AnnualMovieCount[which.min(AnnualMovieCount)] ## 1 movie produced in 1933

##  convert gross profit to $ for visualizations (do this at the end so it does not impact analysis)

power.bi.output <- master.output
power.bi.output$gross.profit <-  paste('$',formatC(power.bi.output$gross.profit,
                                                 big.mark=',', format = 'f'))
##Export finalize data frame as csv for powerBI ##
write.csv(power.bi.output, "C:/Users/snlas/Downloads/master.output.csv", row.names = TRUE)


                    ####   QUESTION 2 ####
#### Q2.1: "HOW DOES ESTIMATED GROSS PROFIT VARY WITH THE INCLUSION OF DIFFERENT GENRES? ####

#Find regression output and find the coefficients 
fit.profitgenre<- lm(gross.profit ~ Drama + Crime + Action 
                     + Adventure + Biography + History + `Sci-Fi` 
                     + Romance + Western + Fantasy + Thriller 
                     + War + Animation + Family + Comedy 
                     + Music + Horror + Documentary
                     + Sport + Musical +`Film-Noir`+ News,
                     data = master.output)
summary(fit.profitgenre)


# Drama movies are significant to Profit (p-value=0.00224 **)
# Crime movies are significant to Profit (p-value=0.00481 **)
# Action movies are significant to Profit (p-value=0.00178 **)
# Adventure movies are significant to Profit (p-value=3.49e-13 ***) -> has most impact
# Comedy movies are significant to Profit (p-value=0.02934**)
# Horror movies are significant to Profit (p-value=0.00707 ** )
# Documentary movies are significant to Profit (p-value= 0.00146**)

# create a data frame of genre/coefficient pairs for visualizations and export as CSV
Genre.Coefficients <- summary(fit.profitgenre)$coefficients[,1]
Genres <- sort(genre.dum, decreasing = FALSE)
Genre.Coefficient.Pairs <- data.frame(coefficient = Genre.Coefficients, Genre = Genres)
Genre.Coefficient.Pairs$coefficient <-  paste('$',formatC(Genre.Coefficient.Pairs$coefficient,
                                                          big.mark=',', format = 'f'))
write.csv(Genre.Coefficient.Pairs, "C:/Users/snlas/Downloads/Genre.Coefficient.Pairs.csv", row.names = TRUE)

#### Q2.2: "HOW DOES ESTIMATED IMDB SCORE VARY WITH THE INCLUSION OF DIFFERENT GENRES?"####
## Repeat for rating based on genre ##
fit.ratinggenre<- lm(IMDB.score ~ Drama + Crime + Action 
                     + Adventure + Biography + History + `Sci-Fi` 
                     + Romance + Western + Fantasy + Thriller 
                     + War + Animation + Family + Comedy 
                     + Music + Horror + Documentary
                     + Sport + Musical +`Film-Noir`+ News,
                     data = master.output)
summary(fit.ratinggenre)
# Action films are significant to IMDB Scores (p-value=0.00590 **)
# Westerns are significant to IMDB Scores (p-value=0.00097 ***)
# Thrillers are significant to IMDB Scores (p-value=0.00097 ***)
# Animation films are significant to IMDB Scores (p-value=0.00259 **)
# Comedy films are significant to IMDB Scores (p-value=6.62e-10 ***)
# Horror films are significant to IMDB Scores (p-value=4.74e-08 ***)

## Create rating/genre estimate dataframe for ease of visuals ##
Genre.Rating.Coefficients <- summary(fit.ratinggenre)$coefficients[,1]
Genre.Rating.Pairs <- data.frame(coefficient = Genre.Rating.Coefficients, Genre = Genres)
write.csv(Genre.Rating.Pairs, "C:/Users/snlas/Downloads/Genre.Rating.Pairs.csv", row.names = TRUE)


                    #### QUESTION 3 ####
####  Q3: "CORRELATION OF RATINGS AND GROSS PROFIT"
#use Cor() to find correlation
cor(master.output$IMDB.score, master.output$gross.profit)
qplot(IMDB.score, gross.profit,
      data = master.output,
      xlab='IMDB Score', ylab ='Gross Profit',
      geom = c('point','smooth'),
      alpha = I(1 / 5))  
#Correlation between IMDB score and Gross Profit is 0.1686375 
#Weak Correlation between IMDB and Gross Profit

