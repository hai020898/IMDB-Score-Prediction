attach(IMDB)
require(dplyr)
library(car)
library(ggplot2)
library(boot)
library(psych)
library(corrplot)
library(methods)
library(tidyverse)
require(lmtest)
require(plm)
library(splines)
library(caTools)
library(data.table)
library(stargazer)


#######Preprocessing Data############
#Add season into the database
IMDB = IMDB %>% mutate(Season =
                         case_when(releaseMonth %in% c('Mar','Apr','May')~'Spring',
                                   releaseMonth %in% c('Jun','Jul','Aug')~'Summer',
                                   releaseMonth %in% c('Sep','Oct','Nov')~'Fall',
                                   releaseMonth %in% c('Dec','Jan','Feb')~'Winter'))
IMDB$Season = as.factor(IMDB$Season)
attach(IMDB)

#Dummify maturity ratings

IMDB$maturityRating = as.factor(IMDB$maturityRating)
attach(IMDB)

#Dummify release month
IMDB$releaseMonth = as.factor(IMDB$releaseMonth)
attach(IMDB)

#Dummify language
IMDB$language = as.factor(IMDB$language)
attach(IMDB)

#Dummify Country
IMDB$country = as.factor(IMDB$country)
attach(IMDB)

#Dummify aspect Ratio
IMDB$aspectRatio = as.factor(IMDB$aspectRatio)
attach(IMDB)

 #Dummify distributor
IMDB$distributor = as.factor(IMDB$distributor)
attach(IMDB)


#Dummify Colour film
IMDB$colourFilm = as.factor(IMDB$colourFilm)
attach(IMDB)

#Dummify cinematographer
IMDB$cinematographer = as.factor(IMDB$cinematographer)
attach(IMDB)



#Dummify all the actors variables
IMDB$actor1 = as.factor(actor1)
IMDB$actor2 = as.factor(actor2)
IMDB$actor3 = as.factor(actor3)
attach(IMDB)

#Reduce the number of director variables observation
summary(director)
IMDB %>%
  group_by(director) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(-n)

IMDB = IMDB %>%
  dplyr::group_by(director)%>%
  dplyr::mutate(director = ifelse(n()<10, 'other', director))

#Dummify director
IMDB$director = as.factor(IMDB$director)
attach(IMDB)

IMDB %>%
  group_by(productionCompany) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(-n)

#Reduce the number of unique values for productionCompany
IMDB = IMDB %>%
  dplyr::group_by(productionCompany)%>%
  dplyr::mutate(productionCompany = ifelse(n()<96, 'other', productionCompany))

# #Dummify production company
IMDB$productionCompany = as.factor(IMDB$productionCompany)
attach(IMDB)

#Add English column
IMDB$English <- ifelse(IMDB$language == 'English', 1, 0)

##Eliminate all irrelevant columns

IMDB = IMDB%>%dplyr::select(-c("movieTitle", "movieID", "imdbLink", "genres", "plotKeywords", "aspectRatio"))
attach(IMDB)

#####################################################
#                Analysis                           #
#####################################################


## Testing each regression with each predictor
# good linear predictors p < 0.05 for individual models from for loop
# movieBudget, releaseYear, duration, nbNewsArticles, colourFilm, nbFaces, genre dummys (except musical, romance, animation), movieMeter_IMDBpro, season

large_cols <- c( "cinematographer",  "distributor")
for (col in 1:ncol(IMDB)) {
  if (colnames(IMDB)[col] %in% large_cols) {
    next
  }
  else if (is.factor(IMDB[,col])){
    next
  }
  else {
    model <- lm(imdbScore~unlist(IMDB[,col]), data = IMDB)
    # plot(IMDB[,col], imdbScore, xlab = colnames(IMDB)[col], ylab = "imdbScore")
    # abline(model)
    # names <- c(names, colnames(data)[col])
    # coef1 <- c(coef1, coef(model)[1])
    # coef2 <- c(coef2, coef(model)[2])
    print(colnames(IMDB)[col])
    print(summary(model))
  }
}

#Counting the frequency of each genre


nrow(subset(IMDB, action==1))
nrow(subset(IMDB, adventure==1))
nrow(subset(IMDB, scifi==1))
nrow(subset(IMDB, thriller==1))
nrow(subset(IMDB, musical==1))
nrow(subset(IMDB, romance==1))
nrow(subset(IMDB, western==1))
nrow(subset(IMDB, sport==1))
nrow(subset(IMDB, horror==1))
nrow(subset(IMDB, drama==1))
nrow(subset(IMDB, war==1))
nrow(subset(IMDB, animation==1))
nrow(subset(IMDB, crime==1))

####################################################Model Testing#################################################################

#####Model 1: Include every significant variables, minus western, sport, war who has fewer than 100 observations3#####

reg1 = lm(imdbScore~movieBudget+releaseYear+duration+nbNewsArticles+colourFilm+nbFaces+action+adventure+scifi+thriller+horror+drama+crime+movieMeter_IMDBpro+director+productionCompany+English)
summary(reg1)




outlierTest(reg1)

newIMDB = IMDB[-c(492,1806,316,395,1581,12,989),] #remove outlier
attach(newIMDB)


#####Model 2: Remove thriller and adventure, director, productionCompany due to being insignificant#####

reg2 = lm(imdbScore~movieBudget+releaseYear + duration+nbNewsArticles+colourFilm+nbFaces+action+horror+drama+crime+movieMeter_IMDBpro+English)
summary(reg2)

#Non-linear test for Model 2
residualPlots(reg2)

#Cross-validation for Model 2
fit2 = glm(imdbScore~movieBudget+releaseYear + duration+nbNewsArticles+colourFilm+nbFaces+action+horror+drama+crime+movieMeter_IMDBpro+English)
mse_2= cv.glm(newIMDB, fit2, K=10)$delta[1]  #0.7433256


####Add knot and run loop with polynomial to decrease non-linearity and find the smallest mse####

##2 knots

#Define knots
c1_2knots = quantile(duration,0.5)
d1_2knots = quantile(nbNewsArticles,0.5)
f1_2knots = quantile(movieMeter_IMDBpro,0.5)


combination_2knots = list()
mse_set_2knots = list()

#Run loop for poly
for (c in 2:5){
  for (d in 2:5){
    
    for (f in 2:5){
      multiplespline = glm(imdbScore~movieBudget+releaseYear+bs(duration,knots=c(c1_2knots), degree = c)+bs(nbNewsArticles, knots=c(d1_2knots), degree=d)+
                             nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_2knots), degree=f)+colourFilm+action+horror+drama+crime+English)
      mse_value = as.numeric(cv.glm(newIMDB,multiplespline, K=10)$delta[1])
      mse_set_2knots=append(mse_set_2knots, mse_value)
      combination_2knots=append(combination_2knots,list(list(c,d,f)))
    }
  }
}

#Find the combination of degree with smallest mse
combination_2knots[which.min(mse_set_2knots)]

mse_set_2knots[which.min(mse_set_2knots)] #0.634

#Model 3.1 with 2 knots

reg3_2knots = lm(imdbScore~movieBudget+releaseYear+bs(duration,knots=c(c1_2knots), degree = 3)+bs(nbNewsArticles, knots=c(d1_2knots), degree=4)+
                   nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_2knots), degree=3)+colourFilm+action+horror+drama+crime+English)
summary(reg3_2knots)


reg3_2knots_spline = glm(imdbScore~movieBudget+releaseYear+bs(duration,knots=c(c1_2knots), degree = 3)+bs(nbNewsArticles, knots=c(d1_2knots), degree=5)+
                           nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_2knots), degree=2)+colourFilm+action+horror+drama+crime+English)

mse3_2knots = cv.glm(newIMDB,reg3_2knots_spline, K=10)$delta[1]#0.6405

##3 knots

#Define knots
c1_3knots = quantile(duration,0.33)
c2_3knots = quantile(duration,0.66)
d1_3knots = quantile(nbNewsArticles,0.33)
d2_3knots = quantile(nbNewsArticles,0.66)
f1_3knots = quantile(movieMeter_IMDBpro,0.33)
f2_3knots = quantile(movieMeter_IMDBpro,0.66)

combination_3knots = list()
mse_set_3knots = list()

#Run loop for poly
for (c in 2:5){
  for (d in 2:5){
      for (f in 2:5){
        multiplespline = glm(imdbScore~movieBudget+releaseYear+
                               bs(duration,knots=c(c1_3knots,c2_3knots), degree = c)+
                               bs(nbNewsArticles, knots=c(d1_3knots,d2_3knots), degree=d)+
                               nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_3knots,f2_3knots), degree=f)+
                               colourFilm+action+horror+drama+crime+English)
        mse_value = as.numeric(cv.glm(newIMDB,multiplespline, K=10)$delta[1])
        mse_set_3knots=append(mse_set_3knots, mse_value)
        combination_3knots=append(combination_3knots,list(list(c,d,f)))
      }
    }
  }

#Find the combination of poly degree with the smallest mse
combination_3knots[which.min(mse_set_3knots)]
mse_set_3knots[which.min(mse_set_3knots)] #0.622

#Model 3.2 with 3 knots
reg3_3knots = lm(imdbScore~movieBudget+releaseYear+
                   bs(duration,knots=c(c1_3knots,c2_3knots), degree = 2)+
                   bs(nbNewsArticles, knots=c(d1_3knots,d2_3knots), degree=2)+
                  nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_3knots,f2_3knots), degree=5)+
                   colourFilm+action+horror+drama+crime+English)
summary(reg3_3knots)



reg3_spline_3knots = glm(imdbScore~movieBudget+releaseYear+
                           bs(duration,knots=c(c1_3knots,c2_3knots), degree = 2)+
                           bs(nbNewsArticles, knots=c(d1_3knots,d2_3knots), degree=4)+
                            nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_3knots,f2_3knots), degree=5)+
                           colourFilm+action+horror+drama+crime+English)
mse3_3knots = cv.glm(newIMDB,reg3_spline_3knots, K=10)$delta[1] #0.622



##4 knots

#Define knots
c1_4knots = quantile(duration,0.25)
c2_4knots = quantile(duration,0.5)
c3_4knots = quantile(duration,0.75)
d1_4knots = quantile(nbNewsArticles,0.25)
d2_4knots = quantile(nbNewsArticles,0.5)
d3_4knots = quantile(nbNewsArticles,0.75)
f1_4knots = quantile(movieMeter_IMDBpro,0.25)
f2_4knots = quantile(movieMeter_IMDBpro,0.5)
f3_4knots = quantile(movieMeter_IMDBpro,0.75)



combination_4knots = list()
mse_set_4knots = list()

#Run loop for poly
for (c in 2:5){
  for (d in 2:5){
      for (f in 2:5){
        multiplespline = glm(imdbScore~movieBudget+releaseYear+
                               bs(duration,knots=c(c1_4knots,c2_4knots,c3_4knots), degree = c)+
                               bs(nbNewsArticles, knots=c(d1_4knots,d2_4knots,d3_4knots), degree=d)+
                               nbFaces+bs(movieMeter_IMDBpro, knots=c(f1_4knots,f2_4knots,f3_4knots), degree=f)+
                               colourFilm+action+horror+drama+crime+English)
        mse_value = as.numeric(cv.glm(newIMDB,multiplespline, K=10)$delta[1])
        mse_set_4knots=append(mse_set_4knots, mse_value)
        combination_4knots=append(combination_4knots,list(list(c,d,f)))
      }
    }
  }

#Find the combination of poly degree with the smallest mse 
combination_4knots[which.min(mse_set_4knots)]

mse_set_4knots[which.min(mse_set_4knots)]#0.617

#Model 3.3 with 4 knots
reg3_4knots = lm(imdbScore~movieBudget+releaseYear+
                   bs(duration,knots=c(c1_4knots,c2_4knots,c3_4knots), degree = 4)+
                   bs(nbNewsArticles, knots=c(d1_4knots,d2_4knots,d3_4knots), degree=2)+
                   nbFaces+
                   bs(movieMeter_IMDBpro, knots=c(f1_4knots,f2_4knots,f3_4knots), degree=2)+
                   colourFilm+action+horror+drama+crime+English)
summary(reg3_4knots)

reg3_spline_4knots = glm(imdbScore~movieBudget+releaseYear+
                   bs(duration,knots=c(c1_4knots,c2_4knots,c3_4knots), degree = 4)+
                   bs(nbNewsArticles, knots=c(d1_4knots,d2_4knots,d3_4knots), degree=2)+
                   nbFaces+
                   bs(movieMeter_IMDBpro, knots=c(f1_4knots,f2_4knots,f3_4knots), degree=2)+
                   colourFilm+action+horror+drama+crime+English)



mse3_4knots = cv.glm(newIMDB,reg3_spline_4knots, K=10)$delta[1] 


####We will go with the 4 knots Model 3.3 since it has the lowest mse

#Heteroskedasticity test
residualPlot(reg3_4knots, quadratic=FALSE)
ncvTest(reg3_4knots)

##Correcting heteroskedasticity

coeftest(reg3_4knots, vcov=vcovHC(reg3_4knots, type='HC1'))

######Test data preprocessing

test_data = test_data %>% mutate(Season = 'Fall')
test_data$English <- ifelse(test_data$language == 'English', 1, 0)

##Final prediction
prediction = predict(reg3_4knots, newdata = test_data)







