library(tidyverse)
library(car)
library(readxl)
library(stats)
library(sentimentr)
library(Hmisc)
library(jtools)
library(qdap)
library(syuzhet)
??get_sentiment
###Cleant Training Data
intdat <- read_excel('Train_data.xlsx')

intdat$Purpose_Of_Loan <- as.factor(intdat$Purpose_Of_Loan)

#Create some new variables


intdat$Description_Lenght <- lengths(gregexpr("\\W+", intdat$Loan_Description)) + 1
intdat$Title_Lenght <- lengths(gregexpr("\\W+", intdat$Loan_Title)) + 1
intdat$`Delinquency?` <- ifelse(is.na(intdat$Months_Since_Deliquency) == F, 1, 0)
intdat$Employed <- ifelse(is.na(intdat$Length_Employed) == F, 1, 0)
intdat$Derogated <- ifelse(is.na(intdat$Months_Since_Last_Derogatory) == F, 1, 0)
intdat$`Decription?` <- ifelse(is.na(intdat$Loan_Description) == F, 1, 0)

###Removing Outliers and adding more variables
incout <- boxplot(intdat$Annual_Income, plot = F)$out
intdat[which(intdat$Annual_Income %in% incout),]
intdat.no.outlier <- intdat[-which(intdat$Annual_Income %in% incout),]
revout <- boxplot(intdat.no.outlier$Revolving_Balance, plot = F)$out
intdat.no.outlier <- intdat.no.outlier[-which(intdat.no.outlier$Revolving_Balance %in% revout),]
intdat.no.outlier$Record <- ifelse(is.na(intdat.no.outlier$Months_Since_Record) == F, 1, 0)
intdat.no.outlier$`Description?` <- ifelse(is.na(intdat.no.outlier$Loan_Description) == T, 1, 0)
intdat.no.outlier$Title_Lenght <- lengths(gregexpr("\\W+", intdat.no.outlier$Loan_Title)) + 1
####KNNN imputation
library(DMwR)
numintdat <- subset(intdat.no.outlier, select = c(Loan_Amount_Requested, Interest_Rate, Length_Employed,
                                                  Annual_Income, Debt_To_Income, Number_Delinqueny_2yrs, 
                                                  Inquiries_Last_6Mo, Months_Since_Deliquency,
                                                  Months_Since_Record, Number_Open_Accounts, Public_Record_Count,
                                                  Revolving_Balance, Revolving_Utilization, Total_Accounts,
                                                  Collections_12Mo_Exclude_Med, Months_Since_Last_Derogatory,
                                                  Description_Lenght))
numintdat <- as.data.frame(numintdat)
knnOutput <- knnImputation(numintdat)
hist(log(numintdat$Annual_Income))
mean(numintdat$Annual_Income, na.rm = T)
intdat.no.outlier$Annual_Income <- knnOutput$Annual_Income

####Clean TEST DATA
test <- read_excel("Test_data.xlsx")
test$`Delinquency?` <- ifelse(is.na(test$Months_Since_Deliquency) == F, 1, 0)
test$Employed <- ifelse(is.na(test$Length_Employed) == F, 1, 0)
test$Derogated <- ifelse(is.na(test$Months_Since_Last_Derogatory) == F, 1, 0)
test$Description_Lenght <- lengths(gregexpr("\\W+", test$Loan_Description)) + 1
test$Title_Lenght <- lengths(gregexpr("\\W+", test$Loan_Title)) + 1
test$Record <- ifelse(is.na(test$Months_Since_Record) == F, 1, 0)
test$`Description?` <- ifelse(is.na(test$Loan_Description) == T, 1, 0)
test$Annual_Income[is.na(test$Annual_Income)] <- mean(test$Annual_Income, na.rm = T)


###Sentiment Analysis
intdat.no.outlier$Loan_Sentiment <- get_sentiment(intdat.no.outlier$Loan_Description, method = 'afinn')
intdat.no.outlier$title_sentiment <- get_sentiment(intdat.no.outlier$Loan_Title, method = 'afinn')
test$Loan_Sentiment <- get_sentiment(test$Loan_Description, method = 'afinn')
test$title_sentiment <- get_sentiment(test$Loan_Title, method = 'afinn')


####Regression####

lm1 <- lm(log(Interest_Rate) ~ as.factor(Employed) + as.factor(`Delinquency?`) + log(Title_Lenght) + 
            Debt_To_Income  + as.factor(`Home_Owner?`) + Purpose_Of_Loan +
            Revolving_Balance + Revolving_Utilization + log(Number_Open_Accounts) +
            poly(I(Loan_Amount_Requested/Annual_Income), degree = 3, raw = F) + Collections_12Mo_Exclude_Med +
            Inquiries_Last_6Mo + as.factor(Derogated) + as.factor(Initial_Loan_Term_Months) +
            as.factor(Record) + as.factor(`Description?`) + as.factor(Income_Verified) + 
            Loan_Sentiment + title_sentiment , data = intdat.no.outlier)
summary(lm1)
###Prediction
library(lm.beta)
lm.beta(lm1)

predicted.interest <- as.data.frame(predict.lm(lm1, test))
predicted.interest <- exp(predicted.interest)
summary(predicted.interest)

write.csv(predicted.interest, file = "Predicted_interest_rates.csv")