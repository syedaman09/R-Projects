#######################################################
###########R_Project################################
#########Logistic Regression########################

####Data Set- Web_Click####

# Not allowing R to use scientific notation till specified number
options(scipen=1000000)

# Step-1
#Loading the raw Data

InputData= read.csv('D:/Data Science/IVY/R/Project/Logistic Regression/Web_data.csv')

head(InputData)

## Step-2
## Explore the data set

summary(InputData)

str(InputData)


dim(InputData)

## Finding total no. of unique values in each variable at once


lengths(lapply(InputData,unique))


ContinuousCols=c('Time_Spent','Age','Avg_Income','Internet_Usage')


CategoricalCols=c('Male','Time_Period','Weekday','Month','Ad_Topic','Clicked')

QualitativeCols=c('VisitID','Year','Country_Name','City_code')

## we don't consider qualitative data for predictive modelling

 ###Country_Name  : Factor w/ 237


InputData=InputData[,c(ContinousCols,CategoricalCols)]

head(InputData)


#### Step-3###

## Identify the Problem- We are predicting who clicked on the advertisement published on website or not using 
##Logistic Regression

###Step-4

## Target Variable = Clicked

###Step-5

##### Our Target variable(Clicked) is a categorical variable hence we are doing Logistic Regression###

### Step-6
## Explore each potential predictor for  distribution and quality


#### histogram for Continous Variables

# library to generate professional colors

library(RColorBrewer)

### Histogram for multiple Column at once


## For spliting windows

par(mfrow=c(3,3))

ColsForHist=c('Time_Spent','Age','Avg_Income','Internet_Usage')

# looping to create the histograms for each column

for(contcols in ColsForHist) {

hist(InputData[,c(contcols)], main=paste('Histogram of;', contcols),
col=brewer.pal(8,'Paired'))
}

### All the variables having good quality distribution

## changed chracter to numeric

InputData$Ad_Topic=as.factor(InputData$Ad_Topic)

str(InputData)
############################################################

# Exploring MULTIPLE CATEGORICAL features

## barplot for multiple categorical variable at once


## For splitting windows

par(mfrow=c(3,3))


ColsForBar=c('Male','Time_Period','Weekday','Month','Clicked','Ad_Topic',)


# looping to create the barplot for each column


for (catcols in ColsForBar) {

barplot(table(InputData[,c(catcols)]), main=paste('Barplot of;',  catcols),
 col=brewer.pal(8,'Paired'))
}

##All good

#########################################################
###Step-7

## Exploring visual relationship b/w target variable and predictors


# Categorical Vs Continuous --- Box Plot
# Categorical Vs Categorical --  Bar chart

##################################
# Categorical Vs Continuous Visual analysis: Boxplot

### Box plot for single continuous variables

boxplot(Time_Spent~Clicked,data=InputData,col=brewer.pal(8,'Paired'))

boxplot(Age~Clicked,data=InputData,col=brewer.pal(8,'Paired'))

boxplot(Avg_Income~Clicked,data=InputData,col=brewer.pal(8,'Paired'))

boxplot(Internet_Usage~Clicked,data=InputData,col=brewer.pal(8,'Paired'))


## All good

#########################
# Categorical Vs Categorical --  Bar chart


CrossTabResult=table(InputData[,c('Clicked','Male')])


barplot(CrossTabResult, legend=T,beside=T,col=c('Red','Green'))


CrossTabResult=table(InputData[,c('Clicked','Time_Period')])


barplot(CrossTabResult, legend=T,beside=T,col=c('Red','Green'))

CrossTabResult=table(InputData[,c('Clicked','Weekday')])


barplot(CrossTabResult, legend=T,beside=T,col=c('Red','Green'))


CrossTabResult=table(InputData[,c('Clicked','Month')])


barplot(CrossTabResult, legend=T,beside=T,col=c('Red','Green'))



CrossTabResult=table(InputData[,c('Clicked','Ad_Topic')])


barplot(CrossTabResult, legend=T,beside=T,col=c('Red','Green'))




###All good

#############
##Step-8

##Statistical Relationship b/w  target variable and predictor

# Categorical Vs Continuous --- ANOVA
# Categorical Vs Categorical -- Chi-square test


##### Continuous Vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# F-Statistic is Mean Sq error/residual MeanSq error
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)



# looping to perform anova test for each column

ContinousCols=c('Time_Spent','Age','Avg_Income','Internet_Usage',)

for (contcols in ContinousCols) {
 anovaData=InputData[,c('Clicked',contcols)]
 (print(str(anovaData)))
 print(summary(aov(Clicked~.,data=anovaData)))
}



################################
# Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)


CrossTabResult1=table(InputData[,c('Clicked','Male')])


chisq.test(CrossTabResult1)


CrossTabResult2=table(InputData[,c('Clicked','Time_Period')])


chisq.test(CrossTabResult2)


CrossTabResult3=table(InputData[,c('Clicked','Weekday')])


chisq.test(CrossTabResult3)

#### p-value = 0.722

#Ho is accepted(Weekday)

CrossTabResult4=table(InputData[,c('Clicked','Month')])


chisq.test(CrossTabResult4)

##p-value = 0.4229

#Ho is accepted(Month)


CrossTabResult5=table(InputData[,c('Clicked','Ad_Topic')])


chisq.test(CrossTabResult5)




### Step-9
### Treating missing values

colSums(is.na(InputData))



###No missing values


### Step-10

## # Generating the Data frame for machine learning

TargetVariableName=c('Clicked')


 # Making sure the class of Target variable is FACTOR

TargetVariable=as.factor(InputData[, c(TargetVariableName)])
class(TargetVariable)



BestPredictorVaribles=c('Time_Spent','Age','Avg_Income','Internet_Usage','Male','Time_Period','Ad_Topic')

PredictorVariables=InputData[,c(BestPredictorVaribles)]

DataForML=data.frame(TargetVariable,PredictorVariables)
str(DataForML)


#############################################################################################
# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

#############################################################################################
#############################################################################################
# Creating Predictive models on training data to check the accuracy on test data
###### Logistic Regression #######

startTime=Sys.time()
LR_Model=glm(TargetVariable ~ ., data=DataForMLTrain, family='binomial')
LR_Model
summary(LR_Model)
endTime=Sys.time()
endTime-startTime

### Perfom Durbin-Watson

durbinWatsonTest(LR_Model)


 #lag Autocorrelation D-W Statistic p-value
   #1     -0.08000955      2.153429    0.05
# Alternative hypothesis: rho != 0


# Checking Accuracy of model on Testing data
PredictionProb=predict(LR_Model, DataForMLTest, type = "response")
DataForMLTest$Prediction=ifelse(PredictionProb>0.6, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")
AccuracyResults

str(DataForML)
# Since Accuracy Results is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]


## SinificantVariables=c('Time_Spent','Age','Avg_Income','Internet_Usage','Male','Time_Period','Ad_Topic')




##############End##########




























