################### R_Project########## Linear Regression############

### Data Set- Life Expectancy Data########

# Not allowing R to use scientific notation till specified number
options(scipen=1000000)


### We will do first exploratory data analysis to find out  which variable are co-related  and have impact on Target Variable####	

#### Step-1####

### Loading the raw Data###


InputData=read.csv('D:/Data Science/IVY/R/Project/Linear Regression/Life Expectancy Data.csv')

head(InputData)
dim(InputData)


##Step-2 ###

### Exploring the  dataset###

summary(InputData)

str(InputData)


## Finding total no. of unique values in each variable at once


lengths(lapply(InputData,unique))


## We have distributed all the variables in three categories as ContinousCols(Unique values>20), CategoricalCols(Unique values<20) and Qualitative Cols


ContinousCols= c('Life_Expectancy','Adult_Mortality','Infant_Deaths','Alcohol','Percentage_Expenditure',
'Measles','BMI','Under.five_Deaths','Polio','Total_Expenditure','Diphtheria','HIV.AIDS',
'GDP','Per_Capita_GDP','Population','Thinness_1.19_Years','Thinness_5.9_Years','Income_Composition_of_Resources',
'Schooling')


CategoricalCols=c('Year','Status')

QualitativeCols=c('Country','Hepatitis_B')

###Country   : Factor w/ 192 levels

## It's having factor level more than 30 and it come  under qualitative data and we do not consider 

## Hepatitis_B - It's having 553 missing values and it is not  good to treat that much  of missing values




InputData[, QualitativeCols]= NULL



str(InputData)


#### Step-3###

## Identify the Problem- We are predicting next year life expectancy value using linear regression###


###Step-4##

## Target Variable= 'Life_Expentancy'

###Step-5###

##### Our Target variable(Life_Expectancy) is a continous variable hence we are doing Linear Regression###


#install.packages('RColorBrewer')

###Step-6###

## Explore each potential predictor for  distribution and quality


#### histogram for Continous Variables

# library to generate professional colors

library(RColorBrewer)

### Histogram for multiple Column at once

## For splitting windows

par(mfrow=c(5,4))

hist(InputData$Per_Capita_GDP, col=brewer.pal(8,"Paired"),main=paste('Histogram of Per Capita GDP'))

ColsForHist=c('Life_Expectancy','Adult_Mortality','Infant_Deaths','Alcohol','Percentage_Expenditure',
'Measles','BMI','Under.five_Deaths','Polio','Total_Expenditure','Diphtheria','HIV.AIDS',
'GDP','Per_Capita_GDP','Population','Thinness_1.19_Years','Thinness_5.9_Years',
'Income_Composition_of_Resources','Schooling')


# looping to create the histograms for each column

for (contCol in ColsForHist){
	hist(InputData[,c(contCol)], main=paste('Histogram of:', contCol), 
	col=brewer.pal(8,"Paired"))

}


## Treating Outliers

# Treating outliers using 12500 as the substitution

##Percentage_Expenditure

hist(InputData[InputData$Percentage_Expenditure<12000,'Percentage_Expenditure'])

InputData[InputData$Percentage_Expenditure>12000,'Percentage_Expenditure']=12500

hist(InputData$Percentage_Expenditure)



 ### These columns have poor quality distribution


####Infant_Deaths, Measles,Under.five_Deaths,HIV.AIDS,GDP,Population

str(InputData)
 

############################################################
# Exploring MULTIPLE CATEGORICAL features

#Splitting the plot window into two parts
par(mfrow=c(2,1))

# looping to create the Bar-Plots for each column


CategoricalCols=c('Year','Status')


for (catCols in CategoricalCols){
 barplot(table(InputData[,c(catCols)]),  main=paste('BarPlot of:', catCols),
 col=brewer.pal(8,'Paired'))
}


## Quality of distribution of  Year is not good as it showing no variance

## We will leave "Year"
#####


#### Step-7##

# Visual Relationship between predictors and target variable
# Continuous Vs Continuous ---- Scatter Plot
# Continuous Vs Categorical --- Box Plot


### # For multiple columns at once-- Scatter Plot

plot(x=InputData$Schooling,y=InputData$Life_Expectancy,col='blue')


ContinousCols= c('Life_Expectancy','Adult_Mortality','Infant_Deaths','Alcohol','Percentage_Expenditure',
'Measles','BMI','Under.five_Deaths','Polio','Total_Expenditure','Diphtheria','HIV.AIDS',
'GDP','Per_Capita_GDP','Population','Thinness_1.19_Years','Thinness_5.9_Years','Income_Composition_of_Resources',
'Schooling')

plot(InputData[,ContinousCols],col='blue')


# Continuous Vs Categorical Visual analysis: Boxplot

boxplot(Life_Expectancy~Status,InputData,col=brewer.pal(8,"Paired"))

boxplot(Life_Expectancy~Year,InputData,col=brewer.pal(8,"Paired"))

## Year-No Variance



# Step-8
# Statistical Relationship between predictors and target variable
# Continuous Vs Continuous ---- Correlation
# Categorical Vs Continuous --- ANOVA



## Multiple Column at once
## Correlation

ColsForCor= c('Life_Expectancy','Adult_Mortality','Infant_Deaths','Alcohol','Percentage_Expenditure',
'Measles','BMI','Under.five_Deaths','Polio','Total_Expenditure','Diphtheria','HIV.AIDS',
'GDP','Per_Capita_GDP','Population','Thinness_1.19_Years','Thinness_5.9_Years','Income_Composition_of_Resources',
'Schooling')

CorData= cor(InputData[,c(ColsForCor)], use='complete.obs')

CorData


# Final columns which has high correlation with the target variable


names(CorrData[,'Life_Expectancy'][abs(CorrData[,'Life_Expectancy'])>0.5])

##[1] "Life_Expectancy"                 "Adult_Mortality"                
##[3] "BMI"                             "HIV.AIDS"        
               
##[5] "Per_Capita_GDP"                  "Income_Composition_of_Resources"
#[7] "Schooling"                      


# These variables get selected as it has 50% correlation


## Correlation after selecting varibales which has 50% correlation


ColName= c('Life_Expectancy','Adult_Mortality','BMI','HIV.AIDS','Per_Capita_GDP','Population','Income_Composition_of_Resources',
'Schooling')

CorData= cor(InputData[,c(ColName)], use='complete.obs')

CorData



# Continuous Vs Categorical correlation strength: ANOVA
# Analysis of Variance(ANOVA)
# F-Statistic is Mean Sq error/residual MeanSq error
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)



###Anova test for multiple col at once

ColsForAnova=c('Year','Status')


for (catCol in ColsForAnova){
	anovaData= InputData[, c('Life_Expectancy', catCol)]
	print(str(anovaData))
	print(summary(aov(Life_Expectancy~., anovaData)))

}



#### Step-9

## Treating Missing values

colSums(is.na(InputData))

# Instead of deleting, we will treat individual columns using data imputation strategies as well

InputData$Life_Expectancy[is.na(InputData$Life_Expectancy)]<- median(InputData$Life_Expectancy,na.rm=T)

InputData$Alcohol[is.na(InputData$Alcohol)]<- median(InputData$Alcohol,na.rm=T)

InputData$BMI[is.na(InputData$BMI)]<- median(InputData$BMI,na.rm=T)

InputData$GDP[is.na(InputData$GDP)]<- median(InputData$GDP,na.rm=T)

InputData$Per_Capita_GDP[is.na(InputData$Per_Capita_GDP)]<- median(InputData$Per_Capita_GDP,na.rm=T)

InputData$Adult_Mortality[is.na(InputData$Adult_Mortality)]<- median(InputData$Adult_Mortality,na.rm=T)

InputData$Income_Composition_of_Resources[is.na(InputData$Income_Composition_of_Resources)]<- median(InputData$Income_Composition_of_Resources,na.rm=T)

InputData$Schooling[is.na(InputData$Schooling)]<- median(InputData$Schooling,na.rm=T)

InputData$Polio[is.na(InputData$Polio)]<- median(InputData$Polio,na.rm=T)

InputData$Total_Expenditure[is.na(InputData$Total_Expenditure)]<- median(InputData$Total_Expenditure,na.rm=T)

InputData$Diphtheria[is.na(InputData$Diphtheria)]<- median(InputData$Diphtheria,na.rm=T)

InputData$Population[is.na(InputData$Population)]<- median(InputData$Population,na.rm=T)

## After treating all missing values

colSums(is.na(InputData))



############################################################################

# Step-10
# Generating the Data frame for machine learning

TargetVariableName=c('Life_Expectancy')

TargetVariable=InputData[,c(TargetVariableName)]

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis


BestPredictorName=c('Status','Adult_Mortality','Percentage_Expenditure','BMI','Polio','Diphtheria','Per_Capita_GDP','Income_Composition_of_Resources',
'Schooling')

 
PredictorVariables=InputData[,c(BestPredictorName)]

DataForML=data.frame(TargetVariable,PredictorVariables)

head(DataForML)

str(DataForML)

#Converted character to factor 

DataForML$Status=as.factor(DataForML$Status)

#########################################################################
# Sampling | Splitting data into 70% for training 30% for testing


TrainingSamplingIndex= sample(1:nrow(DataForML),size=0.7*nrow(DataForML))

DataForMLTrain=DataForML[TrainingSamplingIndex,]

DataForMLTest=DataForML[-TrainingSamplingIndex,]

dim(DataForMLTrain)

dim(DataForMLTest)


########################################################################
# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######
startTime=Sys.time()

Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)
endTime=Sys.time()

endTime-startTime

Residual standard error: 4.616 on 2046 degrees of freedom
Multiple R-squared:  0.7649,    Adjusted R-squared:  0.7639 
F-statistic: 739.6 on 9 and 2046 DF,  p-value: < 0.00000000000000022

## Significant Variable = ('Status','Adult_Mortality','Percentage_Expenditure','BMI','Polio','Diphtheria','Per_Capita_GDP','Income_Composition_of_Resources','Schooling')

#### Durbin Watson test

durbinWatsonTest(Model_Reg)


## we will reject the high probablity and will accept which ever is close to 0
# Checking Accuracy of model on Testing data

DataForMLTest$Predict_LM=predict(Model_Reg,DataForMLTest)

head(DataForMLTest)


##### Calculating the Absolute Percentage Error for each prediction

LM_APE=100*(abs(DataForMLTest$Predict_LM - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)

print(paste('## Mean Accuracy of Linear Model is:', 100 - mean(LM_APE)))

print(paste('## Median Accuracy of Linear Model is:', 100 - median(LM_APE)))


## Mean Accuracy of Linear Model is: 94.9067884952327
## Median Accuracy of Linear Model is: 96.6434916893895"
 

########################################################################

