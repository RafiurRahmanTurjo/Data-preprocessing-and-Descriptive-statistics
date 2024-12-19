library("dplyr")
library("readxl")      #Library to get .xlsx
library("naniar")
library("mice")
library("ggplot2")
library("caret")


#Getting Things Ready --------------------------------------------------------------------------------------------------
#getting the Given Dataset inside a Dataframe
givenDataFrame <- read_excel("D:\\AIUB\\Fall 24-25\\Data Science\\Mid Project 1\\Given Dataset\\dataset.xlsx")

#Turning the dataframe into lowercase
givenDataFrame <- as.data.frame(apply(givenDataFrame,2, tolower))


#Deleting Spaces
totalAttribute <- ncol(givenDataFrame)
i <- 1

while(i <= totalAttribute){
  givenDataFrame[,i] <- trimws(givenDataFrame[,i])
  i <- i+1
}

#----------------------------------------------------------------------------------------------------------------------


#Visualizing Missing Values -------------------------------------------------------------------------------------------
gg_miss_upset(givenDataFrame)

#----------------------------------------------------------------------------------------------------------------------


#Removing Duplicate Values --------------------------------------------------------------------------------------------
givenDataFrame <- distinct(givenDataFrame)

#----------------------------------------------------------------------------------------------------------------------


#Handling Missing Values ----------------------------------------------------------------------------------------------
colSums(is.na(givenDataFrame))          #To check if dataset contains any missing value

#Method 1 - Discard Instances for Depression Attribute & Gender Attribute
givenDataFrame <- subset(givenDataFrame,!is.na(Gender))
givenDataFrame <- subset(givenDataFrame,!is.na(Depression))

#Method 2 -  Most Frequent Value for Sleep Duration
mostFreqVal <- sort(table(givenDataFrame$`Sleep Duration`), decreasing = TRUE)
givenDataFrame$`Sleep Duration`[is.na(givenDataFrame$`Sleep Duration`)] <- names(mostFreqVal[1]) 

#Method 3 - Average Value/Mean Value for Age
average <- mean(as.numeric(givenDataFrame$Age[!is.na(givenDataFrame$Age)]))
givenDataFrame$Age[is.na(givenDataFrame$Age)] <- floor(average)


#---------------------------------------------------------------------------------------------------------------------


#Handeling Invalid Instances (Categorical) ---------------------------------------------------------------------------
#Sleep Duration
all_possible_values <- tolower(c("5-6 Hours","7-8 Hours","More than 8 hours","Less than 5 hours"))
givenDataFrame <- subset(givenDataFrame,`Sleep Duration` %in% all_possible_values)

#Have you ever had suicidal thoughts?
all_possible_values <- tolower(c("yes","no"))
givenDataFrame <- subset(givenDataFrame,`Have you ever had suicidal thoughts ?` %in% all_possible_values)

#Family History of Mental Illness
all_possible_values <- tolower(c("yes","no"))
givenDataFrame <- subset(givenDataFrame,`Family History of Mental Illness` %in% all_possible_values)

#Depression
all_possible_values <- tolower(c("yes","no"))
givenDataFrame <- subset(givenDataFrame,`Depression` %in% all_possible_values)

#Gender
all_possible_values <- tolower(c("male","female"))
givenDataFrame <- subset(givenDataFrame,`Gender` %in% all_possible_values)

#Dietary Habits
all_possible_values <- tolower(c("unhealthy","moderate","healthy"))
givenDataFrame <- subset(givenDataFrame,`Dietary Habits` %in% all_possible_values)

#---------------------------------------------------------------------------------------------------------------------


#Handling Outliers --------------------------------------------------------------------------------------------------
handle_outliers <- function(dataFrame, column){
  totalData <- nrow(dataFrame)
  d_stats <- summary(as.numeric(dataFrame[,column]))
  
  
  IQR <- 1.5 * (as.numeric(d_stats[5]) - as.numeric(d_stats[2]))
  
  
  q1 <- as.numeric(d_stats[2]) - IQR
  q3 <- as.numeric(d_stats[5]) + IQR
  
  
  i <- 1
  while(i <= totalData){
    if(!between(as.numeric(dataFrame[i,column]),q1,q3)){
      dataFrame <- dataFrame[-i,]
      i <- i-1
      totalData <- totalData - 1
    }
    i <- i+1
  }
  return(dataFrame)
}


givenDataFrame <- handle_outliers(givenDataFrame, 3)
givenDataFrame <- handle_outliers(givenDataFrame, 4)
givenDataFrame <- handle_outliers(givenDataFrame, 8)
givenDataFrame <- handle_outliers(givenDataFrame, 9)
givenDataFrame <- handle_outliers(givenDataFrame, 2)


#---------------------------------------------------------------------------------------------------------------------


#Normalize Data ------------------------------------------------------------------------------------------------------
normalize_data <- function(dataFrame, column){
  i <- 1
  totalData <- nrow(dataFrame)

  maxVal <- as.numeric(max(dataFrame[,column]))
  minVal <- as.numeric(min(dataFrame[,column]))

  while(i <= totalData){
    dataFrame[i,column] <- (as.numeric(dataFrame[i,column]) - minVal) / (maxVal-minVal)
    i <- i+1
  }
  return(dataFrame)
}


normalized_df <- normalize_data(givenDataFrame, 2)



#----------------------------------------------------------------------------------------------------------------------


#Handling Imbalanced Dataset ------------------------------------------------------------------------------------------

#Oversampling
value_count <- sort(table(givenDataFrame$Depression))
minor_value <- names(value_count)
totalData <- nrow(givenDataFrame)
oversampled <- 0
i <- 1

while(i <= totalData && oversampled <= (as.numeric(value_count[length(value_count)]) - as.numeric(value_count[1]))-1){
  
  if(givenDataFrame[i,11] == minor_value[1]){
    givenDataFrame <- rbind(givenDataFrame, givenDataFrame[i,])
    oversampled <- oversampled + 1
    totalData <- totalData + 1
  }
  i <- i+1
}

print(table(givenDataFrame$Depression))


#Undersampling
value_count <- sort(table(givenDataFrame$Depression), decreasing = TRUE)
major_value <- names(value_count)
totalData <- nrow(givenDataFrame)
undersampled <- 0
i <- 1
while(i <= totalData && undersampled <= (as.numeric(value_count[1]) - as.numeric(value_count[length(value_count)]))-1){
  if(givenDataFrame[i,11] == major_value[1]){
    givenDataFrame <- givenDataFrame[-i,]
    undersampled <- undersampled + 1
    totalData <- totalData + 1
  }
  i <- i+1
}

print(table(givenDataFrame$Depression))

#----------------------------------------------------------------------------------------------------------------------


#Categorical to numeric ----------------------------------------------------------------------------------------------
givenDataFrame$`Sleep Duration` <- as.numeric(factor(givenDataFrame$`Sleep Duration`, levels = c("less than 5 hours", "5-6 hours",
                                                                                                 "7-8 hours","more than 8 hours")))
givenDataFrame$Depression <- as.numeric(factor(givenDataFrame$Depression, levels = c("no","yes")))
givenDataFrame$Gender <- as.numeric(factor(givenDataFrame$Gender, levels = c("female","male")))
givenDataFrame$`Dietary Habits` <- as.numeric(factor(givenDataFrame$`Dietary Habits`, levels = c("unhealthy","moderate","healthy")))
givenDataFrame$`Have you ever had suicidal thoughts ?` <- as.numeric(factor(givenDataFrame$`Have you ever had suicidal thoughts ?`, levels = c("no","yes")))
givenDataFrame$`Family History of Mental Illness` <- as.numeric(factor(givenDataFrame$`Family History of Mental Illness`, levels = c("no","yes")))

#---------------------------------------------------------------------------------------------------------------------


#Numeric to Categorical -----------------------------------------------------------------------------------------------

givenDataFrame$`Academic Pressure` <- factor(givenDataFrame$`Academic Pressure`, levels = c(1,2,3,4,5), labels=c("Low","Below Average", "Average", "High", "Extremely High"))

print(length(table(givenDataFrame$`Academic Pressure`)))

#----------------------------------------------------------------------------------------------------------------------



#Measure of Central Tendency-------------------------------------------------------------------------------------------
average <- mean(as.numeric(givenDataFrame$Age))
med <- median(as.numeric(givenDataFrame$`Study Hours`))
mod <- names(sort(table(givenDataFrame$Gender),decreasing = TRUE))[1]
#----------------------------------------------------------------------------------------------------------------------


#Measure of Spread-----------------------------------------------------------------------------------------------------
range <- max(as.numeric(givenDataFrame$Age)) - min(as.numeric(givenDataFrame$Age))
std_dev <- sd(givenDataFrame$Age)
variance <- var(givenDataFrame$Age)

print(variance)
#----------------------------------------------------------------------------------------------------------------------


#Feature Selection----------------------------------------------------------------------------------------------------

i <- 1
while(i <= ncol(givenDataFrame)){
  givenDataFrame[,i] <- as.numeric(givenDataFrame[,i])
  i <- i+1
}

cor_matrix <- cor(givenDataFrame[,-11])

threshold <- 0.8

highly_correlated <- (findCorrelation(cor_matrix, cutoff = threshold))

print(highly_correlated)

if(length(highly_correlated != 0)){
  df_reduced <- givenDataFrame[, -highly_correlated]  
}else{
  df_reduced <- givenDataFrame
}


#Data Sampling --------------------------------------------------------------------------------------------------------
sample_data <- givenDataFrame[sample(1:nrow(givenDataFrame),50),]

#----------------------------------------------------------------------------------------------------------------------


#Data Filteration Techniques-------------------------------------------------------------------------------------------
subset(givenDataFrame,`Sleep Duration` == "2")[5:10,]
subset(givenDataFrame,`Dietary Habits` == "unhealthy")[1:10,]
filter(givenDataFrame,`Study Hours` >= 6)[1:10,]
arrange(givenDataFrame,Age)
select(givenDataFrame,Gender,Age)
mutate(givenDataFrame, avg=as.numeric(`Study Hours`)/4)
transmute(givenDataFrame, avg=as.numeric(`Study Hours`)/4)
summarize(givenDataFrame, mean(as.numeric(Age)), sum(as.numeric(`Study Hours`)))
