df <- read.csv('Hospitals_Train.csv')
df_test <- read.csv('Hospitals_Test_X.csv')



#Section 1: Data Insights-----
#-Struction of data----
str(df)

#-Cleaning of data, visualization of data, etc.----
df[df == ''] = NA
sapply(df, function(x) sum(is.na(x)))

df_test[df_test == ''] = NA
sapply(df_test, function(x) sum(is.na(x)))


###INDEX
###If index is NA, all other variables are NA, so we remove rows that have index with NA
df_na <- subset(df, is.na(df$INDEX))
sapply(df_na, function(x) sum(is.na(x)))

df <- subset(df, !is.na(df$INDEX))
sapply(df, function(x) sum(is.na(x)))

###GENDER
###Gender has only 2 NA, randomly fill the NAs by the most category of gender(Male).
table(df$GENDER)
df$GENDER[is.na(df$GENDER)] = 'Male'
sum(is.na(df$GENDER))

###RACE
###Since there is a category of RACE named unknown, we implement NA values as unknown
table(df$RACE)
df$RACE[is.na(df$RACE)] = 'Unknown'
sum(is.na(df$RACE))

###ETHNICITY
###So is ETHNICITY, there is a category called unknown, implement NA values as unknown as well
table(df$ETHNICITY)
df$ETHNICITY[is.na(df$ETHNICITY)] = 'Unknown'
sum(is.na(df$ETHNICITY))

###ED_RESULT
###There are many categories in this variable, and from now we have only 73 NAs, we think simply remove those values is fine.
table(df$ED_RESULT)
df <- subset(df, !is.na(df$ED_RESULT))
sum(is.na(df$ED_RESULT))


###CHARGES
###Convert data type from factor to float, 98 NA pops out, remove it.
table(df$CHARGES)
df <- subset(df, df$CHARGES != '#VALUE!')
df$CHARGES <- as.character(df$CHARGES)
df$CHARGES <- as.numeric(df$CHARGES)

sum(is.na(df$CHARGES))

###So far:
sapply(df, function(x) sum(is.na(x)))

###ACUITY_ARR
###There are about 10% of the data that has ACUITY_ARR with NAs, we think we can remove them.
df <- subset(df, !is.na(df$ACUITY_ARR))
sum(is.na(df$ACUITY_ARR))

###CONSULT_IN_ED -> Missing = 0
df$CONSULT_IN_ED[is.na(df$CONSULT_IN_ED)] = 0
table(df$CONSULT_IN_ED)

###The ADMIT_RESULT, RISK and SEVERITY has only few data we can use, we subset a minor dataframe with no NA of RISK and SEVERITY, and with the original data set we remove those four columns.
table(df$ADMIT_RESULT)
table(df$RISK)
table(df$SEVERITY)

df$CONSULT_CHARGE <- as.factor(df$CONSULT_CHARGE)
df$CONSULT_IN_ED <- as.factor(df$CONSULT_IN_ED)
df$CONSULT_ORDER <- as.factor(df$CONSULT_ORDER)


###No Details for ADMIT_RESULT, RISK, SEVERITY
#ADMIT_RESULT
levels <- levels(df$ADMIT_RESULT)
levels[length(levels) + 1] <- 'No Details'
df$ADMIT_RESULT <- factor(df$ADMIT_RESULT, levels = levels)

df$ADMIT_RESULT[is.na(df$ADMIT_RESULT)] = 'No Details'
table(df$ADMIT_RESULT)


#RISK
levels_1 <- levels(df$RISK)
levels_1[length(levels_1) + 1] <- 'No Details'
df$RISK <- factor(df$RISK, levels = levels_1)

df$RISK[is.na(df$RISK)] = 'No Details'
table(df$RISK)


#SEVERITY
levels_2 <- levels(df$SEVERITY)
levels_2[length(levels_2) + 1] <- 'No Details'
df$SEVERITY <- factor(df$SEVERITY, levels = levels_2)

df$SEVERITY[is.na(df$SEVERITY)] = 'No Details'
table(df$SEVERITY)



str(df)


###Get rid of RETURN == #N/A
table(df$RETURN)
df <- subset(df, df$RETURN != '#N/A')

str(df)
df <- droplevels(df)



###Check if there is NA left.
sum(is.na(df))


#Plot Correlation Matrix
library(ggplot2)
library(GGally)
ggcorr(df, label = T)
#-Value used/not used; did you use any new variables?----

#New Variables------------------------------------------------
## Combine minority race into other
df$RACE[df$RACE=='Two or More Races']='Other'
df$RACE[df$RACE=='Declined to Answer']="Unknown"
df$RACE[df$RACE=='Native Hawaiian or Other Pacific Islander']='Other'
df$RACE[df$RACE=='Hispanic']='Other'
df$RACE[df$RACE=='American Indian or Alaskan Native']='Other'
df$RACE[df$RACE=='Asian']='Other'
df$RACE<-droplevels(df$RACE)
table(df$RACE)

###ETH Decline + Unknown 
table(df$RACE, df$RETURN)
table(df$ETHNICITY, df$RETURN)

df$ETHNICITY[df$ETHNICITY=='Declined to Answer']='Unknown'
df$ETHNICITY <- droplevels(df$ETHNICITY)

###charge vs return statistics
summary(df$CHARGES)

library('dplyr')
df %>% 
  group_by(RETURN) %>% 
  summarize(min=min(CHARGES), q1=quantile(CHARGES, 0.25), median = median(CHARGES), mean=mean(CHARGES), q3=quantile(CHARGES, 0.75), max=max(CHARGES))


###MONTH -> Quarter, HOUR -> day, night, midnight
for (i in 1:nrow(df)){
  if (df$MONTH_ARR[i] <= 3){
    df$QUARTER[i] = 'Q1'
  } else if (df$MONTH_ARR[i] <= 6){
    df$QUARTER[i] = 'Q2'
  } else if (df$MONTH_ARR[i] <= 9){
    df$QUARTER[i] = 'Q3'
  } else {
    df$QUARTER[i] = 'Q4'
  }
}

df$QUARTER <- as.factor(df$QUARTER)

table(df$QUARTER)
table(df$HOUR_ARR)

for (i in 1:nrow(df)){
  if (df$HOUR_ARR[i] <= 7){
    df$DAYTIME[i] = 'midnight'
  } else if (df$HOUR_ARR[i] <= 15){
    df$DAYTIME[i] = 'day'
  } else {
    df$DAYTIME[i] = 'night'
  }
}

df$DAYTIME <- as.factor(df$DAYTIME)
table(df$DAYTIME)

### CONSULT ORDER & CHARGE validation
table(df$RETURN[df$CONSULT_ORDER == 0 & df$CONSULT_CHARGE == 0])
table(df$RETURN[df$CONSULT_ORDER == 1 & df$CONSULT_CHARGE == 0])
table(df$RETURN[df$CONSULT_ORDER == 0 & df$CONSULT_CHARGE == 1])
table(df$RETURN[df$CONSULT_ORDER == 1 & df$CONSULT_CHARGE == 1])

for (i in 1:nrow(df)){
  if (df$CONSULT_ORDER[i] == 1 & df$CONSULT_CHARGE[i] == 0){
    df$CONSULT_NOTCOME[i] = 1
  } else {
    df$CONSULT_NOTCOME[i] = 0
  }
}

df$CONSULT_NOTCOME <- as.factor(df$CONSULT_NOTCOME)


###Interaction: FINANCIAL % Charge, AGE*GENDER



###Correlation Matrix
ggcorr(df, label = T)


##Reduce levels 5%
nrow(df)*0.05

###inancial_class: Medicoid + Medicoid Pending, 5%
table(df$FINANCIAL_CLASS)
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Global Contracts'] = 'Other'
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Medicare Replacement Plan'] = 'Other'
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Military'] = 'Other'
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Out of State Medicaid'] = 'Other'
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Worker\'s Comp'] = 'Other'
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Medicaid Pending'] = 'Medicaid'
df <- droplevels(df)


###ED_RESULT: Admit, Discharge, Leave, Deceased, Observation, Other
levels_3 <- levels(df$ED_RESULT)
levels_3[length(levels_3) + 1] <- 'Leave'
levels_3[length(levels_3) + 1] <- 'Other'
df$ED_RESULT <- factor(df$ED_RESULT, levels = levels_3)

#After asking
df$ED_RESULT[df$ED_RESULT == 'Admit to External Psychiatric Facility'] = 'Admit'
df$ED_RESULT[df$ED_RESULT == 'Admit to UMMS Psychiatry '] = 'Admit'
df$ED_RESULT[df$ED_RESULT == 'Transfer'] = 'Other'
df$ED_RESULT[df$ED_RESULT == 'AMA'] = 'Leave'
df$ED_RESULT[df$ED_RESULT == 'Arrived in Error'] = 'Other'
df$ED_RESULT[df$ED_RESULT == 'Elopement'] = 'Leave'
df$ED_RESULT[df$ED_RESULT == 'Left prior to completing treatment'] = 'Leave'
df$ED_RESULT[df$ED_RESULT == 'Left without signing discharge instructions'] = 'Leave'
df$ED_RESULT[df$ED_RESULT == 'LWBS after Triage'] = 'Leave'
df$ED_RESULT[df$ED_RESULT == 'LWBS before Triage'] = 'Leave'
df$ED_RESULT[df$ED_RESULT == 'Send to L&D after Rooming'] = 'Admit'
df$ED_RESULT[df$ED_RESULT == 'Send to L&D Before Rooming (Mom)'] = 'Admit'

df <- droplevels(df)


###Acutity_ARR Purple -> 5-Non-Urgent
table(df$ACUITY_ARR)
df$ACUITY_ARR[df$ACUITY_ARR == '5 Purple'] = '5-Non-Urgent'
df <- droplevels(df)

##DC_RESULT 5%
table(df$DC_RESULT)
levels1 <- levels(df$DC_RESULT)
levels1[length(levels1) + 1] <- 'Other'
df$DC_RESULT <- factor(df$DC_RESULT, levels = levels1)
df$DC_RESULT <- ifelse(df$DC_RESULT == 'Home or Self Care', 'Home or Self Care', ifelse(df$DC_RESULT == 'LEFT W/O BEING SEEN AFTER TRIAGE', 'LEFT W/O BEING SEEN AFTER TRIAGE', 'Other'))
df <- droplevels(df)

df$DC_RESULT <- as.factor(df$DC_RESULT)

table(df$DC_RESULT)
sum(df$DC_RESULT == 'Home or Self Care')/nrow(df)

#-Visualization-----------------------------------------------------------

#Correlation Matrix
ggcorr(df, label = T)

###??????try pie chart
return_arr <- data.frame(table(df$RETURN))
colnames(return_arr) <- c('Return', 'Freq')

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

library(scales)
ggplot(return_arr, aes(x="", y = Freq, fill=Return)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar('y', start = 0) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Freq/2, 
                label = percent(Freq/sum(Freq))), size=5) +
  ggtitle('Pie Chart of Return')

#Arrive and departure: weekday and hour, heatmap
week_hour_arr <- data.frame(table(df$WEEKDAY_ARR, df$HOUR_ARR))
colnames(week_hour_arr) <- c('week', 'hour', 'freq')

ggplot(aes(x = week, y = hour, fill = freq), data = week_hour_arr) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  ggtitle('Heatmap of Arrival Week and Hour')

week_hour_dep <- data.frame(table(df$WEEKDAY_DEP, df$HOUR_DEP))
colnames(week_hour_dep) <- c('week', 'hour', 'freq')


sum(ifelse(df$HOUR_ARR == df$HOUR_DEP & df$WEEKDAY_ARR == df$WEEKDAY_DEP, 1, 0))

ggplot(aes(x = week, y = hour, fill = freq), data = week_hour_dep) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  ggtitle('Heatmap of Departure Week and Hour')

sum(ifelse(df$HOUR_ARR == df$HOUR_DEP & df$WEEKDAY_ARR == df$WEEKDAY_DEP, 1, 0))

#Consult charge vs Return
ggplot(aes(x = RETURN, y = CONSULT_ORDER), data = subset(df, CONSULT_ORDER == 1)) +
  geom_jitter() +
  ggtitle('RETURN vs CONSULT_ORDER = 1')

ggplot(aes(x = RETURN, y = CONSULT_CHARGE), data = subset(df, CONSULT_CHARGE == 1)) +
  geom_jitter() +
  ggtitle('RETURN vs CONSULT_CHARGE = 1')

str(df)

###plot diag_details
table(df$DIAG_DETAILS, df$RETURN)
ggplot(df, aes(x = DIAG_DETAILS)) + 
  geom_line(stat = 'count') +
  facet_grid(~RETURN)

#Race vs Return
race_count <- data.frame(table(df$RACE, df$RETURN))
race_count <- subset(race_count, Freq != 0)
race_count <- race_count[order(race_count$Freq, decreasing = T),]
colnames(race_count) <- c('Race', 'Return', 'Count')

ggplot(aes(x = reorder(Race, -Count), y = Count, fill = Return), data = race_count) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete('Race') +
  ggtitle('Race vs Return')

table(df$FINANCIAL_CLASS)

###percentage calculate
financial_count <- data.frame(table(df$FINANCIAL_CLASS, df$RETURN))
financial_count <- subset(financial_count, Freq != 0)
financial_count <- financial_count[order(financial_count$Freq, decreasing = T),]
colnames(financial_count) <- c('Financial', 'Return', 'Count')


financial_count <- financial_count %>% 
  group_by(Financial) %>% 
  mutate(Return, Percent = Count/sum(Count))

financial_count <- data.frame(financial_count)
financial_count$Percent <- round(financial_count$Percent*100, 2)
financial_count

ggplot(aes(x = reorder(Financial, -Count), y = Count/sum(financial_count$Count), fill = Return), data = financial_count) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete('Financial') + 
  geom_text(aes(label = Percent), size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  ylab('Overall Percentage') +
  ggtitle('Financial vs Return') 


#MONTH_ARR
#-Overall
ggplot(aes(x = MONTH_ARR), data = df) +
  geom_bar(stat="count") + 
  scale_x_continuous(breaks=c(1:12)) +
  ggtitle('Month Arrival')

#-By HOSPITAL
ggplot(aes(x = MONTH_ARR), data = df) +
  geom_bar(stat="count", aes(fill = HOSPITAL)) + 
  scale_x_continuous(breaks=c(1:12)) +
  facet_grid( ~ HOSPITAL) +
  ggtitle('Month arrival of each hospital')


#AGE vs Return
#-Overall
ggplot(aes(x = RETURN, y = AGE), data = df) +
  geom_boxplot() +
  ggtitle('Age vs Return')

#-By gender
ggplot(aes(x = RETURN, y = AGE), data = df) +
  geom_boxplot(aes(color = GENDER)) +
  ggtitle('Age vs Return group by Gender')




#Charge vs Return
ggplot(aes(x = RETURN, y = CHARGES), data = df) +
  geom_jitter() +
  ggtitle('Charge vs Return')

ggplot(aes(x = RETURN, y = log(CHARGES)), data = df) +
  geom_boxplot(outlier.colour = 'pink', outlier.size = 1) +
  ggtitle('Log(Charge) vs Return Boxplot')


#Section 2: Modeling Insights-----
#-Models you tried

#---Baseline
sum(df$RETURN == 'No')/length(df$RETURN)

#-Evaluation of models
str(df)
#-Insights you gained from running these models

#Section 3: Conclusion and Future Work-----
#-How are you planning to use this/expand this for Phase II?


library(caret)

## FINANCIAL_CLASS
df$FINANCIAL_CLASS[df$FINANCIAL_CLASS == 'Global Contracts'|df$FINANCIAL_CLASS == 'Medicaid Pending'|
                     df$FINANCIAL_CLASS == 'Military'|df$FINANCIAL_CLASS == 'Out of State Medicaid'|
                     df$FINANCIAL_CLASS == "Worker's Comp"|df$FINANCIAL_CLASS == 'Medicare Replacement Plan'] = 'Other'

## ED_RESULT
levels <- levels(df$ED_RESULT)
levels[length(levels) + 1] <- "Other"
df$ED_RESULT <- factor(df$ED_RESULT, levels = levels)
df$ED_RESULT[df$ED_RESULT == 'Admit to External Psychiatric Facility'|df$ED_RESULT == 'Admit to UMMS Psychiatry '|
               df$ED_RESULT == 'Arrived in Error'|df$ED_RESULT == 'Deceased'|df$ED_RESULT == 'Left without signing discharge instructions'|
               df$ED_RESULT == 'LWBS before Triage'|df$ED_RESULT == 'Send to L&D after Rooming'|
               df$ED_RESULT == 'Send to L&D Before Rooming (Mom)'|df$ED_RESULT == 'AMA'|df$ED_RESULT == 'Elopement'|
               df$ED_RESULT == 'Transfer'|is.na(df$ED_RESULT)] <- 'Other'

## DC_RESULT
levels <- levels(df$DC_RESULT)
levels[length(levels) + 1] <- "Other"
df$DC_RESULT <- factor(df$DC_RESULT, levels = levels)
df$DC_RESULT[df$DC_RESULT != 'Home or Self Care' & df$DC_RESULT != 'LEFT W/O BEING SEEN AFTER TRIAGE'&
               df$DC_RESULT != 'Home Health Care Svc'& df$DC_RESULT != 'LEFT W/O COMPLETED TREATMENT'&
               df$DC_RESULT != 'Rehab Facility'|is.na(df$DC_RESULT)] <- 'Other'
count(df$DC_RESULT)

df <- droplevels(df)

df_up_train <- upSample(x = select(df, -RETURN),
                        y = df$RETURN,
                        yname = 'RETURN') %>% as_tibble()
library(caret)
in_train <- createDataPartition(df_up_train$RETURN, p=0.75, list = FALSE)
df_valid <- df_up_train[-in_train,]
df_train <- df_up_train[in_train,]
