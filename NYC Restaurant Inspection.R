library(tidyverse)
library(lubridate)
library(ROCR)
library(ranger)
library(ggplot2)
set.seed(1234)

# NYC Restaurant Inspection Analysis: The goal of this project is to clean the 2017-2019 NYC restaurant inspection dataset and build machine learning model 
# to predict the inspection's outcome

# Part A: Cleaning the Restaurant Inspections Data---------------------------------------------------------------------------------------------------------
# 1.
# Make sure to change the working directory
all_data = read.csv('C:/Users/wangy/WD/DOHMH_New_York_City_Restaurant_Inspection_Results.csv')
as_tibble(all_data)
# 2.
all_data = all_data %>% select(-c("BUILDING", "STREET", "PHONE", "DBA", "ZIPCODE", "RECORD.DATE", 
                                  "VIOLATION.DESCRIPTION","GRADE.DATE")) %>%

                        rename_with(~c('id', 'borough', 'cuisine', 'action', 'code', 'critical', 'score', 'grade', 'inspection_type'),
                                    all_of(c('CAMIS', 'BORO', 'CUISINE.DESCRIPTION', 'ACTION', 'VIOLATION.CODE', 
                                             'CRITICAL.FLAG', 'SCORE', 'GRADE', 'INSPECTION.TYPE'))) %>%
                        mutate(inspection_date = mdy(INSPECTION.DATE),
                               inspection_year = year(inspection_date)) %>%
                        select(-INSPECTION.DATE)




all_data = all_data %>% mutate(action = replace(action, action == "Establishment Closed by DOHMH.  Violations were cited in the following area(s) and those requiring immediate action were addressed.", 'closed'),
                               action = replace(action, action == 'Establishment re-closed by DOHMH', 're-closed'),
                               action = replace(action, action == 'Establishment re-opened by DOHMH', 're-opened'),
                               action = replace(action, action == 'No violations were recorded at the time of this inspection.', 'no violations'),
                               action = replace(action, action == 'Violations were cited in the following area(s).', 'violations'),
                               inspection_type = replace(inspection_type, inspection_type == 'Cycle Inspection / Compliance Inspection', 'cycle-compliance'),
                               inspection_type = replace(inspection_type, inspection_type == 'Cycle Inspection / Initial Inspection', 'cycle-initial'),
                               inspection_type = replace(inspection_type, inspection_type == 'Cycle Inspection / Re-inspection', 'cycle-re-inspection'),
                               inspection_type = replace(inspection_type, inspection_type == 'Cycle Inspection / Reopening Inspection', 'cycle-reopening'),
                               inspection_type = replace(inspection_type, inspection_type == 'Cycle Inspection / Second Compliance Inspection', 'cycle-second-compliance'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Non-operational) / Compliance Inspection', 'pre-permit-nonop-compliance'),
                               inspection_type = replace(inspection_type, inspection_type =='Pre-permit (Non-operational) / Initial Inspection', 'pre-permit-nonop-initial'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Non-operational) / Re-inspection', 'pre-permit-nonop-re-inspection'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Non-operational) / Second Compliance Inspection', ' pre-permit-nonop-second-compliance'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Operational) / Compliance Inspection', 'pre-permit-op-compliance'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Operational) / Initial Inspection', 'pre-permit-op-initial'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Operational) / Re-inspection', 'pre-permit-op-re-inspection'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Operational) / Reopening Inspection', 'pre-permit-op-reopening'),
                               inspection_type = replace(inspection_type, inspection_type == 'Pre-permit (Operational) / Second Compliance Inspection', 'pre-permit-op-second-compliance'))

# 3.
all_data = all_data %>% filter(!borough ==0) %>%
                        filter(!inspection_date== '01/01/1900') %>%
                        filter(!is.na(inspection_type)) %>%
                        filter(score > 0) %>%
                        filter(!is.na(score)) %>%
                        filter(!inspection_type == 'Calorie Posting / Re-inspection') %>%
                        filter(!inspection_type == 'Inter-Agency Task Force / Re-inspection') %>%
                        filter(!inspection_type == "Smoke-Free Air Act / Re-inspection") %>%
                        filter(!inspection_type == "Administrative Miscellaneous / Re-inspection") %>%
                        filter(!inspection_type ==  "Trans Fat / Re-inspection") %>%
                        filter(!inspection_type == "Inter-Agency Task Force / Initial Inspection")


all_data = all_data %>% group_by(inspection_date, id) %>% mutate(score = replace(score, 1:n(), max(score))) %>% ungroup()
                        
                       
# 4.
restaurant_data = all_data %>% filter(inspection_year== 2017 | inspection_year==2018 | inspection_year==2019) %>%
                               filter(inspection_type=='cycle-initial') %>%
                               group_by(id, inspection_date) %>%
                               slice(1) %>%
                               mutate(outcome = ifelse(score>=28,TRUE, FALSE)) %>%
                               select(id, inspection_date, borough, cuisine, inspection_year, outcome)

restaurant_data = restaurant_data %>% mutate(weekday = wday(inspection_date),
                                             month = month(inspection_date))
# 5.
restaurant_history = all_data %>% select(id, action, score, inspection_date) %>% distinct()

# Here we create three categories (low, medium, high) of the inspection's outcome based on the score range: less than 14, between 14 and 28, 
# and above 28
my_table = restaurant_data %>% left_join(restaurant_history, by = 'id') %>%
                                      filter(inspection_date.x > inspection_date.y) %>%
                                      group_by(id, inspection_date.x) %>%
                                      mutate(num_previous_low_inspections = sum(score<14),
                                             num_previous_med_inspections = sum(score>=14 & score<28),
                                             num_previous_high_inspections = sum(score>28),
                                             num_previous_closings = sum(action=='closed' | action=='re-closed')) %>%
                                      rename(inspection_date = inspection_date.x) %>%
                                      distinct()

# left join with my_table and drop those extra columns that we don't need to use so that each 
# row represents the unique inspection with no duplicate 
restaurant_data = restaurant_data %>% left_join(my_table, by = c('id', 'inspection_date')) %>% select(-c('inspection_date.y.y', 'score', 'action', 'borough.y',
                                                                                                         'cuisine.y', 'inspection_year.y','outcome.y',
                                                                                                         'weekday.y','month.y')) %>% distinct()
# replace all na value with NA in the four columns that I just create, then rename the weekday and month
my_table = restaurant_data %>% replace(is.na(.), 0) %>% rename(weekday = weekday.x,
                                                               month = month.x)
# Part B: Modeling------------------------------------------------------------------------------------------------------------------------------------------------
# factor the variables
my_table$outcome.x = as.factor(my_table$outcome.x)
my_table$weekday = as.factor(my_table$weekday)
my_table$month = as.factor(my_table$month)

# split the data into training and testing set 
train = my_table %>% filter(inspection_year.x == 2017 | inspection_year.x ==2018)
test = my_table %>% filter(inspection_year.x == 2019)

# build a logistic regression and make prediction 
fit1 = glm(outcome.x ~ cuisine.x + borough.x + month + weekday, data = train, family = 'binomial')
test$predicted.prob_glm = predict(fit1, test, type = 'response')
test.pred <- prediction(test$predicted.prob_glm, test$outcome.x)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")


# 2. Random forest and prediction
fit2 = ranger(outcome.x ~ cuisine.x + borough.x + month + weekday + num_previous_low_inspections
              + num_previous_med_inspections + num_previous_high_inspections + num_previous_closings,
              respect.unordered.factors = TRUE, probability =  TRUE, num.trees = 100, data = train)
test$predicted.prob_rf = predict(fit2, test)$predictions[,2]
test.pred <- prediction(test$predicted.prob_rf, test$outcome.x)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")


# 3.
# recall-k% plot for logistic regression
test$outcome.x = as.logical(test$outcome.x)
plot.data1 = test %>% ungroup() %>% arrange(desc(predicted.prob_glm)) %>%
                      mutate(num_restaurant = row_number(), precisions = cumsum(outcome.x)/row_number()) %>%
                      select(num_restaurant, precisions) %>%
                      slice(100:n())

# recall-K% for random forest 
plot.data2 = test %>% ungroup() %>% arrange(desc(predicted.prob_rf)) %>%
  mutate(num_restaurant_1 = row_number(), precisions_1 = cumsum(outcome.x)/row_number()) %>%
  select(num_restaurant_1, precisions_1) %>%
  slice(100:n())

p = ggplot() +
  geom_line(data = plot.data1, aes(x = num_restaurant, y = precisions), color = 'red') +
  geom_line(data = plot.data2, aes(x = num_restaurant_1, y = precisions_1), color = 'blue') +
  xlab('restaurant')+
  ylab('precision')
p


# Based on the recall-K% visualization, random forest model has a better predictability 
