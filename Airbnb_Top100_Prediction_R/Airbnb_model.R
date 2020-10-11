# Data Preprocessing
#install.packages('DataExplorer')
#install.packages('DT')
#install.packages("leaflet")
library(dplyr)
library(tidyr)
library(DataExplorer)
library(DT)
library(stringr)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(caret)
# Importing the dataset
dataset = read.csv('/Users/sakanashishunsuke/OneDrive - Asia Pacific University/AML/dataset/listings_test.csv')
#check number of rows and columns
dim(dataset)

#chack contents
#DT::datatable(dataset[1:5,], rownames = FALSE)

#missing values or null
plot_missing(dataset[1:20]) #name,summary,space,description,neighborhood_overview,notes,transit,
                            #host_about,scrape_id,last_scraped,experiences_offered?,
plot_missing(dataset[21:40]) #neighbourhood_group_cleansed,security_deposit?, cleaning_fee?
plot_missing(dataset[41:60]) #square_feet,
plot_missing(dataset[61:84]) #license
# head(dataset$notes,50)
# head(dataset$transit,50)

# summary(dataset$security_deposit == '') # true = 2297 -> security_deposit
# 2297/3723 * 100 # 61.69%
# summary(dataset$cleaning_fee == '') # true = 1388 -> cleaning_fee
# 1388/3723 * 100 # 37.28%
# summary(dataset$weekly_price == '') # true = 1599 -> weekly_price
# 1599/3723 * 100 # 42.94%
# summary(dataset$monthly_price == '') # true = 1916 -> monthly_price
# 1916/3723 * 100 # 51.46%
#summary(dataset$requires_license) # f = 3723 -> all is same value(f) -> requires_license
# dataset %>% group_by(experiences_offered) %>% summarize(count=n()) none = 3723 -> all is same values(none) -> experiences_offered

#dataset %>% group_by(host_acceptance_rate) %>% summarize(count=n())
# summary(dataset$host_verifications) # method-> use categorical data in host_identity_verified

# host_listings_count & host_total_listings_count -> calculated_host_listings

#select state
dataset %>% group_by(state) %>% summarize(count=n())
dataset$state[dataset$state == 'Washington DC'] <- 'DC'
dataset %>% group_by(state) %>% summarize(count=n())
dataset <- dataset[!(dataset$state == 'MD' | dataset$state == 'NY' | dataset$state == 'VA'),]
dataset %>% group_by(state) %>% summarize(count=n())

#city, country, country_code, market, smart_location -> we know target area thus not important
#head(dataset[c('city', 'country', 'country_code', 'market', 'smart_location')], 50)

#availability_30, availability_60, availability_90, availability_365 -> drop
#calender_updated, calender_last_scraped,has_availability -> drop
#host_id -> drop
#street -> use neighbourhood_cleansed
#host_response_rate, host_response_time -> drop
#host_neighbourhood & host_verifications -> host_location,  

#review_scores_rating -> average -> use each reviews rating
#is_location_exact -> not important -> drop
#maximum_nights -> most 1 less than 30 days 
#boxplot(dataset$maximum_nights)
#neighbourhood -> use neighborhood_cleansed ->  
#drop guests_included, use accommodates
#address NA
dataset$beds[is.na(dataset$beds)] <- 1
dataset$bathrooms[is.na(dataset$bathrooms)] <- 1
dataset$bedrooms[is.na(dataset$bedrooms)] <- 1
dataset$review_scores_accuracy[is.na(dataset$review_scores_accuracy)] <- 9
dataset$review_scores_cleanliness[is.na(dataset$review_scores_cleanliness)] <- 9
dataset$review_scores_checkin[is.na(dataset$review_scores_checkin)] <- 9
dataset$review_scores_communication[is.na(dataset$review_scores_communication)] <- 9
dataset$review_scores_location[is.na(dataset$review_scores_location)] <- 9
dataset$review_scores_value[is.na(dataset$review_scores_value)] <- 9
dataset <- subset(dataset, rowSums(is.na(dataset))!=0)
dim(dataset)
#dataset %>% group_by(review_scores_value) %>% summarize(count=n())


df <- dataset[c('id', 'host_name', 'host_since', 'host_location', 
                'host_is_superhost', 'host_has_profile_pic', 'host_identity_verified',
                'neighbourhood_cleansed', 'state', 'zipcode', 'latitude', 'longitude',
                'property_type', 'room_type', 'accommodates', 'bathrooms', 'bedrooms',
                'beds', 'bed_type', 'amenities', 'price', 'extra_people',
                'minimum_nights', 'number_of_reviews', 'first_review', 'last_review',
                'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                'review_scores_communication', 'review_scores_location', 'review_scores_value',
                'instant_bookable', 'cancellation_policy', 'require_guest_profile_picture',
                'require_guest_phone_verification', 'calculated_host_listings_count',
                'reviews_per_month')]

dim(df)
plot_missing(df)
boxplot(df$number_of_reviews)
plot_histogram(df$number_of_reviews)
summary(df)
#drop the listings that have noting of reviews.
df <- df[!(df$number_of_reviews == 0),]
dim(df)
plot_missing(df[1:20])
plot_missing(df[21:38])

#convert t/f to 1/0
df$host_is_superhost <- as.numeric(ifelse(df$host_is_superhost == 't', 1, 0))
df$host_has_profile_pic <- as.numeric(ifelse(df$host_has_profile_pic == 't', 1, 0))
df$host_identity_verified <- as.numeric(ifelse(df$host_identity_verified == 't', 1, 0))
df$instant_bookable <- as.numeric(ifelse(df$instant_bookable == 't', 1, 0))
df$require_guest_profile_picture <- as.numeric(ifelse(df$require_guest_profile_picture == 't', 1, 0))
df$require_guest_phone_verification <- as.numeric(ifelse(df$require_guest_phone_verification == 't', 1, 0))

#price, extra_people covert char to num
df$price <- df$price %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
df$extra_people <- df$extra_people %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()

#feature engineering part1
df  <- df %>%
  mutate(listing_duration = as.numeric(difftime(df$last_review, df$first_review, units = 'days')),
         hosting_duration = as.numeric(difftime(df$last_review, df$host_since, units = 'days')),
         host_local = as.numeric(str_detect(host_location, 'Washington')),
         total_amenities = ifelse(str_count(amenities) > 2 , str_count(amenities, ',') + 1,0),
         price_per_person = price / accommodates)

#top 100 feature engineering part2
df$top_100 <- ifelse(rank(-df$number_of_reviews) <= 100, 1, 0)

#replace non-alpanumeric characters -> cancellation_policy not need to change
df %>% group_by(property_type) %>% summarize(count=n())
df$property_type <- str_replace_all(df$property_type, "&", "and")
df %>% group_by(property_type) %>% summarize(count=n())
df$property_type <- str_replace_all(df$property_type, "[^[:alnum:]]", "_")
df %>% group_by(property_type) %>% summarize(count=n())

df %>% group_by(room_type) %>% summarize(count=n())
df$room_type <- str_replace_all(df$room_type, "[^[:alnum:]]", "_")
df %>% group_by(room_type) %>% summarize(count=n())

df %>% group_by(bed_type) %>% summarize(count=n())
df$bed_type <- str_replace_all(df$bed_type, "[^[:alnum:]]", "_")
df %>% group_by(bed_type) %>% summarize(count=n())

dim(df)
#DISCRETE
discrete <- c("host_is_superhost", "host_has_profile_pic", "host_identity_verified",
              "instant_bookable", "require_guest_profile_picture", "require_guest_phone_verification",
              "host_local")


for (colname in discrete) { 
  
  temp <- subset(df, top_100 == 1) 
  temp <- temp %>% 
    group_by(top_100, temp[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp)[2] <- colname
  
  temp1 <- subset(df, top_100 == 0) 
  temp1 <- temp1 %>% 
    group_by(top_100, temp1[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp1)[2] <- colname
  
  temp2 <- rbind(temp, temp1)
  
  plot <- ggplot(data=temp2, aes(x=as.factor(temp2[[colname]]), y=density, fill=as.factor(top_100))) + 
    geom_bar(position = 'dodge', stat='identity') + labs(fill = "top_100", x = colname, 
                                                         title = paste(colname, " relative density grouped by top_100")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(plot)
}


#categorical
categorical <- c("neighbourhood_cleansed", "property_type", "room_type", "bed_type", "cancellation_policy")

for (colname in categorical) {
  
  
  temp <- subset(df, top_100 == 1) 
  temp <- temp %>% 
    group_by(top_100, temp[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp)[2] <- colname
  
  temp1 <- subset(df, top_100 == 0) 
  temp1 <- temp1 %>% 
    group_by(top_100, temp1[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp1)[2] <- colname
  
  temp2 <- rbind(temp, temp1)
  
  plot <- ggplot(data=temp2, aes(x=temp2[[colname]], y=density, fill=as.factor(top_100))) + 
    geom_bar(position = 'dodge', stat='identity') + labs(fill = "top_100", x = colname, 
                                                         title = paste(colname, " relative density grouped by top_100")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(plot)
}

#continuous
continuous <- c("bathrooms", "bedrooms", "beds", "price_per_person",
               "extra_people", "minimum_nights",
               "calculated_host_listings_count", "listing_duration",
               "hosting_duration","total_amenities", 'price_per_person',
               "review_scores_accuracy", "review_scores_cleanliness", 
               "review_scores_checkin", "review_scores_communication", 
               "review_scores_location", "review_scores_value")

for (colname in continuous) {
  
  plot <- ggplot(data=df, aes(x=as.factor(top_100), y=df[[colname]])) + 
    geom_boxplot(fill="lightblue") + labs(x = "top_100", y = colname,
                                          title = paste(colname, " grouped by top_100")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_y_continuous(limits = quantile(df[[colname]], c(0.1, 0.9)))
  
  print(plot)
}

#NEIGHBOURHOODS GEOSPATIAL DATA
# nb_geo <- geojson_read('C:/Airbnb/neighbourhoods.geojson', what = 'sp')
# 
# other <- df %>% 
#   filter(top_100 == 0)
# 
# is_top_100 <- df %>% 
#   filter(top_100 == 1) 
# 
# leaflet() %>% setView(lng = 2.154007, lat = 41.390205, zoom = 12) %>%
#   addTiles() %>%
#   addPolygons(data = nb_geo, color = "#444444", weight = 2, opacity = 1) %>%
#   addCircleMarkers(  lng = other$longitude, 
#                      lat = other$latitude,
#                      radius = 2, 
#                      stroke = FALSE,
#                      color = "blue",
#                      fillOpacity = 0.5, 
#                      group = "Other"
#   ) %>%
#   addCircleMarkers(  lng = is_top_100$longitude, 
#                      lat = is_top_100$latitude,
#                      radius = 3, 
#                      stroke = FALSE,
#                      color = "red",
#                      fillOpacity = 0.9, 
#                      group = "Top 100"
#   )

#We will now identify and remove near-zero variance predictors using the follwoing code. 
#This data will be then used in our predictive model.
df_num <- df[, c(5:7, 15:18, 21:24, 27:33, 35:44)]
summary(df_num)
dim(df_num)
nzv <- nearZeroVar(select(df_num, -top_100))
df_nzv <- df_num[, -nzv]
dim(df_nzv)
summary(df_nzv)

#check high correlated variables 
descrCor <- cor(df_nzv)
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)
highlyCorCol <- colnames(df_nzv)[highlyCorrelated]
highlyCorCol

#drop high correlated variables (accommodates, number_of_reviews)
df_nzv_uncor <- 
  df_nzv[, -which(colnames(df_nzv) %in% highlyCorCol)]

dim(df_nzv_uncor)

#naive-bayes
library(RCurl)
library(e1071)
#install.packages("performance")
#install.packages("prediction")
library(performance)
library(prediction)

set.seed(132)
nb_sub <- sample(nrow(df_nzv_uncor), floor(nrow(df_nzv_uncor) * 0.6))
nb_train <- df_nzv_uncor[nb_sub, ]
nb_test <- df_nzv_uncor[-nb_sub, ]
nb <- naiveBayes(as.factor(top_100) ~ ., data = nb_train)
nb_prediction <- predict(nb, nb_test)
nb_conf <- table(nb_test$top_100, nb_prediction)
print(nb_conf)

nb_accuracy <- sum(diag(nb_conf))/sum(nb_conf)
print(nb_accuracy)

nb_precision <- nb_conf[2,2] / (nb_conf[2,2] + nb_conf[2,1])
print(nb_precision)

nb_recall <- nb_conf[2,2] / (nb_conf[2,2] + nb_conf[1,2])
print(nb_recall)


nb_roc <- performance(prediction(as.numeric(nb_prediction), as.numeric(nb_test$top_100)), "tpr", "fpr")
plot(nb_roc, colorize=TRUE)
abline(0, 1, lty = 2)

#decison tree
library(rpart)
library(rpart.plot)
# install.packages("rattle")
# install.packages("partykit")
library(rattle)					# Fancy tree plot
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree

##########################
library(formattable)
formattable(df_nzv_uncor)
summary(df_nzv_uncor)
# Proportions of the class values
prop.table(table(df_nzv_uncor$top_100)) 
table(df_nzv_uncor$top_100) 

# Dividing the dataset into training and validation sets. There are many ways to do this.
# Alternate method is also listed here.

set.seed(123)
ind <- sample(2, nrow(df_nzv_uncor), replace=TRUE, prob=c(0.7, 0.3))
train_Data <- df_nzv_uncor[ind==1,]
validation_Data <- df_nzv_uncor[ind==2,]
table(train_Data$top_100)
table(validation_Data$top_100)

# Proportions of the class values
prop.table(table(train_Data$top_100)) 


#Create training and testing sets
tree = rpart(top_100~ host_is_superhost +
               bathrooms + extra_people +
               calculated_host_listings_count + hosting_duration +
               host_local + total_amenities + price_per_person, data=train_Data)

# tree = rpart(top_100~ host_is_superhost +
#                bathrooms + bedrooms + beds + price + extra_people +
#                minimum_nights + review_scores_accuracy +
#                review_scores_cleanliness + review_scores_checkin +
#                review_scores_communication + review_scores_location +
#                review_scores_location + review_scores_value +
#                instant_bookable + require_guest_profile_picture +
#                require_guest_phone_verification + calculated_host_listings_count +
#                hosting_duration +
#                host_local + total_amenities + price_per_person, data=train_Data)
#tree = rpart(top_100~ ., data=train_Data)
print(tree)
prp(tree) # plot Rpart Model
prp (tree, type = 5, extra = 1) #extra = 1 [classification] n=5 [regression]
rpart.plot(tree, extra = 1, nn = TRUE)

# Split with entropy information
ent_Tree = rpart(top_100~ host_is_superhost +
                   bathrooms + extra_people +
                   calculated_host_listings_count + hosting_duration +
                   host_local + total_amenities + price_per_person, data=train_Data, method="class", parms=list(split="information"))
prp(tree)
prp(ent_Tree)

plotcp(tree)

# This code generates the tree with training data
#minsplit how many there are splits, minbucket how many there are 
tree_with_params = rpart(top_100~ host_is_superhost +
                           bathrooms +
                           calculated_host_listings_count + hosting_duration +
                           host_local + total_amenities + price_per_person, data=train_Data, method="class", minsplit = 8, minbucket = 9, cp = -1)
prp(tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)
# Now we predict and evaluate the performance of the trained tree model 
Predict = predict(tree_with_params, validation_Data)
# Now examine the values of Predict. These are the class probabilities
Predict
# pred <= predict (mymodel, dataset, type = 'prob')
# To produce classes only, without the probabilities, run the next command.
# By default threshold is set at 0.5 to produce the classes
Predict = predict(tree_with_params, validation_Data, type = "class")
Predict

# Producing confusion matrix
Confusion_matrix = table(Predict, validation_Data$top_100)
print(Confusion_matrix)

# ROC curve
library(ROCR)

# To draw ROC we need to predict the prob values. So we run predict again
# Note that PredictROC is same as Predict with "type = prob"

Predict_ROC = predict(tree_with_params, validation_Data)
Predict_ROC
Predict_ROC[,2]

pred = prediction(Predict_ROC[,2], validation_Data$top_100)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc
###############################


set.seed(132)
df_nzv1 <- df_nzv_uncor
dt_sub <- sample(nrow(df_nzv1), floor(nrow(df_nzv1) * 0.6))
dt_train <- df_nzv1[dt_sub, ]
dt_test <- df_nzv1[-dt_sub, ]

dt_model <- rpart(top_100~ ., data = dt_train, method = "class", control = rpart.control(cp = 0.01, minbucket = 5))
fancyRpartPlot(dt_model, caption = "")

dt_prediction <- predict(dt_model, dt_test, type = "class")
dt_pred <- prediction(predict(dt_model, type = "prob")[, 2], dt_train$top_100)

dt_conf <- table(dt_test$top_100, dt_prediction)

print(dt_conf)
printcp(dt_model)
plotcp(dt_model)
dt_prediction <- predict(dt_model, dt_test, type = "class")
dt_pred <- prediction(predict(dt_model, type = "prob")[, 2], dt_train$top_100)

dt_conf <- table(dt_test$top_100, dt_prediction)

dt_accuracy <- sum(diag(dt_conf))/sum(dt_conf)

print(dt_accuracy)

dt_precision <- dt_conf[2,2] / (dt_conf[2,2] + dt_conf[2,1])

print(dt_precision)

dt_recall <- dt_conf[2,2] / (dt_conf[2,2] + dt_conf[1,2])

print(dt_recall)

dt_roc <- performance(dt_pred, measure="tpr", x.measure="fpr")
plot(dt_roc, colorize=TRUE)
abline(0, 1, lty = 2)


