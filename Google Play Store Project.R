# Load required libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("multcomp")
library(dplyr)
library(ggplot2)
library(car)

# Read the dataset
data <- read.csv("googleplaystore.csv")  

# Data Cleaning
# Removing unnecessary columns
columns_to_remove <- c("Size", "Last.Updated", "Current.Ver", "Android.Ver", "Genre")
data <- data[, !(names(data) %in% columns_to_remove)]

### Remove rows with NaN values in any column
data <- na.omit(data)

### Clean the 'Installs' variable
unique_values <- unique(data$Installs) #check value
print(unique_values) #invalid value "Free"
data$Installs <- gsub(",", "", data$Installs)  # Remove commas
data$Installs <- gsub("\\+", "", data$Installs)  # Remove plus signs  
data <- subset(data, Installs != "Free") # Remove 'Free' from Installs

### Clean the "Type" variable
unique_values0 <- unique(data$Type) #check value
print(unique_values0) #"Free" "Paid" "0" 
data$Type[data$Type == "0"] <- "Free" # Replace "0" with "Free"
unique_values0 <- unique(data$Type)
print(unique_values0) # Check the unique values after cleaning

### Clean the "Price" variable
unique_values1 <- unique(data$Price)
print(unique_values1)
data$Price <- gsub("\\$", "", data$Price)
data$Price <- ifelse(data$Price == "0", "0.00", data$Price) #Replace all "0" values with "0.00"
data$Price <- as.numeric(data$Price) #convert "Price" to numeric
unique_values1 <- unique(data$Price)
print(unique_values1)

### Clean the "Category" variable
data <- subset(data, Category != "1.9")

### Convert to numeric format
data$Installs <- as.numeric(data$Installs)  
data$Rating <- as.numeric(data$Rating)  
data$Reviews <- as.numeric(data$Reviews)
data$Price <- as.numeric(data$Price)

### Convert to factor format
data$Category <- as.factor(data$Category) 
data$Type <- as.factor(data$Type) 
data$Content.Rating <- as.factor(data$Content.Rating) 


# Data exploring
### Export to csv the cleaned ver
write.csv(data, file = "PlaystoreApp_cleaned.csv")

# Explore the dataset
str(data)
summary(data)

### Rating distribution
ggplot(data, aes(x = Rating, y = Installs)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  xlab('Rating') +
  ylab('Installs') +
  ggtitle('Installs by Rating')

### Type distribution
ggplot(data, aes(x = Type, y = Installs)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  xlab('Type') +
  ylab('Installs') +
  ggtitle('Installs by Type')

### Content.Rating distribution
ggplot(data, aes(x = Content.Rating, y = Installs)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  xlab('Content.Rating') +
  ylab('Installs') +
  ggtitle('Installs by Content.Rating')


# Q1: Factors impact user downloads in the Google Play Store
### Linear Regression Analysis
model <- lm(Installs ~ Category + Rating + Reviews + Type + Content.Rating, data = data)
summary(model)


# Q2: Does the pricing strategy of apps in the Google Play Store influence their user downloads
### Two Sample t-test between free/paid app and user downloads
ttest_result <- t.test(Installs ~ Type, data = data)
ttest_result

### Subset dataset for paid apps only
filtered_data <- data[data$Price != 0, ] # Create a new data frame with Price exclude free app
filtered_data$Price <- as.numeric(filtered_data$Price)
print(filtered_data$Price)
summary(filtered_data$Price)

### Create price ranges for paid apps
price_ranges <- cut(filtered_data$Price, breaks = c(0.00, 1.99, 2.99, 4.99, 13.91, 400), labels = c("0.99-1.99", "1.99-2.99", "2.99-4.99", "4.99-13.91", "Above average")) 
filtered_data$price_ranges <- price_ranges
print(price_ranges)

### Fit in linear regression between user downloads and pricing range among paid apps
model1 <- lm(Installs ~ price_ranges, data = filtered_data)
summary(model1)

### Visualization price_ranges vs user downloads
ggplot(filtered_data, aes(x = price_ranges, y = Installs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Price Range", y = "User Downloads") +
  ggtitle("User Downloads vs. Price Range for Paid Apps")


# Q3: Is there a significant association between the most popular category’s app download and Rating, Reviews, and Type?
### Identify the most popular app category based on the three criterias
#### Highest mean installs
highest_avg_installs <- data %>% group_by(Category) %>% summarize(avg_installs = mean(Installs)) %>% arrange(desc(avg_installs)) %>% head(1)
#### Highest mean reviews
highest_avg_reviews <- data %>% group_by(Category) %>% summarize(avg_reviews = mean(Reviews)) %>% arrange(desc(avg_reviews)) %>% head(1)
#### Highest mean ratings
highest_avg_ratings <- data %>% group_by(Category) %>% summarize(avg_ratings = mean(Rating)) %>% arrange(desc(avg_ratings)) %>% head(1)

#### Print value
print(highest_avg_installs)
print(highest_avg_reviews) 
print(highest_avg_ratings) 

#### Get unique category values
unique_categories <- unique(data$Category)
print(unique_categories)

### Subset the dataset to include communication apps only
communication_apps <- data[data$Category == "COMMUNICATION", ]

### Fit the multiple linear regression model
model2 <- lm(Installs ~ Rating + Reviews + Type, data = communication_apps)
summary(model2)


# Sub-question: Further analyze Rating
summary(data$Rating)

#Create rating ranges
rating_ranges <- cut(data$Rating, breaks = c(0, 4, 4.192, 4.3, 4.5, 5), labels = c("1-4", "4-4.192", "4.192-4.3", "4.3-4.5", "4.5-5")) 
data$rating_ranges <- rating_ranges

### Fit in linear regression between user downloads and rating range 
model3 <- lm(Installs ~ rating_ranges, data = data)
summary(model3)

### Visualization rating_ranges vs user downloads
ggplot(data, aes(x = rating_ranges, y = Installs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Rating Range", y = "User Downloads") +
  ggtitle("User Downloads vs. Rating Range")

### create APA table
install.packages("apaTables")
library(apaTables)
apa.reg.table(lm(Installs ~ Category + Rating + Reviews + Type + Content.Rating, data = data),filename = "Tab1",table.number = 1)
apa.reg.table(lm(Installs ~ price_ranges, data = filtered_data),filename = "Tab3",table.number = 3)
apa.reg.table(lm(Installs ~ Rating + Reviews + Type, data = communication_apps),filename = "Tab4",table.number = 4)
apa.reg.table(lm(Installs ~ rating_ranges, data = data),filename = "Tab5",table.number = 5)




