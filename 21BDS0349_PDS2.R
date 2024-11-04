#Khyati Khurana – 21BDS0349
# Load the dataset
imdb_data <- read.csv("C:/Users/HP/Downloads/IMDB Dataset.csv/IMDB Dataset.csv", stringsAsFactors = FALSE)

# Set a seed for reproducibility
set.seed(123)

# Sample 500 rows from the dataset
sampled_data <- imdb_data[sample(nrow(imdb_data), 250), ]

# Install required packages if not already installed
if (!require(tm)) install.packages("tm")
if (!require(textclean)) install.packages("textclean")
if (!require(dplyr)) install.packages("dplyr")
if (!require(text2vec)) install.packages("text2vec")
if (!require(caTools)) install.packages("caTools")
if (!require(caret)) install.packages("caret")

# Load libraries
library(tm)
library(textclean)
library(dplyr)
library(text2vec)
library(caTools)
library(caret)

# Convert text to lowercase
sampled_data$review <- tolower(sampled_data$review)

# Remove punctuation, numbers, and stop words
sampled_data$review <- removePunctuation(sampled_data$review)
sampled_data$review <- removeNumbers(sampled_data$review)
stopwords <- stopwords("en")
sampled_data$review <- removeWords(sampled_data$review, stopwords)

# Convert sentiment to binary: positive = 1, negative = 0
sampled_data$sentiment <- ifelse(sampled_data$sentiment == "positive", 1, 0)

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(sampled_data$sentiment, SplitRatio = 0.8)
train_data <- subset(sampled_data, split == TRUE)
test_data <- subset(sampled_data, split == FALSE)

# Tokenize the training data
train_tokens <- word_tokenizer(train_data$review)
train_it <- itoken(train_tokens)

# Create vocabulary and vectorizer from training data only
vocab <- create_vocabulary(train_it)
vectorizer <- vocab_vectorizer(vocab)

# Create DTM for training data
dtm_train <- create_dtm(train_it, vectorizer)

# Tokenize the test data
test_tokens <- word_tokenizer(test_data$review)
test_it <- itoken(test_tokens)

# Create DTM for test data using the same vectorizer
dtm_test <- create_dtm(test_it, vectorizer)

# Train the logistic regression model on the training DTM and sentiment
train_data_matrix <- as.data.frame(as.matrix(dtm_train))
train_data_matrix$sentiment <- train_data$sentiment
model <- glm(sentiment ~ ., data = train_data_matrix, family = "binomial")

# Predict probabilities on the test DTM
test_data_matrix <- as.data.frame(as.matrix(dtm_test))
test_predictions <- predict(model, newdata = test_data_matrix, type = "response")

# Convert probabilities to binary classes
predicted_sentiment <- ifelse(test_predictions > 0.5, 1, 0)

# Calculate accuracy using a confusion matrix
confusionMatrix(as.factor(predicted_sentiment), as.factor(test_data$sentiment))
