# Step 1: Load the Dataset and Sample Data
imdb_data <- read.csv("C:/Users/HP/Downloads/IMDB Dataset.csv/IMDB Dataset.csv", stringsAsFactors = FALSE)
set.seed(123)
sampled_data <- imdb_data[sample(nrow(imdb_data), 100), ]  # Use a smaller subset to reduce memory usage

# Step 2: Install and Load Required Packages
if (!require(tm)) install.packages("tm")
if (!require(textclean)) install.packages("textclean")
if (!require(dplyr)) install.packages("dplyr")
if (!require(text2vec)) install.packages("text2vec")
if (!require(caTools)) install.packages("caTools")
if (!require(caret)) install.packages("caret")
library(tm); library(textclean); library(dplyr); library(text2vec); library(caTools); library(caret)

# Step 3: Preprocess the Text Data
sampled_data$review <- tolower(sampled_data$review)
sampled_data$review <- removePunctuation(sampled_data$review)
sampled_data$review <- removeNumbers(sampled_data$review)
stopwords <- stopwords("en")
sampled_data$review <- removeWords(sampled_data$review, stopwords)

# Step 4: Convert Sentiment to Binary Labels
sampled_data$sentiment <- ifelse(sampled_data$sentiment == "positive", 1, 0)

# Step 5: Split the Data into Training and Testing Sets
set.seed(123)
split <- sample.split(sampled_data$sentiment, SplitRatio = 0.8)
train_data <- subset(sampled_data, split == TRUE)
test_data <- subset(sampled_data, split == FALSE)

# Step 6: Tokenize and Vectorize Text for the Training Data
train_tokens <- word_tokenizer(train_data$review)
train_it <- itoken(train_tokens)
vocab <- create_vocabulary(train_it)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(train_it, vectorizer)

# Step 7: Create DTM for the Testing Data
test_tokens <- word_tokenizer(test_data$review)
test_it <- itoken(test_tokens)
dtm_test <- create_dtm(test_it, vectorizer)

# Step 8: Train the Model and Evaluate Performance
train_data_matrix <- as.data.frame(as.matrix(dtm_train))
train_data_matrix$sentiment <- train_data$sentiment
model <- glm(sentiment ~ ., data = train_data_matrix, family = "binomial")

# Make Predictions
test_data_matrix <- as.data.frame(as.matrix(dtm_test))
test_predictions <- predict(model, newdata = test_data_matrix, type = "response")
predicted_sentiment <- ifelse(test_predictions > 0.5, 1, 0)

# Confusion Matrix and Metrics
conf_matrix <- table(Predicted = predicted_sentiment, Actual = test_data$sentiment)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print Results
cat("Confusion Matrix:\n")
print(conf_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Precision:", round(precision * 100, 2), "%\n")
cat("Recall:", round(recall * 100, 2), "%\n")
cat("F1 Score:", round(f1_score * 100, 2), "%\n")

