install.packages("foreign")
install.packages("reshape2")
install.packages("psych")
library(psych)
library(reshape2)
library(foreign)
library(ggplot2)

url <- "https://drive.google.com/uc?id=1ZEcLOrwMWKflMhXjG-m2sQsLVIKSxtwb"
output_file <- "dataset.arff"
download.file(url, destfile = output_file, mode = "wb")
data <- read.arff(output_file)

head(data)

str(data)

data$jundice <- ifelse(data$jundice == "yes", 1, 0)
data$austim <- ifelse(data$austim == "yes", 1, 0)
data$gender <- ifelse(data$gender == "m", 1, 0)
data$used_app_before <- ifelse(data$used_app_before == "yes", 1, 0)
data$`Class/ASD` <- ifelse(data$`Class/ASD` == "YES", 1, 0)

A_columns <- paste0("A", 1:10, "_Score")
data[A_columns] <- lapply(data[A_columns], as.numeric)

data <- data[!is.na(data$ethnicity) & !is.na(data$relation), ]

data <- cbind(data, model.matrix(~ ethnicity - 1, data = data))
data <- cbind(data, model.matrix(~ relation - 1, data = data))
data$ethnicity <- NULL
data$relation <- NULL

data <- data[!is.na(data$age), ]

data <- data[!is.na(data$age), ]

data <- data[!duplicated(data), ]

column_info <- sapply(data, class)
print(column_info)

numeric_columns <- names(column_info[column_info == "numeric"])
categorical_columns <- names(column_info[column_info != "numeric"])
print("Numeric Columns:")
print(numeric_columns)
print("Categorical Columns:")
print(categorical_columns)

numeric_data <- data[sapply(data, is.numeric)]
numeric_data <- numeric_data[, sapply(numeric_data, function(x) var(x, na.rm = TRUE) > 0)]

correlation_matrix <- cor(numeric_data, use = "complete.obs")
correlations_with_target <- correlation_matrix[,"Class/ASD"]
correlation_df <- data.frame(Variable = names(correlations_with_target),
                              Correlation = correlations_with_target)
correlation_df <- correlation_df[order(abs(correlation_df$Correlation), decreasing = TRUE), ]

ggplot(correlation_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Correlation with Class/ASD", x = "Variables", y = "Correlation Coefficient") +
  theme_minimal()

numeric_data <- data[sapply(data, is.numeric)]
numeric_data <- numeric_data[, sapply(numeric_data, function(x) var(x, na.rm = TRUE) > 0)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1),
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Correlation Matrix", x = "Variables", y = "Variables")

summary(data)

#describe(data)

averages <- colMeans(data[sapply(data, is.numeric)], na.rm = TRUE)
average_df <- data.frame(Variable = names(averages), Average = averages)

ggplot(average_df, aes(x = reorder(Variable, Average), y = Average)) +
  geom_bar(stat = "identity", fill = "cyan") +
  coord_flip() +
  labs(title = "Average of Numeric Variables", x = "Variables", y = "Average") +
  theme_minimal()