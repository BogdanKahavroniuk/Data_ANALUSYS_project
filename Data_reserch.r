library(dplyr)
library(tidyverse)
library(ggplot2)

df   <- read.csv("Data\\athlete_events.csv")

#очистка даних


# зміна типу колонок до потрібних 
df$Sex  <-  factor(df$Sex) 
df$ID  <-  as.character(df$ID) 


null_counts <- rowSums(is.na(df))

# delete rows whitch have to many skipped values 
filtered_df <- df[null_counts <= 3, ]

row_count <- nrow(df)

row_count1 <- nrow(filtered_df)

df <- filtered_df
# Print the number of rows in filtered and not filtered df  
print("кількысть рядків до очистки і після")
print(row_count)
print(row_count1)
# в датасеті немає викидів бо розподіл  прямує до нормального і змінні фактчно обмежені людсьькими характеристиками

column_names <- colnames(df)
column_types <- sapply(df, class)

# Combine column names and data types
result <- data.frame(Column = column_names, DataType = column_types)

# Print the result
print(result)


numeric_cols <- sapply(df, is.numeric)

# Compute summary statistics for numeric columns
summary_stats <- summary(df[, numeric_cols])

# Calculate additional statistics

mean_stats <- sapply(df[, numeric_cols], mean, na.rm = TRUE)
sd_stats <- sapply(df[, numeric_cols], sd, na.rm = TRUE)
min_stats <- sapply(df[, numeric_cols], min, na.rm = TRUE)
max_stats <- sapply(df[, numeric_cols], max, na.rm = TRUE)
quantile_stats <- t(sapply(df[, numeric_cols], quantile, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

# Combine statistics into a data frame
describe_df <- data.frame(

  Mean = mean_stats,
  StandardDeviation = sd_stats,
  Minimum = min_stats,
  Q1 = quantile_stats[, 2],
  Median = quantile_stats[, 3],
  Q3 = quantile_stats[, 4],
  Maximum = max_stats
)


print(describe_df)

sport_players <- df %>%
  group_by(Sport) %>%
  summarise(n_unique_players = n_distinct(Name))


wc_count <- setNames(sport_players$n_unique_players, sport_players$Sport)

top_10_sports <- sport_players %>%
  arrange(desc(n_unique_players)) %>%
  head(10)

# Calculate the color scale using logarithm base 10
color_scale <- log10(1 + top_10_sports$n_unique_players)

# Create the bar plot
plot <- ggplot(top_10_sports, aes(x = Sport, y = n_unique_players, fill = color_scale)) +
  geom_bar(stat = "identity") +
  labs(x = "Sport", y = "Nº Unique Athletes", title = "Top 10 Sports by Number of Athletes") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"), legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# Save the plot to a file (e.g., PNG format)
print(plot)