library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(maps)
library(rworldmap)
library(RColorBrewer)
library(maptools)
library(viridis)
library(boot)


df   <- read.csv("Data\\athlete_events.csv")
head(df,n=10)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

# очистка даних з попередньої лаби 
df$Sex  <-  factor(df$Sex) 
df$ID  <-  as.character(df$ID) 
null_counts <- rowSums(is.na(df))
filtered_df <- df[null_counts <= 3, ]
row_count <- nrow(df)
row_count1 <- nrow(filtered_df)
df <- filtered_df
print("кількысть рядків до очистки і після")
print(row_count)
print(row_count1)
column_names <- colnames(df)
column_types <- sapply(df, class)

basketball_avg_height <- basketball_df %>%
  group_by(Year) %>%
  summarize(Average_Height = mean(Height, na.rm = TRUE),
            Standard_Deviation = sd(Height, na.rm = TRUE))

# Calculate average height and standard deviation for other sports players
other_avg_height <- df %>%
  filter(Sport != "Basketball") %>%
  group_by(Year) %>%
  summarize(Average_Height = mean(Height, na.rm = TRUE),
            Standard_Deviation = sd(Height, na.rm = TRUE))

plot <- ggplot() +
  geom_line(data = basketball_avg_height, aes(x = Year, y = Average_Height, color = "Basketball"), size = 1) +
  geom_ribbon(data = basketball_avg_height, aes(x = Year, ymin = Average_Height - Standard_Deviation , ymax = Average_Height + Standard_Deviation , fill = "Basketball"), alpha = 0.3) +
  
  # Plotting other sports average height with standard deviation
  geom_line(data = other_avg_height, aes(x = Year, y = Average_Height, color = "Other Sports"), size = 1) +
  geom_ribbon(data = other_avg_height, aes(x = Year, ymin = Average_Height - Standard_Deviation , ymax = Average_Height + Standard_Deviation , fill = "Other Sports"), alpha = 0.3) +
  
  # Customize the plot
  labs(x = "Year", y = "Average Height", color = "Sport") +
  scale_fill_manual(values = c("Basketball" = "blue", "Other Sports" = "lightgreen")) +
  scale_color_manual(values = c("Basketball" = "red", "Other Sports" = "green")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))


  
ggsave("average_height_plot.png", plot, width = 8, height = 6, dpi = 300 , , bg = "white")

basketball_avg_height <- mean(df$Height[df$Sport == "Basketball"], na.rm = TRUE)
basketball_ci <- t.test(df$Height[df$Sport == "Basketball"], conf.level = 0.95)$conf.int

# Calculate the average height and confidence interval for players in other sports
other_avg_height <- mean(df$Height[df$Sport != "Basketball"], na.rm = TRUE)
other_ci <- t.test(df$Height[df$Sport != "Basketball"], conf.level = 0.95)$conf.in



print("проведемо t - teсти ,  щоб знайти 95  відсоткові інтервали та середній зріст спортсменів за всі роки : ")
print("середній  зріст баскетболістів  = ")
print(basketball_avg_height)
print("95 відсотковий довірчийінтервал =  ")
print(basketball_ci)
print("середній  зріст всіх інших спортсменів   = ")
print(other_avg_height)
print("95 відсотковий довірчийінтервал =  ")
print(other_ci)

results <- data.frame(
  Group = c("Basketball", "Other Sports"),
  Average_Height = c(basketball_avg_height, other_avg_height),
  Lower_CI = c(basketball_ci[1], other_ci[1]),
  Upper_CI = c(basketball_ci[2], other_ci[2])
)

# Create the bar plot with error bars
plot <- ggplot(results, aes(x = Group,y = Average_Height)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  geom_point()

# Display the plot
ggsave("average_height_for_all_time.png", plot, width = 8, height = 6, dpi = 300 , , bg = "white")



#знайдемо медіани зросту для баскетболістів   та  для інших оскільки ми не знаємо чому дорівнює асимтотична дисперсія для медіани  знайдемо її за допомогою  будстрепу

get_median_sd <- function(data, index) {
  median_height <- median(data[index] , na.rm = TRUE)
  sd_height <- sd(data[index] ,  na.rm = TRUE )
  return(c(median_height, sd_height))
}

# Perform bootstrapping for basketball players
boot_basketball <- boot(df$Height[df$Sport == "Basketball"], get_median_sd, R = 1000)
basketball_median <- median(boot_basketball$t[, 1])
basketball_sd <- sd(boot_basketball$t[, 1])

# Perform bootstrapping for players in other sports
boot_others <- boot(df$Height[df$Sport != "Basketball"], get_median_sd, R = 1000)
others_median <- median(boot_others$t[, 1])
others_sd <- sd(boot_others$t[, 1])
print(others_sd)
# Print the results
cat("Basketball Players - Median:", basketball_median, "Standard Deviation:", basketball_sd, "\n")
cat("Others Players - Median:", others_median, "Standard Deviation:", others_sd, "\n")

results <- data.frame(
  Group = c("Basketball", "Other Sports"),
  Median_of_the_Height = c(basketball_median,  others_median),
  Lower_CI = c(basketball_median - basketball_sd, others_median - others_sd),
  Upper_CI = c(basketball_median + basketball_sd, others_median + others_sd)
)
results$Group <- factor(results$Group)
plot <- ggplot(results, aes(x = Group,y = Median_of_the_Height)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  geom_point()

# Display the plot
ggsave("median_height_for_all_time.png", plot, width = 8, height = 6, dpi = 300 , , bg = "white")

#проведемо тест волда 

model <- lm(Height ~ Sport, data = df)

coefficient <- coef(model)["SportBasketball"]
standard_error <- sqrt(vcov(model)["SportBasketball", "SportBasketball"])

wald_statistic <- coefficient / standard_error

p_value <- 2 * (1 - pnorm(abs(wald_statistic)))
cat("p_value = " , p_value) 
alpha <- 0.05  # Significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. Basketball players are higher in height than players in other sports.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in height between basketball players and players in other sports.\n")
}

column_names <- colnames(df)
print(column_names)
  
grouped_df <- df %>%
  group_by(Team) %>%
  summarize(Num_Competitors = n_distinct(ID),
            Total_Medals = (!is.na(df$Medal) ))

# Create a new column for the ratio of medals per athlete
grouped_df$Medals_Per_Athlete <- grouped_df$Total_Medals / grouped_df$Num_Competitors


print(grouped_df)

 

