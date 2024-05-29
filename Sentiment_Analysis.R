# Load all required packages
# Load tidyverse package
library(tidyverse)
# Load naniar package
library(naniar)
# Load tidytext package
library(tidytext)
# Load DT package for table display
library(DT)
# Load TM package
library(tm)
# Load textclean package
library(textclean)
# Load textstem package
library(textstem)
# Load plotly package
library(plotly)
# Set default theme
theme_set(theme_minimal())

## read_files
amzn_data <- read_csv("Reviews.csv")
# Display the first 500 rows of imported data
datatable(
  amzn_data[1:500,],
  filter = "top",
  caption = "The first 500 rows of loaded data.",
  options = list(pageLength = 50,
                 scrollY = "500px",
                 scrollX = TRUE))

## data_str

gg_miss_var(amzn_data) + theme(text = element_text(size = 14),
                               axis.text.x = element_text(size = 12),
                               axis.text.y = element_text(size = 12))
# Check for class of each variable
class_table <- sapply(amzn_data, class)
class_table <- data.frame(
  Variable = names(class_table),
  Class = as.character(class_table),
  stringsAsFactors = FALSE)
# Check for the proportion of missing values in full data
x <- miss_var_summary(amzn_data)
# Rename the columns 
colnames(x) <- c("Variable", "Values_missing", "Proportion_missing")
# Round the Proportion_missing column to 2 decimal points
x$Proportion_missing <- round(x$Proportion_missing, 2)
# Combine data frames on the "Variable" column
properties <- merge(class_table, x, by = "Variable")
# Display the properties of the data
datatable(
  properties,
  caption = "Table displaying the properties of the data.")

## Data Cleaning and Processing


amzn_cleaned <- amzn_data %>%
# Step 1:Convert Time to POSIXct format and remove the day and time parts
  mutate(Time = as.POSIXct(Time, origin = "1970-01-01"),
    Time = format(Time, "%Y-%m")
    ) %>%
# Step 2: Rename Time column to Year_Month
  rename(Year_Month = Time) %>%
# Step 3: Remove columns that do not provide any valuable information for sentiment classification
 select(-Id, -ProfileName, -HelpfulnessNumerator, -HelpfulnessDenominator)
# Step 4: Convert text to lowercase
amzn_cleaned$Text <- tolower(amzn_cleaned$Text)
# Step 5: Remove punctuation and special characters
amzn_cleaned$Text <- removePunctuation(amzn_cleaned$Text)
# Step 6: Remove white spaces
amzn_cleaned$Text <- stripWhitespace(amzn_cleaned$Text)
# Step 7: Remove English "stopwords"
amzn_cleaned$Text <-
  removeWords(amzn_cleaned$Text, stopwords("en"))
# Display the first 500 rows of the cleaned and reformatted data
datatable(
   amzn_cleaned[1:500,],
   caption = "First 500 rows of cleaned data with new created variables.",
   options = list(pageLength = 50,
                 scrollY = "500px",
                 scrollX = TRUE))


# sent_analysis1
amzn_cleaned$Sentiment <-
  ifelse(amzn_cleaned$Score > 3,"positive",
    ifelse(amzn_cleaned$Score < 3, "negative", 
           "neutral")
  )
# Count the number of reviews in each sentiment category
sentiment_counts <- amzn_cleaned %>%
  group_by(Sentiment) %>%
  summarize(Count = n())
# Plot the sentiment distribution
sent_dist <- ggplot(sentiment_counts, aes(x = Sentiment, y = Count, 
                                     fill = Sentiment)) + geom_col()  +
        scale_fill_manual(values = c(
          "negative" = "red",
          "neutral" = "blue",
          "positive" = "green"
        )) +
        labs(title = "Sentiment Distribution", x = "Sentiment type", 
             y = "Count")  +
        theme(
          text = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 14)
        )
sent_dist

# sent_analysis2
monthly_sentiment <- amzn_cleaned %>%
            group_by(Year_Month, Sentiment) %>%
            mutate(Sentiment = as.factor(Sentiment)) %>%
            summarize(Count = n())
# Plot sentiment change over time
sent_time <- ggplotly(
              ggplot(
                monthly_sentiment,
                aes(x = Year_Month, y = Count, 
                    color = Sentiment, 
                    group = Sentiment)) +
                geom_line(size = 0.8) +
                theme(text = element_text(size = 14),
                      axis.text.x = element_blank(),
                      legend.title = element_blank(),
                      legend.background = element_rect(fill = "white",
                                                       color = "grey")) +
                scale_color_manual(values = c("negative" = "red",
                                              "neutral" = "blue",
                                              "positive" = "green")) +
                labs(title = "Sentiment Change Over Time",
                   x = "Time",
                   y = "Number of Reviews"))
# Change legend position to inside the plot
sent_time <- sent_time %>%
  layout(legend = list(x = 0.1, y = 0.9, 
                       xanchor = "left", yanchor = "top"))
sent_time

# sent_analysis3
# Group reviews by product and calculate sentiment distribution
product_sentiment <- amzn_cleaned %>%
    group_by(ProductId, Sentiment) %>%
    summarize(Count = n())
# Filter products with only Positive sentiment
consistently_positive <- product_sentiment %>%
  filter(Sentiment == "positive" & Count > 0) %>%
  arrange(desc(Count))
# Display top 50 ProductIDs with the highest positive reviews
datatable(
  consistently_positive[1:50,],
  options = list(pageLength = 25,
                 scrollY = "500px",
                 scrollX = TRUE),
  caption = "Top 50 ProductIDs with highest positive reviews.")
# Filter products with only Negative sentiment
consistently_negative <- product_sentiment %>%
  filter(Sentiment == "negative" & Count > 0) %>%
  arrange(desc(Count))
# Display top 50 ProductIDs with the highest negative reviews
datatable(
  consistently_negative[1:25,],
  options = list(pageLength = 50,
                 scrollY = "500px",
                 scrollX = TRUE),
  caption = "Top 50 ProductIDs with highest negative reviews.")

# sent_analysis4
# Calculate the review lengths
amzn_cleaned$ReviewLength <- nchar(amzn_cleaned$Text)
# Group reviews by review length and calculate sentiment distribution
review_length_sentiment <- amzn_cleaned %>%
  group_by(ReviewLength, Sentiment) %>%
  summarize(Count = n())
# Plot sentiment vs. review length
sent_len <- ggplot(review_length_sentiment, 
                   aes(x = ReviewLength,
                       y = Count, 
                       color = Sentiment)) +
              geom_boxplot() + scale_y_log10() +
              labs(title = "Sentiment vs. Review Length",
                           x = "Review Length",
                           y = "Number of Reviews in (Log 10) Scale") +
              theme(text = element_text(size = 14),
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    legend.position = "top",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14)) +
              scale_color_manual(values = c("positive" = "green",
                                              "negative" = "red",
                                              "neutral" = "blue"))
sent_len

# sent_analysis5
# Group reviews by user rating to calculate sentiment distribution
rating_sentiment <- amzn_cleaned %>%
    group_by(Score, Sentiment) %>%
    summarize(Count = n())
# Plot sentiment by user rating
sent_rating <- ggplot(rating_sentiment, aes(x = as.factor(Score), 
                                          y = Count,
                                       fill = Sentiment)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme(text = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  legend.position = "top",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14)) +
            labs(title = "Sentiment Variation Across User Ratings",
                 x = "Customer Rating",
                 y = "Number of Reviews",
                 fill = "Sentiment") +
            scale_fill_manual(values = c("positive" = "green", 
                                          "negative" = "red", 
                                          "neutral" = "blue"))
sent_rating