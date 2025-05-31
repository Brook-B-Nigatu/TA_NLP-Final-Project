# Script Start: Install and Load Required Packages
if (getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
}
# List of required packages
required_packages <- c(
  "tidyverse", 
  "tidytext", 
  "topicmodels", 
  "tm", 
  "textdata", 
  "ggplot2", 
  "wordcloud", 
  "RColorBrewer", 
  "dplyr",       # Often part of tidyverse, but good to list if used directly extensively
  "stringr",     # Often part of tidyverse
  "broom",
  "textstem",    # For lemmatization
  "ldatuning",   # For coherence calculations (if you decide to use a pre-built function)
  "reshape2",    # For the melt function used in plotting
  "gridExtra",   # For arranging multiple plots
  "devtools",    # For installing packages from GitHub
  "spacyr"       # For Named Entity Recognition
)

# Check if packages are installed, install if not
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("Package", pkg, "not found. Installing...\n"))
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load all packages (suppressing startup messages for cleaner output if desired)
cat("Loading required libraries...\n")
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidytext)
  library(topicmodels)
  library(tm)
  library(textdata)
  library(ggplot2)
  library(wordcloud)
  library(RColorBrewer)
  library(dplyr)
  library(stringr)
  library(broom)
  library(textstem)
  # library(ldatuning) # Note: Your provided script has a custom coherence function. 
                     # ldatuning is useful if you want other pre-built topic tuning.
                     # If you are only using your custom function, you might not strictly need it.
  library(reshape2)
  library(gridExtra)
})

cat("All required libraries loaded successfully.\n\n")

# Text Analysis of r/Futurology Posts and Comments
# Based on Chapter 9 techniques and plan.txt requirements

# Load required libraries
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(textdata)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(broom)
library(textstem)  # For lemmatization
# library(ldatuning)  # For coherence calculations

# Set working directory
setwd("c:/Users/brook/Desktop/Courses/Year 1/Sem 2/Text Analysis/Project")

# Load data
print("Loading data...")
submissions <- read.csv("futurology_submissions.csv", stringsAsFactors = FALSE)
comments <- read.csv("futurology_comments.csv", stringsAsFactors = FALSE)

# 1. Combine posts and submissions (treat each post body and each comment as one document)
print("Step 1: Combining posts and comments...")

# Prepare submissions data
submissions_clean <- submissions %>%
  filter(!is.na(body) & body != "" & body != "[deleted]" & body != "[removed]") %>%
  select(id, title, body, author, created_utc) %>%
  mutate(
    text = paste(title, body, sep = " "),
    document_type = "submission",
    document_id = paste0("sub_", id)
  ) %>%
  select(document_id, text, document_type, author, created_utc)

# Prepare comments data
comments_clean <- comments %>%
  filter(!is.na(body) & body != "" & body != "[deleted]" & body != "[removed]") %>%
  select(id, body, author, created_utc, submission_id) %>%
  mutate(
    text = body,
    document_type = "comment",
    document_id = paste0("com_", id)
  ) %>%
  select(document_id, text, document_type, author, created_utc)

# Combine all documents
all_documents <- bind_rows(submissions_clean, comments_clean)
print(paste("Total documents:", nrow(all_documents)))

# 4. Remove stop words and tokenize
print("Step 4: Tokenizing, lemmatizing, and removing stop words...")

# Create custom stop words (combining standard English stop words with common Reddit terms)
custom_stop_words <- bind_rows(
  stop_words,
  tibble(word = c("reddit", "sub", "subreddit", "post", "comment", "edit", "update", 
                  "deleted", "removed", "nbsp", "http", "https", "www", "com",
                  "people", "thing", "things", "lot", "bit", "kind", "sort",
                  "pretty", "really", "actually", "basically", "literally"),
         lexicon = "custom")
)

# Tokenize, lemmatize, and clean text
tidy_documents <- all_documents %>%
  unnest_tokens(word, text) %>%
  filter(
    !str_detect(word, "^[0-9]+$"),  # Remove pure numbers
    # str_length(word) > 2,           # Remove very short words
    !str_detect(word, "^https?"),   # Remove URLs
    !str_detect(word, "^[^a-z]")    # Remove non-alphabetic words
  ) %>%
  mutate(word_lemma = lemmatize_words(word)) %>%  # Add lemmatization step
  anti_join(custom_stop_words) # %>%  # Remove stop words after lemmatization
#   filter(str_length(word_lemma) > 2)  # Remove very short lemmas

print(paste("Total tokens after cleaning:", nrow(tidy_documents)))

# Create document-term matrix for topic modeling
print("Preparing document-term matrix for topic modeling...")

# Count words per document
doc_word_counts <- tidy_documents %>%
  count(document_id, word_lemma, sort = TRUE)

# Create document-term matrix
dtm <- doc_word_counts %>%
  cast_dtm(document_id, word_lemma, n)

print(paste("DTM dimensions:", nrow(dtm), "x", ncol(dtm)))

# 2. Topic modeling - try different numbers of topics and evaluate
print("Step 2: Topic modeling with different k values...")

# Function to calculate coherence score (simplified version)
calculate_perplexity <- function(lda_model, dtm) {
  perplexity(lda_model, dtm)
}

# Function to calculate coherence score
calculate_coherence <- function(lda_model, dtm, top_n = 10) {
  # Extract top terms for each topic
  topics_terms <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = top_n) %>%
    ungroup()
  
  # Calculate coherence using word co-occurrence
  coherence_scores <- numeric(length(unique(topics_terms$topic)))
  
  for (topic_num in unique(topics_terms$topic)) {
    topic_words <- topics_terms %>%
      filter(topic == topic_num) %>%
      pull(term)
    
    # Calculate pairwise coherence
    coherence_sum <- 0
    pair_count <- 0
    
    for (i in 1:(length(topic_words)-1)) {
      for (j in (i+1):length(topic_words)) {
        word1 <- topic_words[i]
        word2 <- topic_words[j]
        
        # Count documents containing both words
        docs_with_both <- sum(as.vector(dtm[, word1] > 0) & as.vector(dtm[, word2] > 0))
        docs_with_word1 <- sum(as.vector(dtm[, word1] > 0))
        
        if (docs_with_word1 > 0) {
          coherence_sum <- coherence_sum + log((docs_with_both + 1) / docs_with_word1)
          pair_count <- pair_count + 1
        }
      }
    }
    
    coherence_scores[topic_num] <- if (pair_count > 0) coherence_sum / pair_count else 0
  }
  
  return(mean(coherence_scores))
}

# Test different numbers of topics
k_values <- c(3, 5, 7, 9, 11)
# k_values <- c(3, 4)
topic_models <- list()
perplexity_scores <- numeric(length(k_values))
coherence_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  print(paste("Fitting LDA model with k =", k))
  
  set.seed(123)  # For reproducibility
  lda_model <- LDA(dtm, k = k, control = list(alpha = 0.1, seed = 123))
  
  topic_models[[i]] <- lda_model
  perplexity_scores[i] <- calculate_perplexity(lda_model, dtm)
  coherence_scores[i] <- calculate_coherence(lda_model, dtm)
  
  print(paste("Perplexity for k =", k, ":", round(perplexity_scores[i], 2)))
  print(paste("Coherence for k =", k, ":", round(coherence_scores[i], 4)))
}

# Normalize scores for comparison (higher is better for both after normalization)
# For perplexity, we want lower values, so we invert the ranking
normalized_perplexity <- (max(perplexity_scores) - perplexity_scores) / (max(perplexity_scores) - min(perplexity_scores))
# For coherence, higher is already better
normalized_coherence <- (coherence_scores - min(coherence_scores)) / (max(coherence_scores) - min(coherence_scores))

# Combined score (equal weighting)
combined_scores <- (normalized_perplexity + normalized_coherence) / 2

# Choose optimal number of topics based on combined score
optimal_k_index <- which.max(combined_scores)
optimal_k <- k_values[optimal_k_index]
optimal_model <- topic_models[[optimal_k_index]]

print(paste("Optimal number of topics (combined metric):", optimal_k))
print(paste("Best perplexity score:", round(perplexity_scores[optimal_k_index], 2)))
print(paste("Best coherence score:", round(coherence_scores[optimal_k_index], 4)))
print(paste("Best combined score:", round(combined_scores[optimal_k_index], 4)))

# Create comprehensive evaluation dataframe
evaluation_df <- data.frame(
  k = k_values,
  perplexity = perplexity_scores,
  coherence = coherence_scores,
  normalized_perplexity = normalized_perplexity,
  normalized_coherence = normalized_coherence,
  combined_score = combined_scores
)

print("Complete evaluation results:")
print(evaluation_df)

# Plot all metrics
library(reshape2)
metrics_long <- evaluation_df %>%
  select(k, normalized_perplexity, normalized_coherence, combined_score) %>%
  melt(id.vars = "k", variable.name = "metric", value.name = "score")

ggplot(metrics_long, aes(x = k, y = score, color = metric)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "purple"),
                     labels = c("Normalized Perplexity (inverted)", "Normalized Coherence", "Combined Score")) +
  labs(title = "Topic Model Evaluation Metrics",
       subtitle = paste("Optimal k =", optimal_k, "(based on combined score)"),
       x = "Number of Topics (k)",
       y = "Normalized Score (higher is better)",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

if (!dir.exists("output")) dir.create("output")

ggsave("output/topic_model_evaluation_comprehensive.png", width = 12, height = 8, dpi = 300)

# Individual plots for detailed view
# Perplexity plot
perplexity_df <- data.frame(k = k_values, perplexity = perplexity_scores)
p1 <- ggplot(perplexity_df, aes(x = k, y = perplexity)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed", alpha = 0.7) +
  labs(title = "Perplexity by Number of Topics",
       subtitle = "Lower is better",
       x = "Number of Topics (k)",
       y = "Perplexity") +
  theme_minimal()

# Coherence plot
coherence_df <- data.frame(k = k_values, coherence = coherence_scores)
p2 <- ggplot(coherence_df, aes(x = k, y = coherence)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 3) +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed", alpha = 0.7) +
  labs(title = "Coherence by Number of Topics",
       subtitle = "Higher is better",
       x = "Number of Topics (k)",
       y = "Coherence Score") +
  theme_minimal()

# Combine plots
library(gridExtra)
combined_plot <- grid.arrange(p1, p2, ncol = 2)

ggsave("output/perplexity_and_coherence_plots.png", combined_plot, width = 15, height = 6, dpi = 300)

# 3. Document assignments and topic analysis
print("Step 3: Analyzing document assignments and topic distributions...")

# Get document-topic probabilities
doc_topics <- tidy(optimal_model, matrix = "gamma")

# Get the most likely topic for each document
doc_topic_assignments <- doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1, with_ties = FALSE) %>%
  ungroup()

# Count documents per topic
documents_per_topic <- doc_topic_assignments %>%
  count(topic, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Documents per topic:")
print(documents_per_topic)

# Visualize documents per topic
ggplot(documents_per_topic, aes(x = factor(topic), y = n)) +
  geom_col(fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = paste0(n, " (", percentage, "%)")), vjust = -0.5) +
  labs(title = "Number of Documents per Topic",
       x = "Topic",
       y = "Number of Documents") +
  theme_minimal()

ggsave("output/documents_per_topic.png", width = 10, height = 6, dpi = 300)

# 5. Most common words overall and for each topic
print("Step 5: Finding most common words...")

# Overall most common words
overall_word_freq <- tidy_documents %>%
  count(word_lemma, sort = TRUE) %>%
  slice_head(n = 20)

print("Top 20 most common lemmatized words overall:")
print(overall_word_freq)

# Most common words per topic
topic_terms <- tidy(optimal_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

print("Top words per topic:")
for (t in 1:optimal_k) {
  cat("\n--- Topic", t, "---\n")
  topic_words <- topic_terms %>%
    filter(topic == t) %>%
    select(term, beta)
  print(topic_words)
}

# Visualize top words per topic
ggplot(topic_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_viridis_d() +
  labs(title = "Top Words per Topic",
       x = "Terms",
       y = "Beta (word probability in topic)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave("output/top_words_per_topic.png", width = 15, height = 10, dpi = 300)

# Create word clouds for each topic
print("Creating word clouds for each topic...")

for (t in 1:optimal_k) {
  topic_words <- topic_terms %>%
    filter(topic == t)
  
  png(paste0("output/wordcloud_topic_", t, ".png"), width = 800, height = 600)
  
  wordcloud(words = topic_words$term, 
            freq = topic_words$beta,
            min.freq = 1,
            max.words = 100,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
  
  dev.off()
}

# 7. Sentiment analysis within each topic
print("Step 7: Sentiment analysis by topic...")

# Get sentiment lexicons
# get_sentiments("bing") -> bing_sentiments
# get_sentiments("afinn") -> afinn_sentiments

# bing_sentiments <- textdata::lexicon_bing()
# afinn_sentiments <- textdata::lexicon_afinn()

bing_sentiments <- readRDS("bing_lexicon.rds")
afinn_sentiments <- readRDS("afinn_lexicon.rds")

doc_topic_assignments <- doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

# Add topic information to tidy_documents
tidy_documents_with_topics <- tidy_documents %>%
  left_join(doc_topic_assignments, by = c("document_id" = "document")) %>%
  filter(!is.na(topic))

# Sentiment analysis using bing lexicon
# Note: We'll use original words for sentiment analysis since sentiment lexicons 
# are based on full words, not lemmas. Lemmatization is primarily used for 
# frequency analysis, topic modeling, and TF-IDF calculations.
sentiment_by_topic <- tidy_documents_with_topics %>%
  inner_join(bing_sentiments, by = "word") %>%
  count(topic, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = (positive - negative) / (positive + negative))

print("Sentiment analysis by topic (Bing lexicon):")
print(sentiment_by_topic)

# AFINN sentiment scores
afinn_by_topic <- tidy_documents_with_topics %>%
  inner_join(afinn_sentiments, by = "word") %>%
  group_by(topic) %>%
  summarise(
    mean_sentiment = mean(value, na.rm = TRUE),
    median_sentiment = median(value, na.rm = TRUE),
    total_words = n(),
    .groups = "drop"
  )

print("AFINN sentiment scores by topic:")
print(afinn_by_topic)

# Combine sentiment results
sentiment_summary <- sentiment_by_topic %>%
  left_join(afinn_by_topic, by = "topic") %>%
  select(topic, positive, negative, sentiment_score, mean_sentiment, median_sentiment, total_words)

print("Complete sentiment summary by topic:")
print(sentiment_summary)

# Visualize AFINN sentiment by topic
ggplot(afinn_by_topic, aes(x = factor(topic), y = mean_sentiment)) +
  geom_col(aes(fill = mean_sentiment > 0), alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Negative", "Positive"),
                    name = "Average Sentiment") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_text(aes(label = round(mean_sentiment, 2)), vjust = -0.5, size = 3) +
  labs(title = "AFINN Mean Sentiment Score by Topic",
       subtitle = "Range: -5 (most negative) to +5 (most positive)",
       x = "Topic",
       y = "Mean AFINN Score") +
  theme_minimal()

ggsave("output/afinn_mean_sentiment_by_topic.png", width = 10, height = 6, dpi = 300)

# AFINN sentiment distribution (show both mean and median)
afinn_long <- afinn_by_topic %>%
  select(topic, mean_sentiment, median_sentiment) %>%
  pivot_longer(cols = c(mean_sentiment, median_sentiment), 
               names_to = "measure", values_to = "score") %>%
  mutate(measure = case_when(
    measure == "mean_sentiment" ~ "Mean",
    measure == "median_sentiment" ~ "Median"
  ))

ggplot(afinn_long, aes(x = factor(topic), y = score, fill = measure)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "orange")) +
  labs(title = "AFINN Sentiment Scores by Topic: Mean vs Median",
       subtitle = "Comparing central tendencies of sentiment scores",
       x = "Topic",
       y = "AFINN Score",
       fill = "Measure") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/afinn_mean_vs_median_by_topic.png", width = 12, height = 6, dpi = 300)

# Ensure output directory exists before saving any files

# Visualize sentiment by topic
ggplot(sentiment_by_topic, aes(x = factor(topic))) +
  geom_col(aes(y = positive), fill = "green", alpha = 0.7, position = "identity") +
  geom_col(aes(y = -negative), fill = "red", alpha = 0.7, position = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Sentiment Distribution by Topic (Bing Lexicon)",
       subtitle = "Green = Positive, Red = Negative",
       x = "Topic",
       y = "Word Count") +
  theme_minimal()

ggsave("output/sentiment_by_topic.png", width = 10, height = 6, dpi = 300)

# Net sentiment plot
ggplot(sentiment_by_topic, aes(x = factor(topic), y = sentiment_score)) +
  geom_col(aes(fill = sentiment_score > 0), alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Negative", "Positive"),
                    name = "Overall Sentiment") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Net Sentiment Score by Topic",
       x = "Topic",
       y = "Sentiment Score (Positive - Negative) / Total") +
  theme_minimal()

ggsave("output/net_sentiment_by_topic.png", width = 10, height = 6, dpi = 300)

# Create a comprehensive summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
print("COMPREHENSIVE ANALYSIS SUMMARY")
print(paste(rep("=", 60), collapse = ""))

cat("\n1. DATA OVERVIEW:\n")
cat("- Total documents analyzed:", nrow(all_documents), "\n")
cat("- Total tokens after cleaning:", nrow(tidy_documents), "\n")
cat("- Submissions:", nrow(submissions_clean), "\n")
cat("- Comments:", nrow(comments_clean), "\n")

cat("\n2. TOPIC MODELING:\n")
cat("- Optimal number of topics:", optimal_k, "\n")
cat("- Best perplexity score:", round(perplexity_scores[optimal_k_index], 2), "\n")

cat("\n3. TOPIC DISTRIBUTION:\n")
for (i in 1:nrow(documents_per_topic)) {
  cat("- Topic", documents_per_topic$topic[i], ":", 
      documents_per_topic$n[i], "documents (", documents_per_topic$percentage[i], "%)\n")
}

cat("\n4. SENTIMENT OVERVIEW:\n")
for (i in 1:nrow(sentiment_summary)) {
  cat("- Topic", sentiment_summary$topic[i], ": Positive =", sentiment_summary$positive[i], 
      ", Negative =", sentiment_summary$negative[i], 
      ", Score =", round(sentiment_summary$sentiment_score[i], 3), "\n")
}

# Install and load tidyvader package
if (!requireNamespace("tidyvader", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("chris31415926535/tidyvader")
}

library(tidyvader)

# VADER sentiment analysis by topic
print("Performing VADER sentiment analysis by topic...")

# First, we need to reconstruct the original text for each document to use VADER
documents_for_vader <- all_documents %>%
  left_join(doc_topic_assignments, by = c("document_id" = "document")) %>%
  filter(!is.na(topic)) %>% 
  mutate(
    clean_text = str_squish(text) 
  ) %>%
  filter(nchar(clean_text) > 0) %>% 
  select(document_id, topic, clean_text) 

print(paste("Running VADER analysis on", nrow(documents_for_vader), "non-empty documents..."))

vader_augmented_df <- tidyvader::vader(documents_for_vader, clean_text)

vader_results <- vader_augmented_df %>%
  select(document_id, topic, compound, pos, neu, neg) %>% 
  mutate(
    compound = as.numeric(compound),
    pos = as.numeric(pos),
    neu = as.numeric(neu),
    neg = as.numeric(neg)
  )

print(paste("VADER analysis successfully processed. vader_results has", nrow(vader_results), "rows."))

# Summarize VADER sentiment by topic
vader_by_topic <- vader_results %>%
  group_by(topic) %>%
  summarise(
    mean_compound = mean(compound, na.rm = TRUE),
    median_compound = median(compound, na.rm = TRUE),
    mean_positive = mean(pos, na.rm = TRUE),
    mean_neutral = mean(neu, na.rm = TRUE),
    mean_negative = mean(neg, na.rm = TRUE),
    # Classify overall sentiment based on compound score thresholds
    # Based on VADER documentation: >= 0.05 positive, <= -0.05 negative, between is neutral
    very_positive = sum(compound >= 0.5, na.rm = TRUE),
    positive_docs = sum(compound >= 0.05 & compound < 0.5, na.rm = TRUE),
    neutral_docs = sum(compound > -0.05 & compound < 0.05, na.rm = TRUE),
    negative_docs = sum(compound <= -0.05 & compound > -0.5, na.rm = TRUE),
    very_negative = sum(compound <= -0.5, na.rm = TRUE),
    total_docs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate percentages
    pct_very_positive = round(very_positive / total_docs * 100, 1),
    pct_positive = round(positive_docs / total_docs * 100, 1),
    pct_neutral = round(neutral_docs / total_docs * 100, 1),
    pct_negative = round(negative_docs / total_docs * 100, 1),
    pct_very_negative = round(very_negative / total_docs * 100, 1)
  )

print("VADER sentiment scores by topic:")
print(vader_by_topic)

# Update sentiment summary to include VADER results
sentiment_summary_with_vader <- sentiment_summary %>%
  left_join(vader_by_topic, by = "topic")

print("Complete sentiment summary with VADER by topic:")
print(sentiment_summary_with_vader)

# Visualize VADER sentiment distribution by topic
vader_plot_data <- vader_by_topic %>%
  select(topic, very_positive, positive_docs, neutral_docs, negative_docs, very_negative) %>%
  pivot_longer(cols = -topic, names_to = "sentiment_category", values_to = "count") %>%
  mutate(
    sentiment_category = factor(sentiment_category, 
                               levels = c("very_negative", "negative_docs", "neutral_docs", 
                                        "positive_docs", "very_positive"),
                               labels = c("Very Negative", "Negative", "Neutral", 
                                        "Positive", "Very Positive"))
  )

# Stacked bar chart for VADER sentiment distribution
ggplot(vader_plot_data, aes(x = factor(topic), y = count, fill = sentiment_category)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("darkred", "red", "gray", "green", "darkgreen")) +
  labs(title = "VADER Sentiment Distribution by Topic",
       subtitle = "Based on compound sentiment scores",
       x = "Topic",
       y = "Number of Documents",
       fill = "Sentiment Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/vader_sentiment_distribution_by_topic.png", width = 12, height = 8, dpi = 300)

# VADER compound score by topic
ggplot(vader_by_topic, aes(x = factor(topic), y = mean_compound)) +
  geom_col(aes(fill = mean_compound > 0), alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Negative", "Positive"),
                    name = "Overall Sentiment",
                    breaks = c(FALSE, TRUE)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_text(aes(label = round(mean_compound, 3)), vjust = -0.5, size = 3) +
  labs(title = "VADER Mean Compound Sentiment Score by Topic",
       subtitle = "Range: -1 (most negative) to +1 (most positive)",
       x = "Topic",
       y = "Mean Compound Score") +
  theme_minimal()

ggsave("output/vader_compound_scores_by_topic.png", width = 10, height = 6, dpi = 300)

# Save VADER results to CSV
write.csv(vader_by_topic, file.path("output", "vader_sentiment_by_topic.csv"), row.names = FALSE)
write.csv(sentiment_summary_with_vader, file.path("output", "complete_sentiment_summary_with_vader.csv"), row.names = FALSE)
write.csv(vader_results, file.path("output", "vader_document_level_scores.csv"), row.names = FALSE)

# Create comparison plot of all three sentiment methods
print("Creating sentiment methods comparison plot...")

# Prepare data for comparison plot
comparison_data <- sentiment_summary_with_vader %>%
  select(topic, 
         bing_score = sentiment_score,
         afinn_score = mean_sentiment,
         vader_score = mean_compound) %>%
  pivot_longer(cols = c(bing_score, afinn_score, vader_score), 
               names_to = "method", values_to = "score") %>%
  mutate(
    method = factor(method, 
                   levels = c("bing_score", "afinn_score", "vader_score"),
                   labels = c("Bing Lexicon", "AFINN Lexicon", "VADER"))
  )

# Create comparison plot
ggplot(comparison_data, aes(x = factor(topic), y = score, fill = method)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "Sentiment Analysis Methods Comparison by Topic",
       subtitle = "Comparing Bing, AFINN, and VADER sentiment scores",
       x = "Topic",
       y = "Sentiment Score",
       fill = "Method",
       caption = "Note: Different scales - Bing: [-1,1], AFINN: typically [-5,5], VADER: [-1,1]") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/sentiment_methods_comparison.png", width = 12, height = 8, dpi = 300)

print("VADER sentiment analysis complete!")
print("VADER Sentiment Analysis Summary:")
print("- VADER analyzes full sentences/documents rather than individual words")
print("- It handles negations, intensifiers, capitalization, and punctuation")
print("- Compound scores range from -1 (most negative) to +1 (most positive)")
print("- Classification thresholds: >= 0.05 positive, <= -0.05 negative, between is neutral")
print("Additional files generated:")
print("- output/vader_sentiment_distribution_by_topic.png")
print("- output/vader_compound_scores_by_topic.png") 
print("- output/sentiment_methods_comparison.png")
print("- output/vader_sentiment_by_topic.csv")
print("- output/complete_sentiment_summary_with_vader.csv")
print("- output/vader_document_level_scores.csv")

# --- START: Named Entity Recognition (NER) by Topic ---
print("Starting Named Entity Recognition (NER) by Topic...")

# Install and initialize spacyr
if (!requireNamespace("spacyr", quietly = TRUE)) {
  install.packages("spacyr")
}

library(spacyr)

# Initialize spaCy
spacy_initialize(model = "en_core_web_sm", ask = FALSE)

# Prepare data for NER
documents_for_ner <- all_documents %>%
  left_join(doc_topic_assignments, by = c("document_id" = "document")) %>%
  filter(!is.na(topic) & !is.na(text) & text != "") %>%
  select(doc_id = document_id, text, topic) # spacy_parse expects 'doc_id' and 'text'

# Ensure text is character type
documents_for_ner$text <- as.character(documents_for_ner$text)

# Perform NER parsing
parsed_entities <- spacy_parse(documents_for_ner, entity = TRUE, lemma = FALSE, pos = FALSE)

# Filter for actual entities and get entity text
# Create a list of words to exclude (common words, articles, etc.)
excluded_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", 
                   "of", "with", "by", "from", "as", "is", "was", "are", "were", "be", 
                   "been", "being", "have", "has", "had", "do", "does", "did", "will", 
                   "would", "could", "should", "may", "might", "can", "must", "shall",
                   "this", "that", "these", "those", "i", "you", "he", "she", "it", 
                   "we", "they", "me", "him", "her", "us", "them", "my", "your", 
                   "his", "her", "its", "our", "their", "myself", "yourself", "himself",
                   "herself", "itself", "ourselves", "yourselves", "themselves")

entities_by_doc <- parsed_entities %>%
  filter(entity != "") %>% # Keep only actual entities (use 'entity' column)
  select(doc_id, entity_text = token, entity_type = entity) %>%
  # Filter out common words, very short entities, and entities that are just numbers/symbols
  filter(
    !tolower(entity_text) %in% excluded_words,  # Remove common words
    nchar(entity_text) >= 2,                    # Remove single characters
    !grepl("^[0-9]+$", entity_text),           # Remove pure numbers
    !grepl("^[[:punct:]]+$", entity_text),     # Remove pure punctuation
    !entity_text %in% c("%", "$", "€", "£"),   # Remove currency/percent symbols
    # Keep only meaningful entity types (exclude some spaCy noise)
    entity_type %in% c("PERSON", "ORG", "GPE", "LOC", "EVENT", "FAC", "PRODUCT", 
                      "WORK_OF_ART", "LAW", "LANGUAGE", "NORP", "MONEY", "PERCENT",
                      "ORG_B", "ORG_I", "GPE_B", "GPE_I", "PERSON_B", "PERSON_I",
                      "LOC_B", "LOC_I", "PRODUCT_B", "PRODUCT_I", "EVENT_B", "EVENT_I")
  )

# Join with topic information
entities_with_topics <- entities_by_doc %>%
  left_join(distinct(documents_for_ner, doc_id, topic), by = "doc_id") %>%
  filter(!is.na(topic))

# Calculate entity frequency across all topics to identify potential noise
entity_global_freq <- entities_with_topics %>%
  count(entity_text, sort = TRUE)

# Most frequent entities per topic (this is what "dominant entities" means)
dominant_entities_per_topic <- entities_with_topics %>%
  count(topic, entity_text, entity_type, sort = TRUE) %>%
  group_by(topic) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  arrange(topic, -n)

print("Dominant (most frequent) entities per topic:")
print(dominant_entities_per_topic)

# Save results
write.csv(dominant_entities_per_topic, file.path("output", "ner_dominant_entities_per_topic.csv"), row.names = FALSE)

# Create comprehensive visualization of dominant entities per topic
print("Creating comprehensive visualization of dominant entities per topic...")

# Get top 8 entities per topic for cleaner visualization
top_entities_per_topic <- dominant_entities_per_topic %>%
  group_by(topic) %>%
  slice_head(n = 8) %>%
  ungroup()

# Create a comprehensive plot showing all dominant entities by topic
plot_dominant_entities <- ggplot(top_entities_per_topic, 
                                aes(x = reorder_within(entity_text, n, topic), 
                                    y = n, 
                                    fill = entity_type)) +
  geom_col() +
  facet_wrap(~ paste("Topic", topic), scales = "free_y", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_viridis_d(name = "Entity Type") +
  labs(title = "Dominant Entities per Topic",
       subtitle = "Most frequently mentioned entities by topic with entity type color coding",
       x = "Entity",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggsave(file.path("output", "ner_dominant_entities_per_topic.png"), plot_dominant_entities, 
       width = 15, height = 12, dpi = 300)

print("Saved comprehensive dominant entities visualization: output/ner_dominant_entities_per_topic.png")

# Create visualization of top entities by type (e.g., organizations)
org_entities <- dominant_entities_per_topic %>% 
  filter(entity_type %in% c("ORG", "ORG_B", "ORG_I")) %>%
  group_by(topic) %>%
  slice_max(n, n = 5) %>%
  ungroup()

if (nrow(org_entities) > 0) {
  plot_top_orgs <- ggplot(org_entities, aes(x = reorder_within(entity_text, n, topic), y = n, fill = factor(topic))) +
    geom_col() +
    facet_wrap(~ paste("Topic", topic), scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_viridis_d(name = "Topic") +
    labs(title = "Top Organizations by Topic", 
         subtitle = "Most frequently mentioned organizations in each topic",
         x = "Organization", 
         y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "none"
    )
  
  ggsave(file.path("output", "ner_top_organizations_per_topic.png"), plot_top_orgs, 
         width = 12, height = 8, dpi = 300)
  
  print("Saved organizations visualization: output/ner_top_organizations_per_topic.png")
}

# Create visualization for geographic entities (GPE and LOC)
geo_entities <- dominant_entities_per_topic %>% 
  filter(entity_type %in% c("GPE", "GPE_B", "GPE_I", "LOC", "LOC_B", "LOC_I")) %>%
  group_by(topic) %>%
  slice_head(n = 6) %>%
  ungroup()

if (nrow(geo_entities) > 0) {
  plot_geo_entities <- ggplot(geo_entities, aes(x = reorder_within(entity_text, n, topic), y = n, fill = factor(topic))) +
    geom_col() +
    facet_wrap(~ paste("Topic", topic), scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_viridis_d(name = "Topic") +
    labs(title = "Top Geographic Entities by Topic",
         subtitle = "Most frequently mentioned places and countries in each topic",
         x = "Geographic Entity",
         y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "none"
    )
  
  ggsave(file.path("output", "ner_top_geographic_entities_per_topic.png"), plot_geo_entities, 
         width = 12, height = 8, dpi = 300)
  
  print("Saved geographic entities visualization: output/ner_top_geographic_entities_per_topic.png")
}

# Create visualization for people entities
people_entities <- dominant_entities_per_topic %>% 
  filter(entity_type %in% c("PERSON", "PERSON_B", "PERSON_I")) %>%
  group_by(topic) %>%
  slice_head(n = 5) %>%
  ungroup()

if (nrow(people_entities) > 0) {
  plot_people_entities <- ggplot(people_entities, aes(x = reorder_within(entity_text, n, topic), y = n, fill = factor(topic))) +
    geom_col() +
    facet_wrap(~ paste("Topic", topic), scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_viridis_d(name = "Topic") +
    labs(title = "Top People Mentioned by Topic",
         subtitle = "Most frequently mentioned individuals in each topic",
         x = "Person",
         y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "none"
    )
  
  ggsave(file.path("output", "ner_top_people_per_topic.png"), plot_people_entities, 
         width = 12, height = 8, dpi = 300)
  
  print("Saved people entities visualization: output/ner_top_people_per_topic.png")
}

# Distribution of entity types per topic
entity_type_distribution <- entities_with_topics %>%
  count(topic, entity_type, sort = TRUE) %>%
  group_by(topic) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

print("Distribution of entity types per topic:")
print(entity_type_distribution)

write.csv(entity_type_distribution, file.path("output", "ner_entity_type_distribution_per_topic.csv"), row.names = FALSE)

# Visualization of entity type distribution
plot_entity_types <- ggplot(entity_type_distribution, aes(x = factor(topic), y = n, fill = entity_type)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Entity Types by Topic",
       fill = "Entity Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path("output", "ner_entity_type_distribution.png"), plot_entity_types, 
       width = 12, height = 7, dpi = 300)

# Summary by topic with top entity types
ner_summary_by_topic <- entity_type_distribution %>%
  group_by(topic) %>%
  slice_max(n, n = 3) %>%
  summarise(
    top_entity_types = paste(entity_type, collapse = ", "),
    total_entities = sum(n),
    .groups = "drop"
  )

print("NER Summary by Topic:")
print(ner_summary_by_topic)

write.csv(ner_summary_by_topic, file.path("output", "ner_summary_by_topic.csv"), row.names = FALSE)

# Finalize spaCy (optional cleanup)
spacy_finalize()

# --- END: Named Entity Recognition (NER) by Topic ---

# Save all results to CSV files
write.csv(documents_per_topic, file.path("output", "documents_per_topic.csv"), row.names = FALSE)
write.csv(topic_terms, file.path("output", "top_words_per_topic.csv"), row.names = FALSE)
write.csv(sentiment_summary, file.path("output", "sentiment_summary_by_topic.csv"), row.names = FALSE)
write.csv(overall_word_freq, file.path("output", "overall_word_frequency.csv"), row.names = FALSE)

print("\nAnalysis complete! Check the generated CSV files and plots for detailed results.")
print("Generated files:")
print("- output/topic_model_evaluation_comprehensive.png")
print("- output/perplexity_and_coherence_plots.png")
print("- output/documents_per_topic.png")
print("- output/top_words_per_topic.png")
print("- output/wordcloud_topic_[1-k].png")
print("- output/sentiment_by_topic.png")
print("- output/net_sentiment_by_topic.png")
print("- output/afinn_mean_sentiment_by_topic.png")
print("- output/afinn_mean_vs_median_by_topic.png")
print("- output/vader_sentiment_distribution_by_topic.png")
print("- output/vader_compound_scores_by_topic.png")
print("- output/sentiment_methods_comparison.png")
print("- output/ner_dominant_entities_per_topic.png")
print("- output/ner_top_organizations_per_topic.png")
print("- output/ner_top_geographic_entities_per_topic.png")
print("- output/ner_top_people_per_topic.png")
print("- output/ner_entity_type_distribution.png")
print("- output/documents_per_topic.csv")
print("- output/top_words_per_topic.csv")
print("- output/sentiment_summary_by_topic.csv")
print("- output/overall_word_frequency.csv")
print("- output/vader_sentiment_by_topic.csv")
print("- output/complete_sentiment_summary_with_vader.csv")
print("- output/vader_document_level_scores.csv")
print("- output/ner_dominant_entities_per_topic.csv")
print("- output/ner_entity_type_distribution_per_topic.csv")
print("- output/ner_summary_by_topic.csv")
print("- Various CSV files with detailed results")

print("\n=== NER VISUALIZATION SUMMARY ===")
print("The following NER visualizations have been created:")
print("1. ner_dominant_entities_per_topic.png - Comprehensive view of all dominant entities")
print("2. ner_top_organizations_per_topic.png - Organizations by topic")
print("3. ner_top_geographic_entities_per_topic.png - Geographic entities by topic")
print("4. ner_top_people_per_topic.png - People mentioned by topic")
print("5. ner_entity_type_distribution.png - Distribution of entity types across topics")
print("\nThese visualizations provide insights into:")
print("- Which organizations, people, and places are most discussed in each topic")
print("- The relative importance of different entities within each thematic area")
print("- Geographic and organizational focus of futurology discussions")
print("- Key individuals driving conversations in different topic areas")
