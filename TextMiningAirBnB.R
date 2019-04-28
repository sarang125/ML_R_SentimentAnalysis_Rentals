# Load the dataset
bos_reviews <- readRDS('C:/Users/SARANG/Desktop/DOCS/Jobs/Portfolio/R_TextMining_Airbnb/bos_reviews.rds')
head(bos_reviews)
dim(bos_reviews)
str(bos_reviews)

# Checking the general polarity
install.packages('qdap')
library(qdap)
bos_pol <- polarity(bos_reviews$comments)
summary(bos_pol$all$polarity)
# Plotting the polarity
library(ggplot2)
ggplot(bos_pol$all, aes(x = polarity, y = ..density..)) +
  geom_histogram(binwidth = 0.25, fill = 'skyblue', colour = 'grey60') +
  geom_density(size = 0.75)

# Comparison cloud

bos_reviews$scaled_polarity <- scale(bos_pol$all$polarity)

pos_comments <- subset(bos_reviews$comments, bos_reviews$scaled_polarity > 0)
neg_comments <- subset(bos_reviews$comments, bos_reviews$scaled_polarity < 0)

pos_terms <- paste(pos_comments, collapse = " ")
neg_terms <- paste(neg_comments, collapse = " ")

all_terms <- c(pos_terms,neg_terms)
all_corpus <- VCorpus(VectorSource(all_terms))

all_tdm <- TermDocumentMatrix(all_corpus, 
                              control = list(weighting = weightTfIdf,
                                             removePunctuation = TRUE,
                                             stopwords = stopwords(kind = 'en')
                              )
)
all_tdm_m <- as.matrix(all_tdm)
colnames(all_tdm) <- c('positive', 'negative')

# Plotting the cloud
library(wordcloud)
comparison.cloud(all_tdm_m, max.words = 100, colors = c('green','red'))

# Using tidy way
install.packages('tidytext')
install.packages('dplyr')
install.packages('tidyr')
library(tidytext)
library(dplyr)
library(tidyr)

tidy_reviews <- bos_reviews %>%
  unnest_tokens(word, comments)

tidy_reviews <- tidy_reviews %>%
  group_by(id) %>%
  mutate(original_word_order = seq_along(word))

# Performing the Polarity scoring with lexicon
bing <- get_sentiments('bing')

pos_neg <- tidy_reviews %>%
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)

# Assessing the author effort
tidy_reviews
pos_neg

pos_neg_col <- tidy_reviews %>%
  count(id) %>%
  inner_join(pos_neg) %>%
  mutate(pol = ifelse(polarity >= 0 , 'positive', 'negative'))

pos_neg_col

