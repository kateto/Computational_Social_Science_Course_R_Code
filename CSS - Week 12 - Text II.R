
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##




# ================  Text Analysis in R  ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  

# install.packages("readr")       # Data reading
# install.packages("tidyverse")   # Data processing
# install.packages("tidytext")    # Text analysis  
# install.packages("textclean")   # Text cleaning  
# install.packages("textstem")    # Word stemming 
# install.packages("topicmodels") # Topic modeling
# install.packages("topicdoc")    # Topic model fit


# ================  Reading and pre-processing  ================


library("readr")
library("tidyverse") 
library("tidytext") 
library("textclean")
library("textstem")
library("topicmodels")  
library("topicdoc")  


# You may need to run this depending on your system settings to avoid 
# errors based on the language of your system. Run it if you get errors
# saying that something is "invalid in this locale"
# Sys.setlocale('LC_ALL','C')


# ------~~ Reading in PDF files --------  

# Since you may have to work with PDF data, we will start  with a 
# quick example of reading the text and metadata of a PDF file:

library("pdftools")

# Read the PDF in a text vectors, each element is a page:
my_pdf <- pdf_text("article.pdf")
my_pdf

# Read the PDF metadata:
pdf_meta  <- pdf_info("article.pdf")
pdf_meta

# As you can see, the text would need quite a bit of cleaning.
# We won't use this one for further analyses...


# delete the objects we created:
rm(list = ls())

# ------~~ Reading in text files --------  

# Following a tradition in the field, let's analyze some political speech.

# The SOTU folder contains State of the Union presidential addresses
# from https://www.kaggle.com/rtatman/state-of-the-union-corpus-1989-2017
# They are stored in text files named "President_Year.txt"

# You can download the SOTU data used here from:
# http://kateto.net/css/sotu_data.zip

sotu_files <- list.files(path="./SOTU")
sotu_files

# Get the person's name from the file name:
sotu_pres <- sub("_.*", "", sotu_files)
sotu_pres

# Get the SOTU year from the file name:
sotu_year <- sub(".*_(.*).txt", "\\1", sotu_files)
sotu_year

# For a reminder on how we can find patterns in text using
# functions like sub() and grep(), look at our Week 5 code.

# Make a data frame with a column for president, year, and empty text.
sotu <- tibble(pres = sotu_pres, year = sotu_year, file = sotu_files, content = "")
sotu

# Sort by year: 
sotu <- arrange(sotu, year)
sotu

# Select speeches given after 1960:
sotu <- sotu[sotu$year>1960, ]
sotu

# The presidents included in our data:
unique(sotu$pres)

sotu_n <- nrow(sotu)
sotu_n

# Loop over each of the file names:
for(i in 1:sotu_n) {
  fn <- sotu$file[i]
  fn <- paste0("./SOTU/", fn)
  fl <- read_file(fn) 
  sotu$content[i] <- fl
}

sotu

# ------~~ Pre-processing the text --------  


# The steps are very similar to those we completed last week.
# Clean the data a bit:
sotu_tdy <- sotu
sotu_tdy$content <- replace_non_ascii(sotu_tdy$content)
sotu_tdy$content <- replace_contraction(sotu_tdy$content) 
sotu_tdy$content <- gsub("\'s", "", sotu_tdy$content)
sotu_tdy$content <- gsub("[0-9]+", "", sotu_tdy$content)

# Tokenize the text:
sotu_tdy <- unnest_tokens(sotu_tdy, input = content, output = term,
                          token = "words", format = "text",
                          to_lower = TRUE, strip_punct = TRUE)
dim(sotu_tdy)
sotu_tdy

# Get stop words:
data(stop_words)
stop_words 

# Add some SOTU-based words:
stopws <- c(stop_words$word, "america", "american", "americans", "americas")

# Remove the stop words:
sotu_tdy <- sotu_tdy[!(sotu_tdy$term %in% stopws), ]
dim(sotu_tdy)

# Lemmatize the remaining words in the data:
sotu_tdy$word <- lemmatize_words(sotu_tdy$term) 
sotu_tdy$word
sotu_tdy


# ------~~ Frequencies and associations --------  

# TOP TERMS overall

count(sotu_tdy, word, sort=TRUE) 

# Count how many times each word appears in the data:
word_counts <- count(sotu_tdy, word, sort=TRUE)
word_counts

# What are the top ten terms?
top_words <- word_counts[1:10,]
top_words

# Reorder the words for plotting (based on n rather than alphabetical)
top_words$word <- reorder(top_words$word, top_words$n)
top_words

ggplot(top_words, aes(x = n, y = word)) + 
  labs(title = "SOTU: Top frequency terms") +
  geom_col(fill="orange") + 
  geom_text(aes(label=n, x=n-150)) +
  theme_bw()


# TOP TERMS by PRESIDENT

# How about the top terms for each president?
# Count word/pres combinations (term mentions by each president):
pres_count <- count(sotu_tdy, pres, word, sort=TRUE)
head(pres_count)

# Group by president:
pres_count <- group_by(pres_count, pres) 

# Find the maximum frequency term for each president:
top_pres<- summarize(pres_count, top = max(n), word = word[which.max(n)],)
top_pres$pres <- reorder(top_pres$pres, top_pres$top)
top_pres

ggplot(top_pres, aes(x = top, y = pres)) + 
  labs(title = "SOTU: Top frequency term for each president") + 
  geom_col(fill="olivedrab") + 
  scale_x_continuous(limits=c(0, 400)) + 
  geom_text(aes(label=word, x=top+10), size=4, hjust = 0, color="black") +
  geom_text(aes(label=top), x = 5, size=4, hjust = 0, color="white") +
  theme_classic()

# How about the most distinctive terms per president?
# Get the tf-idf scores (term freq - inverse document freq)
pres_count <- bind_tf_idf(pres_count, term=word, document=pres, n=n)
pres_count

top_pres<- summarize(pres_count, top = max(tf_idf), word = word[which.max(tf_idf)],)
top_pres$pres <- reorder(top_pres$pres, top_pres$top)
top_pres

# Carter's 1981 speech abbreviates fiscal year to FY, which is why we see that as a term.
pres_count <- pres_count[pres_count$word!="fy",]

top_pres<- summarize(pres_count, top = max(tf_idf), word = word[which.max(tf_idf)],)
top_pres$pres <- reorder(top_pres$pres, top_pres$top)
top_pres


# ================  Ngrams  ================

# Instead of splitting our text into words, we can use n-grams:
# sequences of n words that appear together in the text.

# Going back to our initial data frame:
sotu 

# Instead of words, our selected tokens are now bigrams (ngrams, n=2)
sotu_bigr <- unnest_tokens(sotu, input = content, output = bigram,
                           token = "ngrams", n = 2, format = "text")
sotu_bigr

bigr_count <- count(sotu_bigr, bigram, sort=T)
bigr_count 

# Let's remove the stop words from the bigrams.
# We can extract separate columns for the two words using "separate":
bigr_sep <- separate(bigr_count, bigram, c("first", "second"), sep = " ")
bigr_sep

remove_row <- (bigr_sep$first %in% stopws) | (bigr_sep$second %in% stopws)
bigr_sep <- bigr_sep[!remove_row, ]


bigr_sep$first <- lemmatize_words(bigr_sep$first) 
bigr_sep$second <- lemmatize_words(bigr_sep$second) 
bigr_sep$bigram <- paste(bigr_sep$first, bigr_sep$second)
bigr_sep$bigram <- reorder(bigr_sep$bigram, bigr_sep$n) 
bigr_sep

ggplot(bigr_sep[1:10,], aes(x = n, y = bigram)) + 
  labs(title = "SOTU: Top frequency bigrams") +
  geom_col(fill="tomato") + 
  geom_label(aes(label=n)) +
  theme_bw()

# We can use those bigrams to create a term co-occurrence network:

library(igraph)

bigram_edges <- bigr_sep[bigr_sep$n>15, 1:2]
bigram_net <- graph_from_data_frame(bigram_edges)

plot(bigram_net, vertex.shape = "none", vertex.label.cex = 0.8, 
     vertex.label.color = "black", edge.arrow.mode = 0,
     edge.color = "red", layout = layout_with_kk   )


# ================  LDA topic models ================


# ------~~ Document term matrix --------  

# Before we get to topic modeling, we need to create a document term matrix.
# That is a table where each row is a document, each column is a term and
# the number in each cell shows how often each term appears in each document.

sotu_tdy$doc <- paste(sotu_tdy$year, sotu_tdy$pres, sep="-")

# Here is a long dataset with a column for document, term, and frequency.
sotu_freq <- count(sotu_tdy, doc, word)
sotu_freq

# Let's convert it into a document term matrix using cast_dtm()
sotu_dtm <- cast_dtm(sotu_freq, term=word, document=doc, value=n)
sotu_dtm

dim(sotu_dtm) # documents x terms
colnames(sotu_dtm) # terms
rownames(sotu_dtm) # documents

# For our planned topic modeling, we can speed things up by removing
# terms that show up in our texts too infrequently.
# Remove terms that are missing in 85% or more of the documents:
sotu_dtm <- tm::removeSparseTerms(sotu_dtm, sparse = 0.85)

# Note that here tm:: specifies that we want to use a function 
# from a package called "tm" that might not be currently loaded

# ------~~ Topic models --------  

library("topicmodels")


# Topic modeling -- trying to find k topics.

# 'k' is the number of topics, 'method' is the model fitting method.
# In the control list, 'iter' is the number of Gibbs iterations,
# we use 'seed' to set a random seed making sure we get the same 
# results every time we run this code (since it is non-deterministic)
sotu_lda_5 <- LDA(sotu_dtm, k = 5, method = "Gibbs", 
                  control=list(iter = 500, seed = 1111)) 

# Top 5 terms for each topic:
terms(sotu_lda_5, k=5)

# For each topic and each term, what would be the beta -- that is,
# the probability that this term would be generated for that topic?
# High beta --- the term shows more often within the topic.
# The function 'tidy' below turns various objects in tidy data frames'
beta_5 <- tidy(sotu_lda_5, matrix = "beta")
beta_5

# Let's get the top terms per topic again using the betas:
# (same top terms as before, just more convenient for plots)
beta_5 <- group_by(beta_5, topic)
top_beta_5 <- slice_max(beta_5, beta, n=10) 
top_beta_5$term <- reorder(top_beta_5$term, top_beta_5$beta)

# Plot top words by topic:
ggplot(top_beta_5, aes(x = beta, y = term, fill = factor(topic))) +   
  labs(title = "SOTU: Top words per topic (K=5)") + 
  facet_wrap( vars(topic), scales = "free_y", ncol=2) + 
  geom_col(show.legend = FALSE) +   
  theme_bw()


# Most likely topic for each SOTU address:
topics(sotu_lda_5, k=1)

# For each document and each topic, what would be the gamma --
# that is, the estimated proportion of terms in the document
# that may be generated by each topic we have identified.
# High gamma --- the document likely speaks more about the topic
# (in arrange below, -gamma means sort descending by gamma)
gamma_5 <- tidy(sotu_lda_5, matrix = "gamma")
gamma_5 <- arrange(gamma_5, topic, -gamma)
gamma_5

# For example, this suggests that over 50% of the words in 
# President Bush's 2002 & 2003 SOTU speech may have come from Topic 1


# Let's look at the way the 5 topics shift over time:
gamma_5$year <- sub("(.*)-.*", "\\1", gamma_5$document)
top_terms <- terms(sotu_lda_5, k=5) # Get the top 5 terms per topic
top_terms <- as.data.frame(top_terms)

# Create topic names by combining the top 5 terms of each topic together.
# Here, sapply() applies the same function to each column of top_terms.
# The function paste() with an argument collapse="-" combines the terms with dashes in between.
top_terms <- sapply(top_terms, paste, collapse="-")

# Add the topic name (top 5 terms) to each row of the gamma dataset:
gamma_5$top_terms <- top_terms[gamma_5$topic]
gamma_5

# Plot how much each topic appears by year (since each document
# corresponds to one year, SOTU is given yearly)
ggplot(gamma_5,  aes(x = year, y = gamma, group = factor(top_terms)) ) +  
  geom_line (linewidth = 1.5, aes(color = factor(top_terms))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# What is the topic distribution in Trump's 2018 SOTU speech?
trump18 <- subset(gamma_5, document == "2018-Trump")
trump18

ggplot(trump18, aes(x = gamma, y = top_terms)) +   
  labs(title = "SOTU: Topic distribution Trump 2018") +  
  geom_col(fill="orange") +   
  geom_text(aes(label= paste(round(gamma*100, 0), "%"), x = gamma-0.03)) +
  theme_bw()


# Let us try the modeling again and change the number of topics:
sotu_lda_12 <- LDA(sotu_dtm, k = 12, method = "Gibbs", 
                   control=list(iter = 500, seed = 1111)) 

# Most likely topic for each state of the union address:
topics(sotu_lda_12, k=1)

# Top 5 terms for each topic:
terms(sotu_lda_12, k=5)

# Top terms per topic for plotting
beta_12 <- tidy(sotu_lda_12, matrix = "beta")
beta_12 <- group_by(beta_12, topic)
top_beta_12 <-   slice_max(beta_12, beta, n=5) 
top_beta_12

# Plot top words by topic:
ggplot(top_beta_12, aes(x = beta, y = term, fill = factor(topic))) +   
  labs(title = "SOTU: Top words per topic (K=12)") + 
  facet_wrap( vars(topic), scales = "free_y", ncol=3) + 
  geom_col(show.legend = FALSE) +  
  scale_y_reordered() +
  theme_bw()



# ------~~ Topic model fit --------  

# So how do we decide which number of topics to select?
# (1) By looking at the topics and deciding what makes sense to us.
# (2) By looking at statistical measures of how good our model seems to be.

# One such statistic (there are many others) is 'perplexity'
# How "surprised" the model is at seeing the data. Smaller is better. 
# Low perplexity means the model does well predicting the sample.

perplexity(sotu_lda_5, sotu_dtm)

perplexity(sotu_lda_12, sotu_dtm)

# Here we are just using our full SOTU dataset.
# We could hold back part of our data, estimate the model, 
# and test perplexity on the data that was withheld.

# Another available measure: the 'topic coherence' of each topic.
# It looks at the top words for each topic (here we select top 6)
# and measures how often those top words appear together in documents.
tc_5 <- topic_coherence(sotu_lda_5, sotu_dtm, 6)
tc_5

tc_12 <- topic_coherence(sotu_lda_12, sotu_dtm, 6)
tc_12

# We can compare the average topic coherence between documents.
# Here a higher score (closer to zero) is better (more coherent)
# Coherence seems to correlate with human ability to 
# interpret topics, while perplexity does not.
mean(tc_5)
mean(tc_12)

# Now we can generate models with different number of topics
# and compare their 'perplexity' and 'coherence' metrics.

res <- tibble(k =  2:15, cohere = NA, perplex = NA)

for (i in 1:14) {
  ks <- res$k[i]
  sotu_lda <- LDA(sotu_dtm, k = ks, method = "Gibbs", 
                  control=list(iter = 500, seed = 1111)) 
  res$perplex[i] <- perplexity(sotu_lda, sotu_dtm)
  res$cohere[i] <-  mean(topic_coherence(sotu_lda, sotu_dtm, 6))
}

# Perplexity plot for different numbers of topics (k)
ggplot(res, aes(x=k, y = perplex)) + 
  labs(title = "SOTU: Perplexity per number of topics (k)") +
  scale_x_continuous(breaks=0:15) +
  geom_line (size = 0.6) +
  geom_point(size = 2, color = "tomato", alpha = 0.8) +
  geom_text(aes(label= round(perplex), y = perplex + 5), ) +
  theme_bw()  

# Coherence plot for different numbers of topics (k)
ggplot(res, aes(x=k, y = cohere)) + 
  labs(title = "SOTU: Coherence per number of topics (k)") +
  scale_x_continuous(breaks=0:15) +
  geom_line (size = 0.6) +
  geom_point(size = 2, color = "tomato", alpha = 0.8) +
  geom_text(aes(label= round(cohere,1), y = cohere + 0.2), ) +
  theme_bw()  

# ================ THE END ================



