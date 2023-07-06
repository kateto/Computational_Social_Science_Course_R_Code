
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

# install.packages("readr")     # Data reading
# install.packages("tidyverse") # Data processing
# install.packages("tidytext")  # Text analysis  
# install.packages("textclean") # Text cleaning  
# install.packages("textstem")  # Word stemming
# install.packages("textdata")  # Sentiment lexicons
# install.packages("wordcloud") # Wordclouds


# ================  Cleaning and pre-processing  ================


library("readr")
library("tidyverse")
library("tidytext")
library("textclean") 
library("textstem")
library("textdata")
library("wordcloud")


# You may need to run this depending on your system settings to avoid 
# errors based on the language of your system. Run it if you get errors
# saying something is "invalid in this locale"
Sys.setlocale('LC_ALL','C')


# In today's class on text analysis, we will examine a well-known book:
# Alice’s Adventures in Wonderland, by Lewis Carroll

alice <- read_file("https://www.gutenberg.org/files/11/11-0.txt")
class(alice)
alice



# ------~~ Cleaning the text --------  

# First, let's take a look at some helpful functions 
# from the 'textclean' package that we can use here:
replace_contraction("He isn't going, don't go, I'm not.")
replace_number("Mambo number 5")
replace_symbol("@John: you owe me 10% which is $20")
replace_non_ascii("Some symbols: © µ ¼")
replace_html("<b> Hey! </b> &nbsp; &amp;")
replace_emoticon(":-) :-D :(")
replace_word_elongation("Nooooooooooooo")
replace_internet_slang("OMG lol gr8 w00t")

# Sometimes, we may also need to fix encoding problems:
txt <- ' \x93This is a quote\x94 ...!'
Encoding(txt)
Encoding(txt) <- "latin1"
txt

# During our web scraping week, we learned about using regular expressions:
# It may be a good idea to go back to and review the examples from that week.
# Brief reminder:
##  .       will match any character.
##  *       will match whatever came before it zero or more times.
##  +       will match whatever came before it one or more times.
##  ?       will match whatever came before it zero or one times.
##  [...]   will match any character in the []
##  [^...]  will match any character not in []
##  |       means OR as it usually does in R
##  ^       will match the start of the string.
##  $       will match the end of the string.
##  We can also match things like a new line (\n)

# Occasionally, various functions will give you errors because
# of particular problematic characters embedded in the text.
# [:graph:] matches any letter, number, or punctuation mark
# Below, we replace anything other than [:graph:] with space
gsub("[^[:graph:]]", " ", "Problem here: 👀👇💀") # Problem solved!


# Back to Alice in Wonderland!

# First, let's remove the text we do not need.
# We want to delete the text (table of contents, etc.) before CHAPTER I
# as well as the Project Gutenberg info text after the end of the book.
# Remember that we use sub() to replace patterns found in our text:
alice <- sub("^(.*)CHAPTER I.\r\n", "", alice)
alice <- sub("THE END(.*)$", "", alice)

# Notice that above \r\n are the end of line and new line symbols.

# Specify the text encoding:
Encoding(alice) <- "UTF-8"


# Replace non-standard symbols and expand contractions:
alice <- replace_non_ascii(alice)
alice <- replace_contraction(alice)

# This doesn't get rid of all possessive forms (Alice's)
# So let's just drop those manually:
alice <- gsub("\'s", "", alice)

# The text version of Project Gutenberg books uses underscore for emphasis
# Let's make sure to remove those underscores. 
alice<- gsub("_", "", alice)

# Split the text into chapters -- "strsplit" will split the text
# into separate elements every time it finds the string "CHAPTER ".
alice <- strsplit(alice, split="CHAPTER ")
alice <- unlist(alice) #strsplit returns a list
length(alice) # 12 chapters

# Create a data frame with chapter number and chapter text.
# The tidyverse packages like to use a "tibble" format -- those
# are fancier data frames. You could use data.frame() instead. 
alice_df <- tibble(chapter = 1:12, content = alice)
alice_df


# ------~~ Tokenizing --------  

# Tokenizing is the splitting of the text into "tokens" -- 
# various language units (e.g. terms, sentences, n-grams, etc.)

# Transform the text into a sequence of words, one per data row:
# "input" is the name of the column where our text is stored
# "output" is the name of the column where the words will be stored
# "token" can be words, characters, sentences, lines, ngrams, etc.
# "format" can be text, html, latex, xml, etc. (your data's format)
# "to_lower" if TRUE switches everything to lower case 
# "strip_punct" if TRUE removes punctuation

alice_tdy <- unnest_tokens(alice_df, input = content, output = term,
                           token = "words", format = "text",
                           to_lower = TRUE, strip_punct = TRUE)
dim(alice_tdy)
alice_tdy

# This operation splits the book into words and creates a dataset from them:
# one word per row, with a second colum specifying which document the word
# belongs to. In our case, we have 12 documents (each of the chapters)

# Note that unnest_tokens() does some of the work we might otherwise need to 
# do while pre-processing -- convert to lower case, remove punctuation, etc.



# ------~~ Removing stop words--------  


# Let's get the dataset of "stop words" included with tidytext.
# Those are commonly used words like "a", the" or "is".
# We often want to remove them and focus on important words.
# Note that this stop list includes contractions, which
# means we may not have to expand them if we use it.

# Get the stop words data from the tidytext package:
data(stop_words)
stop_words$word

# If you want a parsimonious stop word list that only excludes
# the most frequent ones, you can limit the lexicon you use:
stop_words$word[stop_words$lexicon=="snowball"]

# Which words in our dataset are stop words?
alice_tdy[alice_tdy$term %in% stop_words$word, ]

# Remove the stop words from our data:
alice_tdy <- alice_tdy[!(alice_tdy$term %in% stop_words$word), ]
dim(alice_tdy)


# ------~~ Stemming & lemmatizing --------  

# Stemming the words in our dataset to collapse multiple forms:
# This cuts off common word prefixes and suffixes, leaving stems.
# (for a large dataset, not efficient to do on raw data)
alice_tdy$stem <- stem_words(alice_tdy$term)
alice_tdy$stem 

# Lemmatization is a bit more sophisticated as a way to collapse
# multiple forms of the same words. It relies on dictionaries and 
# will always aim to produce a valid word. For example:
stem_words("studies")
lemmatize_words("studies")

alice_tdy$word <- lemmatize_words(alice_tdy$term) 
alice_tdy$word
alice_tdy

# ================  Term frequencies  ================

# How many times does each term appear?
# Below, we ask count() to look at dataset 'alice_tdy'
# and report how many times it finds each element of column 'term':
count(alice_tdy, term, sort=TRUE)
count(alice_tdy, word, sort=TRUE)


# ------~~ Top words --------  

# Count how many times each word appears in the data:
word_counts <- count(alice_tdy, word, sort=TRUE)
word_counts

# Words here are treated as a factor (categorical variable).
# Let's make sure the levels of that variable are ordered by n.
# The reason we care is because we want that order when plotting.
word_counts$word <- reorder(word_counts$word, word_counts$n)
word_counts

# What are the top ten terms in Alice?
top_words <- word_counts[1:10,]
top_words

ggplot(top_words, aes(x = n, y = word)) + 
  labs(title = "Alice: Top frequency terms") +
  geom_col(fill="tomato") + 
  geom_label(aes(label=n)) +
  theme_bw()

# Wordcloud of top terms:
wordcloud(word_counts$word, word_counts$n, max.words = 120)

# Top terms removing "Alice"
wordcloud(word_counts$word[-1], word_counts$n[-1], scale = c(2.5,.5), max.words = 60)


# ------~~ Top per chapter --------  

# How about the top terms for each chapter?

# Count word/chapter combinations (term mentions in each chapter):
chapter_count <- count(alice_tdy, chapter, word, sort=TRUE)
head(chapter_count)

# Remember that we can split our data into separate groups with group_by()
# Then, we can use summarize() to analyze and calculate things by group 

# Group by chapter so 'summarize' can find top terms in each group:
chapter_count <- group_by(chapter_count, chapter) 

# Summarize by finding the maximum number in each group and
# looking up the corresponding term name for that number:
top_chapter <- summarize(chapter_count, top = max(n), word = word[which.max(n)],)
top_chapter

ggplot(top_chapter, aes(x = top, y = factor(chapter))) + 
  labs(title = "Alice: Top frequency term per chapter") +
  scale_y_discrete(labels= paste("Ch.",1:12)) +
  geom_col(fill="olivedrab") + 
  geom_text(aes(label=word),size=5, x=5, color="white") +
  geom_text(aes(label=top, x=top-2), color="white") +
  theme_classic()


# ------~~ Selected term frequencies --------  

# Let's compare mentions of Alice, the queen and king over time.

my_terms <- c("alice", "queen", "king")
my_terms_count <- chapter_count[chapter_count$word %in% my_terms, ]

ggplot(my_terms_count, aes(x = chapter, y = n, color=word)) + 
  labs(title = "Alice: term frequency per chapter") +
  scale_x_continuous(breaks=1:12, labels= paste("Ch.",1:12)) +
  geom_line(linewidth = 1.5) + 
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("darkgray", "darkred", "orange"))  + 
  theme_bw()


# ------~~ tf_idf scores --------  

# Term frequency (TF) - inverse document frequency (IDF)
# tf-idf measures how important a term is to a document
# Terms are important to a document if they:
#   (1) Appear often in that document
#   (2) Appear rarely in other documents in that corpus

# the higher 'tf' is, the more often the term appears in the document
# the higher 'idf' is, the less often the term appears in all documents
#  tf-idf is calculated as tf * idf

# Let's get the most distinctive terms for each chapter.
chapter_count

# bind_tf_idf() takes a data frame with a column listing terms, 
# another column containing the document each term belongs to, 
# and a column for the number of times each term appears in the document

chapter_count <- bind_tf_idf(chapter_count, term=word, document=chapter, n=n)
chapter_count

# Group by chapter :
chapter_count <- group_by(chapter_count, chapter) 

# slice_max() gets the top 5 words from each group ordering by tf_idf:
top_tf_idf <- slice_max(chapter_count, order_by = tf_idf, 
                        n = 5, with_ties = FALSE)

# Reorder the word factor based on the word frequency:
# As before, we just do that so that it shows in the 
# right order on our plot below.
top_tf_idf <-  mutate(top_tf_idf, word = reorder(word, tf_idf))
top_tf_idf$chapter <- factor(top_tf_idf$chapter, levels=1:12,
                             labels = paste("Chapter", 1:12))
top_tf_idf

# Plot most important words by chapter using tf_idf score:
ggplot(top_tf_idf, aes(x = tf_idf, y = word, fill = chapter)) +   
  labs(title = "Alice: Most important words per chapter") + 
  facet_wrap( vars(chapter), scales = "free_y", ncol=2) + 
  geom_col(show.legend = FALSE) +  
  theme_bw()


# ================  Sentiment Analysis  ================

# The simplest way to do sentiment analysis is dictionary-based.
# We use dictionaries that link words to various emotions
# (e.g. positive/negative, joy, fear, anger, etc.)
# Counting the number or proportion of words linked to an emotion 
# in our text will give us  a score for each of those emotions.

# 'tidytext' and 'textdata' allow us to download several sentiment lexicons.
# afinn    -- just offers a single score ranging from negative to positive.
# bing     -- two categories, "positive" and "negative"
# nrc      -- has additional categories like "anger", "fear", "joy", etc.
# loughran -- has additional categories like "litigious", "uncertainty", "superfluous", etc.

afinn <- get_sentiments("afinn")
afinn

nrc  <- get_sentiments("nrc")
nrc
unique(nrc$sentiment)

# Before we get to sentiment analysis, let's make a slight detour.
# Let's learn about merging datasets in R using tidyverse functions.
# Below is a quick example of dataset merging using a shopping list.

shop_list <- data.frame(product=c("egg", "apple", "tomato"),
                        how_many=c(12, 3, 3))
prices <-  data.frame(product=c("apple", "tomato", "cookie"),
                      price=c(5, 10, 20))
shop_list
prices

inner_join(shop_list, prices) # keep if in both x and y
left_join(shop_list,  prices) # keep if in x
right_join(shop_list, prices) # keep if in y
full_join(shop_list,  prices) # keep if in x or y

# Now let's use those functions to add sentiment to our data.
# First, we'll add the 'afinn' score for each term in the data
# We will merge the two datasets matching them on "word".

alice_sen <- left_join(alice_tdy, afinn, by = "word")
head(alice_sen)

# Average sentiment score of the book:
mean(alice_sen$value, na.rm = T)

# Group by chapter so we can find chapter sentiment:
alice_sen <- group_by(alice_sen, chapter) 

# Summarize by averaging sentiment by chapter:
chap_pos_neg <- summarize(alice_sen, sentiment = mean(value, na.rm=T)) 
chap_pos_neg

# Our sentiment score finds the book to be rather negative:
ggplot(chap_pos_neg, aes(x = chapter, y = sentiment)) +  
  scale_x_continuous(breaks=1:12, labels= paste("Ch.",1:12)) +
  labs(title = "Alice: Chapter sentiment") + 
  geom_col(fill="tomato") +  
  theme_bw()


# ------~~ Emotions by chapter --------  

# How about sentiments that go beyond just positive/negative?
# Add columns for each sentiment/emotion included in 'nrc' to our data:
nrc_cats <- c("positive", "negative", "joy", "sadness", "anger", "fear", 
              "disgust", "surprise", "trust", "anticipation")
nrc_cats

# We'll use a loop that iterates over each sentiment in 'nrc'
# First, we'll get the list of words linked to a sentiment.
# Then, we'll add a column to our data which is 1 if a word is
# in the list for the current sentiment, and 0 of it is not.

alice_sen <- alice_tdy
alice_sen <- group_by(alice_sen, chapter) 

# Loop over each sentiment (e.g. run this once with sen="positive", then 
# with sen="negative", then "joy", "anger", and so on)

for(sen in nrc_cats) {
  
  # Get all word that have this emotion
  sen_words <- nrc$word[nrc$sentiment == sen]             
  
  # Make a new column named for the current sentiment (positive, joy, etc.)
  # Each element of this column is TRUE if it belongs to sen_words, FALSE otherwise.
  # To force R to turn the logical TRUE/FALSE vector into numbers, add + 0
  # This makes all TRUE values turn into 1, and all FALSE values into 0s
  alice_sen[, sen] <- alice_sen$word %in% sen_words + 0    
}

alice_sen

# Get the sum of words from each sentiment category by chapter:
chap_sen <- summarize(alice_sen, positive = sum(positive), negative = sum(negative),
                      joy = sum(joy), sadness = sum(sadness), anger = sum(anger),
                      fear = sum(fear), disgust = sum(disgust), 
                      surprise = sum(surprise), trust=sum(trust), 
                      anticipation = sum(anticipation) ) 
chap_sen


# Convert the data format from wide to long for plotting (remember from week 10!)
# Here, pivot_longer takes the name of our data, then the columns to be combined (cols),
# then the name of the new column where the combined names live (names_to)
sen_long <- pivot_longer(chap_sen, cols=nrc_cats, names_to = "sentiment")
sen_long$sentiment <- factor(sen_long$sentiment, levels = nrc_cats)
sen_long

# Plot sentiments by chapter:
ggplot(sen_long, aes(x = chapter, y = value, fill = sentiment)) +  
  scale_x_continuous(breaks=1:12, labels= paste("Ch.",1:12)) +
  labs(title = "Alice: Chapter sentiment") + 
  facet_wrap( vars(sentiment), ncol=2) + 
  geom_col(show.legend = FALSE) +  
  theme_bw()

# Plot sentiments within each chapter:
ggplot(sen_long, aes(x = sentiment, y = value, fill = sentiment)) +  
  labs(title = "Alice: Chapter sentiment") + 
  facet_wrap( vars(chapter), ncol=2) + 
  geom_col() +  
  theme_bw()


# ------~~ Top words per sentiment --------  


# What are the top words in the text for each sentiment?

# Convert the data format from wide to long:
alice_long <- pivot_longer(alice_sen, cols=nrc_cats, names_to = "sentiment")
alice_long

# For each word, we have all possible sentiments and whether they apply to it (1) or not (0)
# We'll drop the irrelevant sentiments (0s) and only keep the ones that do apply (1s)
alice_long <- alice_long[alice_long$value==1,-6]
alice_long$sentiment <- factor(alice_long$sentiment, levels = nrc_cats)
alice_long

# Count word/sentiment combinations:
top_sen <- count(alice_long, sentiment, word, sort=TRUE)
head(top_sen)

# Group by sentiment :
top_sen <- group_by(top_sen, sentiment) 

# Get the top 10 words from each group:
top_sen <- slice_max(top_sen, order_by = n, n = 10, with_ties = FALSE)

# Reorder the words based on their frequency:
top_sen <-  mutate(top_sen, word = reorder(word, n))

# Plot to words by sentiment:
ggplot(top_sen, aes(x = n, y = word, fill = sentiment)) +   
  labs(title = "Alice: Top words per sentiment") + 
  facet_wrap( vars(sentiment), scales = "free_y", ncol=2) + 
  geom_col(show.legend = FALSE) +  
  theme_bw()



# ================  Ngrams  ================

# Instead of splitting our text into words, we can use n-grams:
# sequences of n words that appear together in the text.

# Going back to our initial data frame:
alice_df 

# Instead of words, our selected tokens are now bigrams (ngrams, n=2)
alice_bigr <- unnest_tokens(alice_df, input = content, output = bigram,
                            token = "ngrams", n = 2, format = "text")
alice_bigr

bigr_count <- count(alice_bigr, bigram, sort=T)
bigr_count 

# Let's remove the stop words from the bigrams.
# We can extract separate columns for the two words using "separate":
bigr_sep <- separate(bigr_count, bigram, c("first", "second"), sep = " ")
bigr_sep

remove_row <- (bigr_sep$first %in% stop_words$word) | 
  (bigr_sep$second %in% stop_words$word)
bigr_sep <- bigr_sep[!remove_row, ]


bigr_sep$first <- lemmatize_words(bigr_sep$first) 
bigr_sep$second <- lemmatize_words(bigr_sep$second) 
bigr_sep$bigram <- paste(bigr_sep$first, bigr_sep$second)
bigr_sep <- mutate(bigr_sep, bigram = reorder(bigram, n))
bigr_sep

ggplot(bigr_sep[1:10,], aes(x = n, y = bigram)) + 
  labs(title = "Alice: Top frequency bigrams") +
  geom_col(fill="tomato") + 
  geom_label(aes(label=n)) +
  theme_bw()

# We can use those bigrams to create a term co-occurrence network:

library(igraph)

bigram_edges <- bigr_sep[bigr_sep$n>2, 1:2]
bigram_net <- graph_from_data_frame(bigram_edges)

plot(bigram_net, vertex.shape = "none", vertex.label.cex = 0.7, 
     vertex.label.color = "black", edge.arrow.mode = 0,
     edge.color = "red", layout = layout_with_kk   )



# ================ THE END ================



