
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##



# A useful resource for APIs available to social scientists:
# https://bookdown.org/paul/apis_for_social_scientists/


# Packages we would need today:
# (remove # and run this if they are not already installed)  


# install.packages("rtweet")           # Get Twitter data
# install.packages("RedditExtractoR")  # Get Reddit data
# install.packages("archiveRetriever") # Get Internet Archive data
# install.packages("igraph")           # Work with networks
# install.packages("wordcloud")        # Create word clouds 
# install.packages("tm")               # Text analysis package



# ================  Twitter  API access in R   ================


# Note that the most recent version of 'rtweet' as of Feb 2023 has a bug
# which can prevent proper authentication. To avoid it, install using:

install.packages("rtweet", repos = 'https://ropensci.r-universe.dev/') 


# We will focus on the search options which currently work.
# The filtered streaming may or may not be available for free.


# ================  ~~ Getting authenticated with Twitter ================


library(rtweet)

# USING YOUR TWITTER ACCOUNT & THE RTWEET APP
# This will look up the account currently logged in from your browser.
# A browser window will open up and ask you to confirm authorizing this app.

my_auth <- auth_setup_default()
auth_as(my_auth)


# USING YOUR OWN APP 
# To get better rate limits you can create your own twitter app.
#  1. Create a new Twitter account (or use an existing one if you want)
#  2. Go to https://developer.twitter.com/en/portal/petition/use-case 
#  3. Log in and apply for a developer account (student, no data posting)
#  3. Go to https://developer.twitter.com/en/apps click on "Create App"
#  4. Name the app. Once created, copy the "Bearer token" from "Keys and Tokens"
# Run the code below and paste your token in the window that pops up.

my_auth <- rtweet_app()
auth_as(my_auth)


# For today's class, you can use authorization from one of my apps:
load("auth_ko")
auth_as(my_auth)



# ================  ~~ Twitter search data ================


# Get tweets from search: returns tweets from the last ~7 days
# The rate limit used to be ~18,000 tweets every 15 minutes, 72,000 per hour.
# That seems to have changed, with lower rate limits now imposed on free access. 

# Searches for all the included words -- so both "rutgers" and "news". 
# If we wanted one or the other, we could use "rutgers OR news"
# If we wanted the exact phrase, we could use '"rutgers news"'
# (basically put in the q parameter whatever it is you'd put in the 
# Twitter search box in order to find the tweets you want to collect)

# Below, n is the number of tweets we want to get (if available)
# include_rts specifies if we want to include retweets in the results

ru_tweets <- search_tweets(q = "rutgers news",
                           n = 50, 
                           include_rts = FALSE, 
                           language = "en")

# Take a look at the data:
head(ru_tweets)
colnames(ru_tweets)
dim(ru_tweets)

# Plot the data over time:
ts_plot(ru_tweets, by="hours")

# Look at the text of the tweets
ru_tweets$text

# Get URLs from tweet data:
ru_urls   <-  entity(ru_tweets, "urls")
ru_urls$expanded_url

# Get mentions from tweet data
ru_mention <- entity(ru_tweets, "user_mentions")
ru_mention$name


# Get clean text removing mentions, hashtags, urls, and media:
clean_tweets(ru_tweets, clean = c("users", "hashtags", "urls", "media"))

# We'll do text processing in the second half of the term
# But just for fun, let's take a look at a word cloud
# of terms appearing in tweets more than 3 times
# (later we'll talk about cleaning up the text)

library(wordcloud)
wordcloud(ru_tweets$text, min.freq=3)


# We'll also work more with networks later this semester. 
# So note that the rtweet package lets us quickly create a network based 
# on our preferred relationship types -- mention, retweet, reply, and quote.

ru_net <- network_graph(ru_tweets, c("mention", "retweet", "reply"))

library(igraph)
plot(ru_net, vertex.shape="none", vertex.label.color="gray40",
     vertex.label.cex=.7, edge.arrow.size=.2, layout=layout_in_circle)


# The rtweet package can also give us the tweet author data:
# Note that in the resulting data, each row has information about
# the person who authored the tweet on the same row of ru_tweets:
ru_users <- users_data(ru_tweets)
head(ru_users) 

# If a person authored more than one tweet in our data, 
# they may appear in the users data more than once:
ru_users$screen_name

# And now unique users:
unique(ru_users$screen_name)

# For large numbers of tweets, we would need to set parameter retryonratelimit=TRUE
# Doing that forces R to try again if we reach Twitter rate limits.
# For example (don't run now!):
#
# tweets <- search_tweets(q = "politics",
#                         n = 500000,
#                         include_rts = FALSE,
#                         retryonratelimit=TRUE)

# Return only tweets sent from the president's account @POTUS:

pres_tweets <- search_tweets(q = "from:POTUS",
                             n = 20 )
head(pres_tweets) 
dim(pres_tweets)
pres_tweets$text

wordcloud(pres_tweets$text, min.freq=2, scale=c(2,.4))


# Return only tweets sent to @POTUS or to @CNN:
# (note those are replies, if we wanted mentions
# we could simply search for "@POTUS OR @CNN")
cnn_tweets <- search_tweets(q = "to:POTUS OR to:CNN",
                            n = 20 )
head(cnn_tweets) 
dim(cnn_tweets)
cnn_tweets$text

wordcloud::wordcloud(cnn_tweets$text, min.freq=2,scale=c(3,.8))


# Useful additional specifications in your search 
# (based on search operators Twitter offers):
#
# Return only tweets with links:         "filter:links"
# Return only tweets with links to news: "filter:news"
# Return only tweets with media:         "filter:media"
# Return only tweets with video:         "filter:video"
# Return only verified tweets:           "filter:verified"
# Exclude verified tweets                "-filter:verified
# Exclude retweets:  "-filter:retweets"
# Exclude quotes:    "-filter:quote"
# Exclude replies:   "-filter:replies" 
# Minimum X number of replies:   "min_replies:X"
# Minimum X number of likes:     "min_faves:X"
# Minimum X number of retweets:  "min_retweets:X"


news_tweets <- search_tweets(q = "covid filter:news min_retweets:100",
                             n = 20, 
                             include_rts = FALSE, 
                             language = "en")
head(news_tweets) 
dim(news_tweets)


# Tweet text:
news_tweets$text
wordcloud(news_tweets$text, min.freq=2)

# News URL:
news_urls <- entity(news_tweets, "urls")
news_urls$expanded_url

# Favorite & retweet counts
hist(news_tweets$favorite_count)
hist(news_tweets$retweet_count)

# Tweets with over 1,000 retweets:
news_tweets[news_tweets$retweet_count>1000,  "text" ] 

# Both in the streaming API and in the search API, we can request Tweets
# based on geographic location. For example, we can specify longitude & latitude 
# for a point on the map along with a radius around it.

# Let's try to get some tweets from within 50 miles of Rutgers:

ru50_tweets <- search_tweets(q="",  
                             geocode= "40.5051,-74.4530,50mi",
                             n = 20, 
                             include_rts = FALSE,  
                             language = "en")

head(ru50_tweets) 
dim(ru50_tweets)
ru50_tweets$text


# ================  ~~ Twitter users ================

# The Twitter API lets us get the most recent tweets from a user's timeline
# The limit is a maximum of 3,200 tweets.
# Parameter home=FALSE gets user timeline; TRUE gets their home feed.

rr_tweets <- get_timeline(user="VancityReynolds",
                          n=20,
                          home=FALSE )

rr_tweets$text
wordcloud::wordcloud(rr_tweets$text, min.freq=2, scale=c(2,.4))


# Multiple users' timelines:
media_tweets <- get_timeline(user= c("cnn", "foxnews"),
                             n=20,
                             home=FALSE )
media_tweets$text


# Get the users followed by the president's @POTUS account:
# Note that Twitter's default limit for following is 5,000

pres_fr <- get_friends(users="POTUS", n=100)

pres_fr

# Note that the accounts are returned as user IDs. 
# We can use those go get user data:

pres_fr_acc <- lookup_users(pres_fr$to_id)

pres_fr_acc

pres_fr_acc$name 


# Get the users who follow the president on Twitter:
# Remember, if you want to get a large number of users/followers
# you would need to include parameter retryonratelimit = TRUE  
# and be prepared to wait for a longer time.

pres_fol <- get_followers(user="POTUS", n=20)

pres_fol_acc  <- lookup_users(pres_fol$from_id)

pres_fol_acc

pres_fol_acc$name 


# Search for users based on their profile:

ru_user <- search_users("rutgers", n = 20)

# Profile description
ru_user$description



# ================  Reddit API in R ================

library("RedditExtractoR")

# Find subreddits about Rutgers:
ru_red <- find_subreddits("rutgers") 

ru_red$subreddit
ru_red$title

# Get threads from a subreddit:
# Reddit offers sorting by "hot", "new", "top", and "rising"
# Period could be hour, day, week, month, year, etc. or all
ru_threads <- find_thread_urls(subreddit="rutgers", 
                               sort_by="top", 
                               period="week")


# Get threads that mention communication:
comm_threads <- find_thread_urls(keywords = "communication",
                                 subreddit="rutgers", 
                                 sort_by="top", 
                                 period="all")
comm_threads$title

# Thread with most comments:
comm_threads$title[which.max(comm_threads$comments)]


# Get the content of a thread, including comments:
comm_content <- get_thread_content(comm_threads$url[1:10])

# Thread data:
threads <- comm_content$threads
names(threads)

# Comment data:
comments <- comm_content$comments
names(comments)

# Get information and content about specific users:
# e.g. we could get threads$author

users <- get_user_content("nasa")

# We get a list with each user as an element.
# For each user, we have three elements:

# Information about the user
nasa_about <- users$nasa$about
nasa_about

# Threads by the user:
nasa_threads <- users$nasa$threads
nasa_threads[1:5,]

# Comments by the user
nasa_comments <- users$nasa$comments
nasa_comments[100,]



# ================  Internet Archive API ================


library("archiveRetriever") 

# Calendar showing when the page was archived 
# within a startDate to endDate period

archive_overview(homepage = "www.nytimes.com", 
                 startDate = "2010-01-01", 
                 endDate = "2010-02-01")

# What are the URLs for a particular page recorded by the archive
# within the selected time period we are interested in?
nyt_urls <- retrieve_urls("www.nytimes.com/section/politics", 
                          startDate = "2020-01-01", 
                          endDate = "2020-01-10")
nyt_urls

# We will discuss web scraping more next week!
# For now, let's just say the following code will get us data
# on the article titles and teaser text from the NYT front page.
nyt <- scrape_urls( Urls = nyt_urls,
                    Paths = c(title = "//article/div/h2//text()", 
                              teaser = "//article/div/p/text()"), 
                    collapse = FALSE, 
                    archiveDate = TRUE)

nyt


# ================ THE END ================






