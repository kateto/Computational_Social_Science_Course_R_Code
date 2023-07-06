
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##



# ================  Web scraping in R   ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  


# install.packages("xml2")       # Process web content
# install.packages("rvest")      # Scrape web pages  


# ================  ~~ Getting started with rvest  ================

# Simple example to get us started. Let's make a web page:

my_html <-'<html>
            <head>
              <title>This is my page!</title>
            </head>
            
            <body>
             <p> Hello, this is a text paragraph.</p>
             <p style="color:red"> This is a paragraph styled to have text that is red!</p>
             <p id="giraffes"> This paragraph is probably about giraffes.</p> 
             <p> <a href="http://google.com"> This is a Google link!</a></p>
            </body>
            
            </html>'

write(my_html, "my_page.html") # save it as a file
browseURL("my_page.html")      # open in browser

# We can inspect the code of our page (or any web page!) using a browser 
# ( If you are using something other than Chrome or Firefox, you can google
#   "inspect element in" followed by the name of your browser to find out )
#
# Right-click anywhere on the page and select "Inspect" or press Ctrl/Cmd + Shift + I
# Press Ctrl/Cmd + Shift + C and select any element on the page. Look at its code.


library("rvest") 

# Get the web page into R using read_html() from package 'rvest'
# This gives us the page in a tree-like structure:
my_page <- read_html("my_page.html")

# Get the title (this will look for the "title" tag):
# html_text() will get the text inside that element.
my_title <- html_elements(my_page, css="title")
html_text(my_title)

# Get the entire body (looks for the "body" tag)
my_body <- html_elements(my_page, css="body")
html_children(my_body) # See what's inside the element

# Get the paragraphs from our page (the "p" tag):
my_paras <- html_elements(my_page, css="p")

# Get the text for each paragraph using html_text():
html_text(my_paras)

# Get all the links from our page (the "a" tag)
my_links <- html_elements(my_page, css="a")

# Get the link URLs from those links (in "href"): 
# The html_attr function gets attributes from an element.
html_attr(my_links, "href")

# Get the link text:
html_text(my_links)

# I can also find the element a specific id using #id:
html_elements(my_page, css="#giraffes")

# Pages are usually not that simple though. One way to deal with more complex 
# structures is using Xpath -- a standard that can help us navigate a 
# complicated document and identify the elements we're looking for.

# Here is the Xpath we can use to find the giraffe paragraph:
html_elements(my_page, xpath='//*[@id="giraffes"]')

# The Xpath above matches elements with attribute "id" that equals "giraffes"
# Note that the parameter itself contains double quotes ("giraffes"). 
# To prevent problems in R, we have to use single quotes around it ('')

# Don't worry if you don't know too much about html or xpath!
# We can find the info we need using a browser, as we'll see below.


# ================  ~~ Get movie info from IMDB  ================

# Get some movie information for "Star Wars: A new Hope" from IMDB:

sw_url <- "https://www.imdb.com/title/tt2488496"
sw_page <- read_html(sw_url)

# Next, the tricky part: find out where in the page we can find the content we need.
# Open the URL in your browser (this works at least in Chrome and Firefox)
# Right-click anywhere on the page and select "Inspect" or press Ctrl/Cmd + Shift + I
# Press Ctrl/Cmd + Shift + C and select the element you want to get.
# Right-click on the element and select Copy -> Copy Xpath. 

# Let's try to get the movie rating first.
# We can copy the xpath when we inspect the page:
sw_rating_1 <- html_elements(sw_page, xpath='/html/body/div[2]/main/div/section[1]/section/div[3]/section/section/div[2]/div[2]/div/div[1]/a/div/div/div[2]/div[1]/span[1]')
html_text(sw_rating_1)

# Or, in our inspection, we may notice that the number we want is enclosed in a tag
# which seems to have a specific attribute. We can ask for elements with that attribute.
sw_rating_2 <- html_elements(sw_page, xpath='//*[@class="sc-7ab21ed2-1 eUYAaq"]')
html_text(sw_rating_2)     

# Get the cast of the movie:
sw_cast <- html_elements(sw_page, xpath='//*[@data-testid="title-cast-item__actor"]')
html_text(sw_cast) 

# Get the roles they each play:
sw_cast <- html_elements(sw_page, xpath='//*[@class="sc-bfec09a1-4 llsTve"]')
html_text(sw_cast) 

# Get images on the page:
sw_pics <- html_elements(sw_page, xpath='//*[@class="ipc-image"]')

# Just like link URLs are stored in attribute "href", image URLs are stored in "src":
sw_pics_links <- html_attr(sw_pics, "src")

# Get the first image -- the movie poster:
sw_poster_link <- sw_pics_links[1]
sw_poster_link

# We can take a look at the image in a browser just to be sure:
browseURL(sw_poster_link)

# We could also download the image:
download.file(sw_poster_link,'poster.jpg', mode = 'wb')


# ================  ~~ Getting a table from Wikipedia  ================

# Say we need the list of Senate committees and their chairs.
# We decide to get those from Wikipedia:

wiki_url <- "https://en.wikipedia.org/wiki/117th_United_States_Congress"

# Get the page we need using the read_html function from rvest:
wiki_page <- read_html(wiki_url)

# Inspect the page in your browser (press Ctrl/Cmd + Shift + I)
# Press Ctrl/Cmd + Shift + C and click on the table you want to get.
# (or, click on the first column and look for <table> above it in the code)
# Right-click on the table element and select Copy -> Copy Xpath.  

wiki_table <- html_elements(wiki_page, xpath = '/html/body/div[1]/div/div[3]/main/div[2]/div[3]/div[1]/table[8]')

# We can now use html_table() to get our data. The function assumes that headers 
# are in the top row, and that no cell can span multiple rows.

wiki_df <- html_table(wiki_table)
wiki_df

# We could also opt to get all tables from the page:
wiki_tables <- html_elements(wiki_page, xpath = '//*[@class="wikitable"]')

# And select the ones we want:
wiki_df <- html_table(wiki_tables[4])[[1]]
wiki_df


# Let's make sure that our data is in a data frame format:
wiki_df  <- as.data.frame(wiki_df)
View(wiki_df)

# ================  ~~ Pattern matching detour  ================


# Success! We have our table. Now we need to clean it up a bit 
# before we can use it. To do this, we need to know something 
# about pattern matching and regular expressions.

# Finding and replacing patterns in a string:

cat_or_bat <- c("bat", "rat", "cat", "caaaat", "ct", "the cat", "the bobcat", "cats")

# Does the word "cat" appear in any items of my_text?
grep("cat", cat_or_bat)   # returns the position of those items 
grepl("cat", cat_or_bat)  # returns true/false for each item

# Return the items where you do find "cat" with 'value=TRUE':
grep("cat", cat_or_bat, value=TRUE)

# Replace "cat" with "dog":
sub("cat", "dog", cat_or_bat)  # sub: replace the first match
gsub("cat", "dog", cat_or_bat) # gsub: replace all matches

# Note that sub() and gsub() do not change the original object:
cat_or_bat

# We need to do something like:
dog_or_bat <- gsub("cat", "dog", cat_or_bat)
dog_or_bat

# Instead of looking for a specific word, we can also search for a pattern.
# We can do more complicated pattern searches using regular expressions:
# .       will match any character. ".at" could be "cat", "bat", "rat", "mat", "vat" etc.
# *       will match whatever came before it zero or more times. "ca*t" matches "ct", "cat", "caaaat", etc.  
# +       will match whatever came before it one or more times. "ca+t" matches "cat", "caaaat", but not "ct"
# ?       will match whatever came before it zero or one times. "ca?t" matches "ct" or "cat", but not "caaat"
# [...]   will match any character in the [], so "[cb]at" matches only "cat" and "bat"
# [^...]  will match any character not in [], so "[^cb]at" matches "rat", "mat", etc. but NOT "cat" ot "bat"
# |       means OR as it usually does in R, so "cat|bat" will match either "cat" or "bat".
# ^       will match the start of the string. "^cat" will match "cats" but not "the cat"
# $       will match the end of the string. "cat$" will match "the cat" but not "cats"
# \\<     will match the beginning of a word. "\\<cat" will match "a cat" but not "bobcat"
# \\>     will match the end of a word. "cat\\>" will match "a cat", "bobcat lane", but not "cats"
# \\b     will match the edges of the word (start or end) -- so we can ask for "cat\b", "\bcat", or "\bcat\b"
# \\B     will match stuff not at the edge of a word. So "\\Bcat" matches "the bobcat" but not "the cat"
# We can also match things like a new line (\n) 
# For a longer regex cheatsheet, check out https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

grep(".at", cat_or_bat, value=TRUE)
grep("ca?t", cat_or_bat, value=TRUE)
grep("ca+t", cat_or_bat, value=TRUE)
grep("ca*t", cat_or_bat, value=TRUE)
grep("[cb]at", cat_or_bat, value=TRUE)
grep("[^cb]at", cat_or_bat, value=TRUE)
grep("rat|bat", cat_or_bat, value=TRUE)
grep("^cat", cat_or_bat, value=TRUE)
grep("cat$", cat_or_bat, value=TRUE)
grep("\\<cat", cat_or_bat, value=TRUE)
grep("cat\\>", cat_or_bat, value=TRUE)
grep("cat\\b", cat_or_bat, value=TRUE)
grep("\\Bcat", cat_or_bat, value=TRUE)


# Remember that capitalization matters in R!
grep("Cat", cat_or_bat, value=TRUE) # None found!

# But, we can tell grep to ignore capitalization:
grep("Cat", cat_or_bat, value=TRUE, ignore.case= TRUE)

# More examples of finding and replacing patterns in a string:

my_text <- c("Here comes the sun", "Walking on sunshine", 
             "Dark side of the moon!", "A great pun...")

# Does the word "sun" appear in any items of my_text?
grep("sun", my_text)   # returns the position of those items 
grepl("sun", my_text)  # returns true/false for each item
grep("sun", my_text, value=TRUE) # Returns the actual items

# Replace "sun" with "shoe":
sub("sun", "shoe", my_text)  # sub: replace the first match
gsub("sun", "shoe", my_text) # gsub: replace all matches

# An example where sub and gsub give different results:
sub("s", "X", my_text)  # sub: replace the first match
gsub("s", "X", my_text) # gsub: replace all matches

# Find all items that contain "un" 
grep("un", my_text, value=TRUE)

# Find all items that contain a 3-letter word ending in "un" (sun, pun, bun, etc)
grep("\\<.un\\>", my_text, value=TRUE)

# Find whatever word follows "the " in our text and replace it with "cheese".
# Note that below ".+\\>" matches any symbol (".") one or more times ("+"),
# until the end of a word ("\\>")
gsub("the .+\\>", "the cheese", my_text)

# We may want to search for a symbol that has a special meaning like ".", "?", ")", or "]"
# To do that, we use backslashes to "escape" it -- show that we want the actual symbol: 
grep(".", my_text, value=TRUE)    # . here means we want to match any symbol
grep("\\.", my_text, value=TRUE)  # \\. here means we want the actual period


# Find whatever word follows "the " in our text and repeat it twice:
# We can use parentheses to enclose some part of the text that we want to use later.
# Afterwards, we can get back whatever was enclosed in () by using \\1, \\2, etc.
# \\1 gets us what was in the first pair of parentheses, \\2 the second, etc.
# The code below finds the pattern in (), then refers to it as \\1 later.

gsub("the (.+\\>)", "the \\1 \\1", my_text)

# Find the last word of each string and move it to be first.
# Below, ^(.+)\\< gets us the first word, and \\<(.+)$ the last one.

gsub("^(.*)\\<(.+)$", "\\2 \\1", my_text)

# Remember that ^ means the start of the string, $ the end of the string;
# while <// means the start of a word, //> the end of a word. 

# Finally, if we wanted to append something to all of our strings,
# we could do it using paste() with parameter "sep" for the separator
# between the strings:
paste(my_text, "Yay!", sep=" --- ")
paste("SUN ", my_text, sep=": ")

# If we wanted to combine all of our strings together into one string,
# we can also do that with paste() by using 'collapse' instead of 'sep'
paste(my_text, collapse="; ")


# ================  ~~ Cleaning our table  ================

# Back to our table...

wiki_df$Chair

# Get the names of the chairs 
# get all symbols until an opening parenthesis \\( and use them to replace the whole string:
wiki_df$Chair_name <- gsub("(.+) \\(.*", "\\1", wiki_df$Chair)

wiki_df

# Get first and last name:
# Remember that [^ ] means anything that's not " ":
wiki_df$Chair_first_name   <- gsub("([^ ]+) (.*)", "\\1", wiki_df$Chair_name)
wiki_df$Chair_last_name    <- gsub("([^ ]+) (.*)", "\\2", wiki_df$Chair_name)

wiki_df

# Get party and state:
# We find the text in parentheses using \\(  \\)
# Then we split it in two: first one (.) 1 symbol; the second (..) is 2 symbols
wiki_df$Chair_party        <- gsub(".+ \\((.).(..)\\)", "\\1", wiki_df$Chair)
wiki_df$Chair_state        <- gsub(".+ \\((.).(..)\\)", "\\2", wiki_df$Chair)

wiki_df



# ================  ~~ Scraping from multiple pages  ================


# Get info about all articles from an issue of the Journal of Communication

issue_url <- "https://academic.oup.com/joc/issue/72/6"
issue_page <- read_html(issue_url)

# Let's take a look at the issue page...
browseURL(issue_url)


# Notice below we ask for an element with the specific title class we found on the page
# and then we add "/a" at the end gets us the elements nested inside it (i.e. the link)
pub_links <- html_elements(issue_page, xpath='//*[@class="customLink item-title"]/a')
pub_txt <-   html_text(pub_links)
pub_urls <-  html_attr(pub_links, "href")

# Note the links are local, so we need to add the starting part
# to each link -- in this case, "http://academic.oup.com"
# We can do that with paste() which combines strings separated with 'sep':
pub_urls <- paste("http://academic.oup.com", pub_urls, sep="")


# Let's look at the first link:
pub_page <- read_html(pub_urls[1])

browseURL(pub_urls[1])

# Get the page title (after inspecting with your browser)
pub_title <- html_elements(pub_page, xpath='//*[@class="wi-article-title article-title-main accessible-content-title at-articleTitle"]')
pub_title <- html_text(pub_title) # Get the text from the title
pub_title <-  trimws(pub_title)    # trimws() removes blank spaces and new lines from the text
pub_title

# Get the authors:
pub_author <- html_elements(pub_page, xpath='//*[@class="linked-name js-linked-name-trigger"]')
pub_author  <- html_text(pub_author) # Get the text from the author list elements
pub_author <-  paste(pub_author, collapse=", ") # Combine all author names, separated by comma
pub_author 

# Get the abstract:
pub_abstract <- html_elements(pub_page, xpath='//*[@class="abstract"]')
pub_abstract  <- html_text(pub_abstract) # Get the text from the abstract element
pub_abstract 

# Now let's do the same thing, but for all articles in the issue.
# We'll use loop: we'll go over each of the article URLs one by one.
# The loop will iterate over numbers, and get the first 5 articles.

# We'll store our results in this list (currently empty):

pub_data <- data.frame(N=1:6, Title="", Authors="", Abstract="")
pub_data

for( url_number in 1:6) {
  
  pub_page <- read_html(pub_urls[url_number])
  
  pub_title <- html_elements(pub_page, xpath='//*[@class="wi-article-title article-title-main accessible-content-title at-articleTitle"]')
  pub_title  <- html_text(pub_title)  
  pub_title <-  trimws(pub_title)     
  
  pub_data$Title[url_number] <- pub_title
  
  # Get the authors:
  pub_author <- html_elements(pub_page, xpath='//*[@class="linked-name js-linked-name-trigger"]')
  pub_author  <- html_text(pub_author)  
  pub_author <-  paste(pub_author, collapse=", ")
  
  pub_data$Authors[url_number]  <- pub_author
  
  # Get the abstract:
  pub_abstract <- html_elements(pub_page, xpath='//*[@class="abstract"]')
  pub_abstract  <- html_text(pub_abstract)  
  
  pub_data$Abstract[url_number] <- pub_abstract
  
}

View(pub_data)


# ================  ~~ Things that will go wrong  ================


# Websites frequently change, so script that works fine today
# may not work tomorrow. Code often breaks over time, but that
# is much more likely to happen when you're scraping web content.

# Many websites are not too happy to get their content scraped.
# Some may specifically prohibit it in their terms of service.
# Many may block you temporarily and ask you to solve a CAPTCHA
# before they let you access the site again. When that happens,
# you code will stop working temporarily as well.

# Finally, do remember that there is simply no need to scrape many 
# popular websites. They often have official APIs that will give you 
# access to their content. That is true of Wikipedia, IMDB, Reddit,
# news websites like the New York Times, etc. 


# ================  ~~ Bonus content: bibliometrics  ================ 

# Remove the comment and run the line below to install the package
# install.packages("bibliometrix")

library("bibliometrix") 

# Available bibliographic databases:
# SCOPUS, Clarivate Analytics Web of Science, RISmed PubMed/MedLine,
# and the Cochrane Database of Systematic Reviews (CDSR)

# Quick example using the Web of Science
# Go to www.webofscience.com (you may have to register)
# Conduct a search for articles using any parameters of interest
# Once you've found the articles, click "EXPORT" at the top.
# Export all the articles/data you want as BIBTEX or plain text.


dat <- convert2df(file = "joc_networks.bib", 
                  dbsource = "wos", 
                  format = "bibtex")

dat$AU # Authors
dat$TI # Title
dat$PY # Publication year
dat$SO # Publication source
dat$DT # Document type
dat$AB # Abstract
dat$DE # Author keywords
dat$CR # Cited references
dat$TC # Times cited
dat$affiliations # Author affiliations


# Descriptive analysis of the data:

bib_desc <- biblioAnalysis(dat)

# Here, pause = T stops the printing after each table until we press "Enter"
summary(bib_desc,  pause = T)

plot(bib_desc, pause = T) # you might get errors for some plots, ignore them.


# Descriptive analysis results include (among other things): 

bib_desc$Articles      # Total N of articles
bib_desc$nAuthors      # Total N of authors
bib_desc$Appearances   # Total N of author appearances

bib_desc$Authors       # Articles per author
bib_desc$AuthorsFrac   # Percent articles per author

bib_desc$nAUperPaper   # N of authors per manuscript
bib_desc$Years         # Publication years of manuscripts

bib_desc$DE            # Frequency of author-provided keywords
bib_desc$ID            # Frequency of system-provided keywords

bib_desc$Sources              # Publication sources
bib_desc$Countries            # Countries (from author affiliation)
bib_desc$CountryCollaboration # Single country (SCP) or multi-country (MCP) 

bib_desc$TotalCitation        # Total citations per manuscript
bib_desc$TCperYear            # Average annual citations per manuscript
bib_desc$MostCitedPapers      # Most cited manuscripts


# Citations included in the data:

# The parameter 'field' can be "author" or "article":
cited_papers <- citations(dat, field = "article")
cited_p <- as.data.frame(cited_papers$Cited )
cited_p$year <- cited_papers$Year
cited_p$source <- cited_papers$Source

View(cited_p)


cited_authors <- citations(dat, field = "author")
cited_a <- as.data.frame(cited_authors$Cited )
cited_a$year <- cited_authors$Year
cited_a$source <- cited_authors$Source

View(cited_a)


# Citation networks -- we'll talk more about networks in the coming weeks!

# Types of analysis to produce the network ties include:
#  collaboration -- authors working together
#  coupling -- having shared citations in common
#  co-citation -- being cited together
#  co-occurrences -- e.g. co-occurrences of key words


net <- biblioNetwork(dat, analysis = "coupling", 
                     network = "authors")

# Plot similarity among the first 20 authors in the data
networkPlot(net, n = 20, 
            Title = "Author similarity", 
            remove.multiple=TRUE )




# ================ THE END ================










