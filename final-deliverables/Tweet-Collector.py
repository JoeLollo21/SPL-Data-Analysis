# Tweet Collector
# Using the twarc package and Twitter API

# Install and configure twarc package
!pip install twarc
!pip install plotly

# Run this command by itself using your Twitter API Developer ID.
# To apply for a Twitter Developer ID, go here: 
# https://developer.twitter.com/en/docs/twitter-api/getting-started/getting-access-to-the-twitter-api  
!twarc2 configure
# Happy twarcing!

# Get Tweets and Metadata
# To collect tweets and associated metadata, we can use the command twarc2 search and insert a query. 
# If you have an Academic Research account, you can collect tweets from the entire archive with the flag --archive.
!twarc2 search "(\"james baldwin\")" --archive --limit 30 --tweet-fields "text"

# Save Tweets to a CSV file.
!twarc2 search "(\"james baldwin\")" --archive > baldwin_tweets.csv
