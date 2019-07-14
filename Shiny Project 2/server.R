server <- function(input, output, session) {
  
  observe({
    #api validation at beginning of session
    cKey <- "Your CKey here"
    cSecret <- "Your cSecret here"
    accesstoken <- "Your accesstoken here"
    accesssecret <- "Your accesssecret here"
    setup_twitter_oauth(consumer_key = cKey, consumer_secret = cSecret, access_token = accesstoken, 
                        access_secret = accesssecret)
    
    twitteR:::setup_twitter_oauth(cKey,cSecret,accesstoken,accesssecret)  
    
  })
  
  observeEvent(input$button,{
    ntweets <- input$ntweets
    freqwords <- input$freqwords
    wcloud <- input$wcloud
    sometweets = searchTwitter("Stranger Things", n = ntweets, lang = "en") #Pulling tweets which mention Stranger Things
    tweets2.df <- twListToDF(sometweets) #COnverting to dataframe
    tweets2.df$originaltext <- tweets2.df$text # Copying original tweet for later use
    
    #Data Clean-up
    tweets2.df$text <- iconv(tweets2.df$text, "utf-8", "ascii", sub="")
    tweets2.df$text <- tolower(tweets2.df$text) 
    tweets2.df$text = gsub("\n","",tweets2.df$text)
    tweets2.df$text <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)","",tweets2.df$text)
    tweets2.df$text <- gsub("http[^[:blank:]]+", "", tweets2.df$text)
    tweets2.df$text <- gsub("@\\w+","",tweets2.df$text)
    tweets2.df$text <- gsub("[[:punct:]]","",tweets2.df$text)
    assign("tweets2.df", value = tweets2.df, envir = .GlobalEnv)
    output$text <- renderText("Data loaded successfully. Please click 'Populate Charts' button.")
  })
  
  observeEvent(input$button2,{  
    ntweets <- input$ntweets
    freqwords <- input$freqwords
    wcloud <- input$wcloud
    #Examining most frequent words
    myCorpus <- Corpus(VectorSource(tweets2.df$text))
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    myStopwords <- c(stopwords(language="en", source="smart"), "available", "via", "the", "ist", "edt", "utc", "pdt", "...")
    idx <- which(myStopwords == "very")
    myStopwords <- myStopwords[-idx]
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    myDtm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
    rowTotals <- apply(myDtm , 1, sum) #Find the sum of words in each Document
    myDtm <- myDtm[rowTotals > 0, ] #remove all docs without words
    freq.terms <- findFreqTerms(myDtm, lowfreq=freqwords)
    term.freq <- rowSums(as.matrix(myDtm))
    term.freq <- subset(term.freq, term.freq >= freqwords)
    df <- data.frame(term = names(term.freq), freq = term.freq)
    df$term <- iconv(df$term, "utf-8", "ascii", sub="") #if some non-UTF 8 chars then remove them
    maxwords <- nrow(df)
    
    #Sentiment Analysis
    word.df <- as.vector(tweets2.df$text)
    emotion.df <- get_nrc_sentiment(word.df)
    emotion.df2 <- cbind(tweets2.df$text, emotion.df) 
    sent.value <- get_sentiment(word.df)
    most.positive <- word.df[sent.value == max(sent.value)]
    most.negative <- word.df[sent.value <= min(sent.value)] 
    
    
    output$text <- renderText(paste("No. of tweets pulled:", ntweets))
    output$text2 <- renderText("The following table shows impact of text clean-up")
    output$text3 <- renderText(paste("Most positive tweet is: ", most.positive,"\n", "Most negative tweet is: ",most.negative))
    output$table <- renderTable(tweets2.df[1:10,c(1,17)])
    
    if (wcloud > maxwords){
      wcloud = maxwords
      updateSliderInput(session, "wcloud", value = wcloud)
      output$wcloudout <- renderWordcloud2(wordcloud2(df[1:wcloud, ], size = 0.5,
                                                      color = "random-light", backgroundColor = "grey"))
    } 
    else {
      output$wcloudout <- renderWordcloud2(wordcloud2(df[1:wcloud, ], size = 0.5,
                                                      color = "random-light", backgroundColor = "grey"))
    }
    
    output$sentiplot <- renderPlot(barplot(colSums(emotion.df2[,2:11]), main = "Overall Sentiment Analysis", col = rainbow(20)))
    output$freqplot <- renderPlot(
      ggplot(df, aes(x = reorder(term, freq), y = freq)) + geom_bar(stat = "identity") +
        xlab("Terms") + ylab("Count") + coord_flip()
    ) 
  })
}
    

