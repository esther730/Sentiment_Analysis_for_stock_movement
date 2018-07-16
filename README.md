# Using Social Media as an Early Signal for Risk Management
## This capstone project is for the Statistics Master Program at UC Berkeley.
### In this project, we will use financial news from Reuters and Bloomberg between 2006 to 2013 to perform sentiment analysis, and use the sentiment outputs with machine learning models to predict stock movement. (Data referance: https://github.com/philipperemy/financial-news-dataset)

### Dictionary:
1. Harvard IV-4 Sentiment Dictionaries
2. Loughran and McDonald Financial Sentiment Dictionaries
3. Henry’s finance-specific dictionary
4. QDAP Dictionaries

### Paper Referance:
1. Using structured event to predict stock movement
2. Deep learning for event-driven stock prediction

### Models:
1. Random forest
2. SVM
3. Logistic Regression

### Stocks:
1. S&P500
2. Google
3. Walmart
4. Boeing


### Conclusions:
1. The movements are more predictable when stronger polarity
2. Positivity/Negativity of news will impact the market for 1~4 days
3. Sentiment polarity is positively correlated to SP500 movement
4. LM  and HE dictionaries play a more important role in deciding the stock movement
5. Random forest model works better than logistic regression model for risk management problem(lower false positive rate)
5. Sentiment Analysis at least works better than random guess or naïve guess for daily stock movement prediction(accuracy >58%)


![Screenshot](https://github.com/esther730/Sentiment_Analysis_for_stock_movement/blob/master/cluster1.PNG)

![Screenshot](https://github.com/esther730/Sentiment_Analysis_for_stock_movement/blob/master/corr.PNG)
