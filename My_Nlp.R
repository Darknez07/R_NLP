# Import dataset
dataset_orig = read.delim('Restaurant_Reviews.tsv', quote = "",
                     stringsAsFactors = FALSE )
# Cleaning the texts
# install.packages('tm')

library(tm)
library(SnowballC)
library(textstem)

# Construct the corpus

corpus = VCorpus(VectorSource(dataset$Review))

# Putting all in lower case
# Each of the words should be unique

corpus = tm_map(corpus, content_transformer((tolower)))

# Remove numbers from the data
# Indeed helpful

corpus = tm_map(corpus, removeNumbers)

## Remove puntuation

corpus = tm_map(corpus, removePunctuation)

## Remove stopwords

corpus = tm_map(corpus, removeWords, stpword)

# Here is stemmed words

corpus = tm_map(corpus, stemDocument)

corpus = tm_map(corpus, lemmatize_words)


corpus = tm_map(corpus, stripWhitespace)

# Creating bag of words
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)

# Build classification model

library(randomForest)
library(caTools)
set.seed(123)


# Get independent and dependent variables

dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_orig$Liked
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splittting the dataset

split = sample.split(dataset$Liked, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Training your model now

clf = randomForest(x = training_set[-767],
            y = training_set$Liked,
            ntree = 1000)

# Predicting the model

ypred = predict(clf, newdata = test_set[-767])

# Making confusion matrix

cm = table(test_set[,767], ypred)

# Plot confusion Matrix

library(ggplot2)
ggplot(data =  data.frame(cm), mapping = aes(x = ypred, y = Var1))+
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1)+
  scale_fill_gradient(low = "lightblue", high = "blue")+
  theme_bw() + theme(legend.position = "none")

# 80 % accuracy u know
