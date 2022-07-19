df<-read.csv(file = "Spotify_2010_-_2019_Top_100.csv",header = TRUE, sep=",")
df <- na.omit(df)

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

df.subset$artist.type.enc <- new.col1

genre_dict <- c(
  "dance pop","pop", 
  "pop soul","soul",
  "atl hip hop","hip hop",
  "pop rap","rap",
  "big room","house",
  "canadian hip hop","hip hop",
  "disco house","house",
  "romanian house","house",
  'lilith',"indie",
  'detroit hip hop',"hip hop", 
  'asian american hip hop',"hip hop", 
  'east coast hip hop', "hip hop",
  'neo mellow',"pop", 
  'canadian pop',"pop", 
  'reggae fusion',"reggae", 
  'idol',"pop", 
  'art pop',"pop",
  "talent show","pop", 
  'modern alternative rock',"rock",
  'indietronica',"electronic", 
  'grime',"hip hop", 
  'barbadian pop',"pop", 
  'acoustic pop',"pop",
  'dutch house',"house", 
  'belgian pop',"pop", 
  'contemporary country',"country", 
  'boy band',"pop",
  'celtic rock',"rock", 
  'edm',"electronic", 
  'indie rock',"indie", 
  'australian dance',"dance",
  'british soul',"soul", 
  'eau claire indie',"indie", 
  'dancefloor dnb',"dance",
  'permanent wave',"rock", 
  'hip pop',"pop", 
  'g funk',"funk", 
  'baroque pop',"pop", 
  'indie pop',"pop",
  'chicago rap',"rap", 
  'indie poptimism',"indie", 
  'french shoegaze',"rock",
  'alternative metal',"metal", 
  'indie folk',"indie", 
  'alternative rock',"rock",
  'uk hip hop',"hip hop", 
  'electro house',"house", 
  'garage rock',"rock", 
  'israeli pop',"pop",
  'alternative r&b',"r&b", 
  'australian pop',"pop", 
  'candy pop',"pop", 
  'modern rock',"rock",
  'conscious hip hop',"hip hop", 
  'folk-pop',"pop", 
  'alternative dance',"dance", 
  'k-pop',"pop",
  'gangster rap',"rap", 
  'brostep',"dance", 
  'downtempo',"pop", 
  'la indie',"indie", 
  'bass trap',"dance",
  'metropopolis',"pop", 
  'electropop',"pop", 
  'electro',"electronica", 
  'destroy techno',"dance", 
  'emo',"rock",
  'austrian pop',"pop", 
  'irish pop',"pop", 
  'adult standards',"pop", 
  'modern folk rock',"rock",
  'tropical house',"house", 
  'contemporary r&b',"r&b", 
  'deep disco house',"house",
  'bubblegum dance',"dance", 
  'chill pop',"pop", 
  'comic',"pop", 
  'complextro',"electronica", 
  'nyc rap',"rap",
  'deep groove house',"house", 
  'australian hip hop',"hip hop", 
  'neo soul',"soul",
  'deep house',"house", 
  'french indie pop',"pop", 
  'german pop',"pop", 
  'dutch hip hop',"hip hop",
  'aussietronica',"electronica", 
  'australian indie',"indie", 
  'canadian contemporary r&b',"r&b",
  'kentucky hip hop',"hip hop", 
  'new jersey rap',"rap", 
  'irish singer-songwriter',"pop",
  'ghanaian hip hop',"hip hop", 
  'icelandic indie',"indie", 
  'indie pop rap',"pop",
  'new french touch',"pop", 
  'san diego rap',"rap", 
  'australian psych',"rock",
  'canadian indie',"indie", 
  'alt z',"pop", 
  'danish pop',"pop", 
  'melodic rap',"rap",
  'social media pop',"pop", 
  'london rap',"rap", 
  'florida rap',"rap", 
  'emo rap',"rap",
  'latin',"dance", 
  'ohio hip hop',"hip hop", 
  'dfw rap',"rap", 
  'hawaiian hip hop',"hio hop",
  'dirty south rap',"rap", 
  'afroswing',"swing", 
  'basshall',"dance", 
  'memphis hip hop',"hip hop",
  'bedroom pop',"pop", 
  'hollywood',"pop", 
  'afrofuturism',"pop", 
  'comedy rap',"rap",
  'colombian pop',"pop", 
  'cali rap',"rap", 
  'black americana',"pop",
  'north carolina hip hop',"hip hop", 
  'alternative pop rock',"pop", 
  'dark clubbing',"dance",
  'lgbtq+ hip hop',"hip hop", 
  'afro dancehall',"dance", 
  'argentine hip hop',"hip hop",
  'classic rock',"rock", 
  'uk drill',"hip hop")
val = 0
while (val <= 258)
{
  tempval <- val
  df["top.genre"][df["top.genre"] == genre_dict[tempval+1]] <-  as.character(genre_dict[tempval+2])
  val = val + 2
}
new.col2<-encode_ordinal(df[["top.genre"]])

#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

df.subset <- as.data.frame(lapply(df[,6:15], normalize))
df.subset <- df[,6:15]
df.subset$artist.type.enc <- new.col1
df.subset$top.genre.enc <- new.col2
df.subset <- as.data.frame(lapply(df.subset, normalize))
colnames(df.subset)
df.subset
#splicing
set.seed(123)
dat.d <- sample(1:nrow(df.subset),size=nrow(df.subset)*0.7,replace = FALSE) #random selection of 70% data.

train <- df.subset[dat.d,] # 70% training data
test <- df.subset[-dat.d,] # remaining 30% test data

#Creating seperate dataframe for 'Creditability' feature which is our target.
train_labels <- df.subset[dat.d,12]
test_labels <-df.subset[-dat.d,12]

#Install class package
install.packages('class')
# Load class package
library(class)
NROW(train_labels) 

#The square root of 700 is around 26.45, therefore we’ll create two models. One with ‘K’ value as 26 and the other model with a ‘K’ value as 27.
knn.26 <- knn(train=train, test=test, cl=train_labels, k=26)
knn.27 <- knn(train=train, test=test, cl=train_labels, k=27)
knn.9 <- knn(train=train, test=test, cl=train_labels, k=9)

#Calculate the proportion of correct classification for k = 26, 27
ACC.26 <- 100 * sum(test_labels == knn.26)/NROW(test_labels)
ACC.27 <- 100 * sum(test_labels == knn.27)/NROW(test_labels)
ACC.9 <- 100 * sum(test_labels == knn.9)/NROW(test_labels)
ACC.26
ACC.27
ACC.9

#You can also use the confusion matrix to calculate the accuracy. 
install.packages('caret')
install.packages('lattice')
install.packages('ggplot2')
install.packages('InformationValue')
install.packages('ISLR')
library(caret)
library(lattice)
library(ggplot2)
library(InformationValue)
library(ISLR)
confusionMatrix(table(knn.26 ,test_labels))

i=1
k.optm=1
for (i in 1:28){
  knn.mod <- knn(train=train, test=test, cl=train_labels, k=i)
  k.optm[i] <- 100 * sum(test_labels == knn.mod)/NROW(test_labels)
  k=i
  cat(k,'=',k.optm[i],'')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")




install.packages('tidyverse')


library(tidyverse)
library(ggplot2)
df <- df[ which( df$top.year >= 2010 & df$top.year <= 2014),]
df$ï..title
df <- df %>% 
  group_by(top.genre) %>% 
  tally(sort = TRUE) %>% 
  filter(row_number() <= 5)
df[[1]][1]
ggplot(data = df, aes(x = top.genre, y = n)) +
  geom_col() +
  facet_grid(.~top.genre, scales = "free_x")
