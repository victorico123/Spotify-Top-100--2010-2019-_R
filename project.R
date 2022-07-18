df<-read.csv(file = "Spotify_2010_-_2019_Top_100.csv",header = TRUE, sep=",")
View(df)

typeof(df)


colnames(df)

sapply(df,class)


encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

new.col<-encode_ordinal(df[["artist.type"]])
df$artist.type.enc<-new.col
df



gsub("\\?+","'",iconv(df$added, "latin1", "ASCII", sub=" "))
gsub("  ","-",df$added)
df$added<-as.POSIXct(df$added)
df$ï..title
nrow(df)
apply(df,2, function(x) is.na(x))
is.null(df)
df <- na.omit(df)
rename(df, title = ï..title)
library(dplyr)
df = select(df,Science_score,Mathematics_score,Name)

sapply(df, class)

temp<-which(is.na(df), arr.ind=TRUE)
temp[1,0]
unique(temp)

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
  print(tempval)
  df["top.genre"][df["top.genre"] == genre_dict[tempval+1]] <-  as.character(genre_dict[tempval+2])
  val = val + 2
}

length(unique(df$top.genre))
