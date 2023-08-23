# ------------------------
# Descriptive Analysis
# ------------------------
#Load Dataset
movies <- read.csv("movies.csv", header = TRUE)

#Structure of the data imported
str(movies)

#Summary statistics for each variable
#Load up psych library
library("psych")
describe(movies) 

#Number of missing value in each variable
#Load up naniar library
library("naniar")
miss_var_summary(movies)

#Box and Whisker plot for numeric variables 
boxplot(movies$imdb,
        ylab = "IMDb rating"
)

# ------------------------
# Problem Statement 1
# ------------------------
library(tidyverse)  #Date Formatting
library(lubridate)  #Date Formatting
library(dplyr)      #Count and Group By
library(ggplot2)    #Plotting
library(plotly)
library(htmlwidgets)#HTML

#Clean Dataset 1
temporary1 <- subset(movies,nchar(movies$release_date) ==4)                     # Already in Year only Format
temporary2 <- subset(movies,nchar(movies$release_date) ==10)                    # Date in the format of "YYYY-MM-DD" converted to Year only
temporary2$release_date <- format(as.Date(temporary2$release_date),format="%Y") 
temporary3 <- subset(movies,nchar(movies$release_date) > 10)										# Date in the format of "MONTH DD, YYYY" converted to Year only
temporary3$release_date <- format(mdy(temporary3$release_date),"%Y")
moviesUpdated <- rbind(temporary1,temporary2,temporary3)
moviescount <- count(moviesUpdated,release_date)
names(moviescount) <- c("Release_Year","Number_of_Movies")

#Time Series Plot 1
tsplot <- ggplot(moviescount, aes(x=Release_Year, y=Number_of_Movies, group =1)) +
  geom_line(color="#69b3a2") +
  ylab("Number of Movies") +
  xlab("Year") +
  ggtitle("Time Series Plot of the Number of Movies over the years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
tsplot <- ggplotly(tsplot)
saveWidget(tsplot, file="index.html") #Uploaded to Github


#Clean Dataset 2
genrelist <- c("Adventure","Action","Animation","Biography","Crime","Drama","Documentary","Costume","Comedy","Fantasy","Family","Game-Show","History","Horror","Kungfu","Mystery","Music","Romance","Sport","Sci-Fi","TV Show","Thriller","Western","War")

#Assign Records GenreCount = 0 or 1 based on whether the Genre contains the words in Genrelist
genreAssign <- matrix(ncol = length(genrelist), nrow = nrow(moviesUpdated)) #Data Frame to store count
for (j in 1:nrow(moviesUpdated)) {
  for (i in 1:length(genrelist)) {
    if (grepl(genrelist[i],moviesUpdated$genre[j])==TRUE){
      genreAssign[j,i] = 1;
    }
    else{
      genreAssign[j,i] = 0;
    }
  }
}
genreAssign <- data.frame(genreAssign)
colnames(genreAssign) <- genrelist
moviesUpdated2 <- cbind(moviesUpdated,genreAssign)[,c(7,10:32)]

#Group records together based on release_year
tempStore=c()
for (i in genrelist){
  temp <- aggregate(moviesUpdated2[[i]], by=list(Release_Year=moviesUpdated2$release_date), FUN=sum)
  colnames(temp)[names(temp) == 'x'] <- i
  if (is.null(tempStore)){
    tempStore <- temp
  } else {
    tempStore <- merge(tempStore,temp,by.x = "Release_Year",by.y="Release_Year")
  }
}
moviesUpdated2 <- tempStore %>%
  select(colnames(tempStore[1:length(tempStore)])) %>%
  gather(key = "Genres", value = "Count", -Release_Year)

#Time Series Plot 2
tsplot2 <- ggplot(moviesUpdated2, aes(x = Release_Year, y = Count, group=Genres)) + 
  geom_line(aes(color = Genres)) +
  ylab("Number of Movies") +
  xlab("Year") +
  ggtitle("Time Series Plot of the Number of Movies over the years by Genres") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
tsplot2 <- ggplotly(tsplot2)
saveWidget(tsplot2, file="index.html") #Uploaded to Github



# ------------------------
# Problem Statement 2
# ------------------------
library(igraph)
library(network) 
library(intergraph) 

#Get only the cast variable
moviecasts<-subset(movies, select = c(title,imdb,cast,release_date))
# Properly formats all the dates in release_date
for(i in 1:length(moviecasts$release_date)){
  if(nchar(moviecasts$release_date[i])>10){
    moviecasts$release_date[i]<-as.character(as.Date(moviecasts$release_date[i],format = "%B %d, %Y"))
  }else if(nchar(moviecasts$release_date[i])>4){
    moviecasts$release_date[i]<-as.character(as.Date(moviecasts$release_date[i],format = "%Y-%m-%d"))
  }else{
    moviecasts$release_date[i]<-as.character(as.Date(moviecasts$release_date[i],format = "%Y"))
  }
}

# ===================================================================
#Filtering
movies_2021 <- subset(moviecasts, as.Date(release_date)> "2020-12-31")
movies_2021 <- subset(movies_2021, imdb> 7)
movies_2021 <- subset(movies_2021, cast!=" N/A ")

#reset index
rownames(movies_2021) <- NULL
# ===================================================================

#What this does is that it splits up the casts together with its respective movie index
CastandIndex<- matrix(, nrow = 0, ncol=2, byrow=TRUE)
colnames(CastandIndex)<-c("cast","index")
for(k in 1:nrow(movies_2021)){
  cast<-movies_2021$cast[k]
  cast<-substr(cast,2,nchar(cast)-1)
  #split the cast names
  splitted<-strsplit(cast,", ")
  for(g in 1:length(splitted[[1]])){
    CastandIndex<-rbind(CastandIndex,c(splitted[[1]][g],k))
  }
}
# Get the index of all movies with repeated actor presence
# full naming is Movie Index With Repeating Casts
# It needs 2 duplicated() because 1 would always treat the first/last as original
MovieIndexWRC<-as.numeric(unique(c(CastandIndex[duplicated(CastandIndex[,1], fromLast=TRUE),2],
                                   CastandIndex[duplicated(CastandIndex[,1]),2])))
MovieIndexWRC<-sort(MovieIndexWRC)
rm(CastandIndex)
finalmovielist<-movies_2021[MovieIndexWRC,]

#Go through each movie at a time as 'cast'
castedgelist<- matrix(, nrow = 0, ncol=2, byrow=TRUE)
for(k in MovieIndexWRC){
  cast<-movies_2021$cast[k]
  cast<-substr(cast,2,nchar(cast)-1)
  #split the cast names
  splitted<-strsplit(cast,", ")
  #If It has an imdb of 7 or more
  if(!is.na(splitted[[1]][2]) 
  ){
    n<-length(splitted[[1]])
    print(n)
    for(i in 1:(n-1)){
      for(j in (i+1):(n)){
        castedgelist <- rbind(castedgelist,c(splitted[[1]][i],splitted[[1]][j]))
        
      }
    }
  }
}
#Convert Dataframe into CSV file
write.table(castedgelist,file="2021castedgelist.txt")

# check the structure of the input data data frame
print(str(castedgelist))
# i.e. eliminate any nodes that are self-referring
castedgelist <- subset(castedgelist, subset = (castedgelist[,1] != castedgelist[,2]))

# ==========================================================
cat("\n\nNumber of Valid Links: ", nrow(castedgelist))
# create network object from the links
cast_net <- network(as.matrix(castedgelist),
                    matrix.type = "edgelist", directed = FALSE, multiple = TRUE)
# create graph object with intergraph function asIgraph()
cast_graph <- asIgraph(cast_net)
# name the nodes noting that the first identifer on input was "0"
node_index <- as.numeric(V(cast_graph))
node_name<-network.vertex.names(cast_net)
# node name lookup table
node_reference_table <- data.frame(node_index, node_name, stringsAsFactors =
                                     FALSE)
print(str(node_reference_table))
print(head(node_reference_table))
set.seed(9999) # for reproducible results
pdf(file = "fig_cast_network.pdf", width = 8.5, height = 11)
plot(cast_graph, vertex.size = 8, vertex.color = "yellow",
     vertex.label.cex = 0.7, edge.arrow.size = 0.25,
     edge.color = "black", layout = layout.fruchterman.reingold)
dev.off()



#--------------------
#Problem statement 3 
#--------------------
library(dplyr)
library(tm)
library(wordcloud)

#--------------------
#word cloud 2021 
#--------------------
#--------------------
#description
#--------------------
movies_2021 <- filter(movies, grepl('2021', release_date))

moviesdes2021.corpus = Corpus(VectorSource(movies_2021$description))

moviesdes2021.corpus = moviesdes2021.corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART")) 

dtm <- TermDocumentMatrix(moviesdes2021.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#--------------------
#genre
#--------------------
moviesgenre2021.corpus = Corpus(VectorSource(movies_2021$genre))

moviesgenre2021.corpus = moviesgenre2021.corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower))

dtm <- TermDocumentMatrix(moviesgenre2021.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#--------------------
#word cloud 1971 
#--------------------
#--------------------
#description
#--------------------
movies_1971 <- filter(movies, grepl('1971', release_date))

moviesdes1971.corpus = Corpus(VectorSource(movies_1971$description))

moviesdes1971.corpus = moviesdes1971.corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART")) 

dtm <- TermDocumentMatrix(moviesdes1971.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#--------------------
#genre
#--------------------
moviesgenre1971.corpus = Corpus(VectorSource(movies_1971$genre))

moviesgenre1971.corpus = moviesgenre1971.corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower))

dtm <- TermDocumentMatrix(moviesgenre1971.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
