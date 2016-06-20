library(rjson) 
library(RCurl) 
library(tm) 
library(SnowballC) 

###Set working dir path 
setwd("C:/meta")
j_file1 <-"./talks_art.json"
j_file2 <-"./talks_free.json"
j_file3 <-"./talks_music.json"
j_file4 <-"./talks_time.json"


###Read a json file to workspace 
talk_creativ1<-fromJSON(file=j_file1, method="C")
talk_creativ2<-fromJSON(file=j_file2, method="C") 
talk_creativ3<-fromJSON(file=j_file3, method="C") 
talk_creativ4<-fromJSON(file=j_file4, method="C") 
str(talk_creativ1) 




talk_names<-names(talk_creativ1$talks[[1]]$talk)
talk_names 


###Convertobject type 
talk_creativ_list1<-lapply(talk_creativ1$talks, function(x){unlist(x)})
talk_creativ_list2<-lapply(talk_creativ2$talks, function(x){unlist(x)}) 
talk_creativ_list3<-lapply(talk_creativ3$talks, function(x){unlist(x)}) 
talk_creativ_list4<-lapply(talk_creativ4$talks, function(x){unlist(x)}) 
str(talk_creativ_list1) 
parse_talk_all<-data.frame() 


####Data merge
df_parse_talk_all<-do.call("rbind", c(parse_talk_all, talk_creativ_list1, talk_creativ_list2, talk_creativ_list3, talk_creativ_list4))
str(df_parse_talk_all)


df_parse_talk_all<-data.frame(df_parse_talk_all) 
str(df_parse_talk_all) 


###Chage datatype of talk description 
df_parse_talk_all$talk.description<-as.character(df_parse_talk_all$talk.description) 
str(df_parse_talk_all$talk.description) 


###Change names of variables 
names(df_parse_talk_all)<-talk_names 
str(df_parse_talk_all)


####Remove duplicate values
df_parse_talk_all<-unique(df_parse_talk_all,incomparables = FALSE)
str(df_parse_talk_all)




#####Term Clustering 


###Convert object type 
class(df_parse_talk_all$description) 
ted_docs <- Corpus(VectorSource(df_parse_talk_all$description)) 
class(ted_docs) 


###Pre-processing 
ted_docs <- tm_map(ted_docs, tolower) 
ted_docs <- tm_map(ted_docs, removeNumbers) 
ted_docs <- tm_map(ted_docs, removePunctuation) 
ted_docs <- tm_map(ted_docs, removeWords, stopwords("SMART")) 
#ted_docs <- tm_map(ted_docs, removeWords, "ted") 


###Tokenizing 
strsplit_space_tokenizer <- function(x) 
  unlist(strsplit(as.character(x), "[[:space:]]+")) 
#token_docs<-(sapply(ted_docs, strsplit_space_tokenizer)) 
token_docs<-(sapply(ted_docs$content, strsplit_space_tokenizer)) 
token_freq<-table(unlist(token_docs)) 
summary(data.frame(token_freq)$Freq) 


###Stemming 
stem_docs <- sapply(token_docs, stemDocument) 
stem_freq<-table(unlist(stem_docs)) 
summary(data.frame(stem_freq)$Freq) 


df_stem_freq<-data.frame(stem_freq) 
str(df_stem_freq) 




###Term-Doc Matrix with Stemming 
class(stem_docs) 
stem_docs <- Corpus(VectorSource(stem_docs)) 
class(stem_docs) 


###term weight: TfIDF 
ted_tdm <- TermDocumentMatrix(stem_docs, 
                              control = list(removePunctuation = TRUE, 
                                             weighting=weightTfIdf, 
                                             stopwords = TRUE)) 


inspect(ted_tdm[1, ]) 




#####Hierachical Clustering: Term Clustering 
###Remove sparse terms 
ted_tdm_sparse <- removeSparseTerms(ted_tdm, sparse = 0.95) 
ted_tdm_sparse$nrow 
ted_tdm<-ted_tdm_sparse 
#sparse 0.98=265
#sparse 0.97=111
#sparse 0.96=57
#sparse 0.95=57
#sparse 0.94=28
#sparse 0.93=17
#sparse 0.92=13
#sparse 0.91=13
#sparse 0.90=11

###Convert to matrix 
ted_m <- as.matrix(ted_tdm) 


###Calculate similarity 
###dist {stats} Distance Matrix Computation 
###scale {base} Scaling and Centering of Matrix-like Objects 
distMatrix<- dist(scale(ted_m)) 


###Execute hierarchial clustering 
###hclust {stats} Hierarchical Clustering 
###method=c("single", complete", "average", "mcquitty", "median, "centroid", "ward.D", "ward.D2) 
fit <- hclust(distMatrix, method="average") 


###Draw dendrogram 
plot(fit) 
###rect.hclust {stats} Draw Rectangles Around Hierarchical Clusters 
###k = number of clusters 
rect.hclust(fit, k=14) 


###Save the dendrogram as PNG image file 
png("./dendrogram_sparse95_average.png", width = 1200, height=600) 
plot(fit) 
###k= number of clusters 
rect.hclust(fit, k=14) 
dev.off() 


###Assign a cluster to a term 
###cutree {stats} Cut a Tree into Groups of Data 
###k= number of clusters 
groups <- cutree(fit, k=14) 
df_groups <- data.frame(groups) 
str(df_groups) 
df_groups$KWD <- rownames(df_groups) 
str(df_groups) 


###Write the clustering result to text file 
write.table(df_groups, "./tc_result095_average.txt", row.names=FALSE, col.names=TRUE, sep="\t") 


####Write the description to text file
write.table(df_parse_talk_all$description, "description.txt")
