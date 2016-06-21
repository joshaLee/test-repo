library(tm)
library(rjson)
library(RCurl)
library(SnowballC)

#I want See You
j_file1 <-"./talks_art.json"
j_file2 <-"./talks_free.json"
j_file3 <-"./talks_music.json"
j_file4 <-"./talks_time.json"

talk_creativ1<-fromJSON(file=j_file1, method="C")
talk_creativ2<-fromJSON(file=j_file2, method="C") 
talk_creativ3<-fromJSON(file=j_file3, method="C") 
talk_creativ4<-fromJSON(file=j_file4, method="C") 

talk_names<-names(talk_creativ1$talks[[1]]$talk)
talk_names 

###Convertobject type 
talk_creativ_list1<-lapply(talk_creativ1$talks, function(x){unlist(x)})
talk_creativ_list2<-lapply(talk_creativ2$talks, function(x){unlist(x)}) 
talk_creativ_list3<-lapply(talk_creativ3$talks, function(x){unlist(x)}) 
talk_creativ_list4<-lapply(talk_creativ4$talks, function(x){unlist(x)}) 

str(talk_creativ_list1) 
parse_talk_all<-data.frame() 
df_parse_talk_all1<-do.call("rbind", c(parse_talk_all, talk_creativ_list1))
df_parse_talk_all2<-do.call("rbind", c(parse_talk_all, talk_creativ_list2))
df_parse_talk_all3<-do.call("rbind", c(parse_talk_all, talk_creativ_list3))
df_parse_talk_all4<-do.call("rbind", c(parse_talk_all, talk_creativ_list4))
str(df_parse_talk_all1)

df_parse_talk_all1<-data.frame(df_parse_talk_all1) 
df_parse_talk_all2<-data.frame(df_parse_talk_all2)
df_parse_talk_all3<-data.frame(df_parse_talk_all3)
df_parse_talk_all4<-data.frame(df_parse_talk_all4)
str(df_parse_talk_all1)

names(df_parse_talk_all1)<-talk_names 
names(df_parse_talk_all2)<-talk_names 
names(df_parse_talk_all3)<-talk_names 
names(df_parse_talk_all4)<-talk_names 
str(df_parse_talk_all1) 

df_parse_talk_all1$talk.description<-as.character(df_parse_talk_all1$talk.description)
df_parse_talk_all2$talk.description<-as.character(df_parse_talk_all2$talk.description)
df_parse_talk_all3$talk.description<-as.character(df_parse_talk_all3$talk.description)
df_parse_talk_all4$talk.description<-as.character(df_parse_talk_all4$talk.description)
str(df_parse_talk_all1$talk.description)

write.table(df_parse_talk_all1$description, "description1.txt")
write.table(df_parse_talk_all2$description, "description2.txt")
write.table(df_parse_talk_all3$description, "description3.txt")
write.table(df_parse_talk_all4$description, "description4.txt")

