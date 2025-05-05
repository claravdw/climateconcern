rm(list=ls())
library(tidyverse)
library(magrittr)

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}
if(onUser("pberg")){
  setwd("~/Documents/GitHub/climateconcern")
  figfolder<-"~/Documents/GitHub/climateconcern/figures/"
}
## Clara set up your folders here
if(onUser("clara")){
  setwd("~/Documents/climateconcern")
  figfolder<-"~/Documents/climateconcern/figures/"
}

#load cleaned model outputs
modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
timecollapse<-"2yr"
qrestrict<-"concernhuman"
datafilter<-paste(timecollapse,qrestrict,sep="_")


if(exists("datafilter")){
  load(paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
}else{
  load(paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
}

# table of survey questions included in model #### 
qs<-read_csv("qfolder/questions_filtered.csv")%>%
  arrange(source)%>%
  filter(use==1)%>%
  mutate(newquestion=gsub("concern","worry",newquestion),
         newquestion=gsub("worry_worry","worry",newquestion),
         newquestion=gsub("human_caused","attribution",newquestion),
         newquestion=gsub("human_cause","attribution",newquestion),
         newquestion=gsub("human","attribution",newquestion),
         newquestion=gsub("happening_aware_","aware_",newquestion),
         newquestion=gsub("happening_aware","aware",newquestion))

## sub to questions included in model 
qs<-qs%>%
  select(source,newquestion,question_summary)%>%
  distinct()
questions<-d%>%select(question)%>%distinct()

questions<-left_join(questions,qs,by=c("question"="newquestion"))
questions<-questions%>%
  mutate(year=substr(source,nchar(source)-3,nchar(source)))
unique(questions$year)
nrow(questions[is.na(questions$year),]) 

## assign a code to each survey--will need to make a bib file using these as keys (don't repeatedly create this file)
# surveys<-unique(questions$source)
# codes<-paste(combn(letters[1:26],2)[1,],combn(letters[1:26],2)[2,],sep="")
# codes<-c(letters[1:26],codes[1:(length(surveys)-26)])
# surveys<-data.frame(source=surveys,code=codes)
# write.csv(surveys,file=paste0(dropbox,"sourcecodes.csv"),row.names = FALSE)
# write.csv(surveys,file="qfolder/sourcecodes.csv",row.names=FALSE)

surveys<-read_csv("supptable_questions/sourcecodes.csv")

questions<-left_join(questions,surveys,by="source")

## find sources without codes and add manually: 
surveys2<-questions%>%filter(is.na(code))%>%select(source)%>%distinct()
surveys2$code<-c("fa","fb","fc","fd","fe","ff","fg","fh","fi","fj","fk","fl","fm","fn","fo","fp","fq",
                 "fr","fs","ft","fu","fv","fw","fx","fy","fz","ge","ga","gb","gc","gd") ## change these every time I re-do this. 
write_csv(surveys2,file=paste0("supptable_questions/new_surveys_with_codes_",Sys.Date(),".csv")) ## add bibliographic info for these to survey_refs.bib
write_csv(surveys,file=paste0("supptable_questions/old_surveys_with_codes_",Sys.Date(),".csv"))  ## check that the keys are correct on these in survey_refs.bib. 
questions2<-questions%>%
  filter(is.na(code))%>%
  select(-code)%>%
  left_join(surveys2,by="source")
questions<-questions%>%filter(!is.na(code))
questions<-rbind(questions,questions2)
## hard-code removal of rows where data are merged in as separate csv files when they're actually part of another survey included in the spreadsheet
questions<-questions%>%
  filter(source!="ziegler-china_2017",
         source!="ziegler-germany_2017",
         source!="lapopANTIGUA_2016",
         source!="ccfrindia_2006",
         source!="ccfrchina_2006")

## create table of question names, categories, years, and sources. 
newquestions<-unique(questions$question)
newquestions.surveys<-list()
newquestions.years<-list()
newquestions.summary<-list()

qs<-list()
for(i in 1:length(newquestions)){
  newquestions.surveys[[i]]=unique(questions$code[questions$question==newquestions[i]])
  newquestions.surveys[[i]]=paste("citealt{",paste(newquestions.surveys[[i]],collapse=", "),sep="")
  newquestions.surveys[[i]]=paste(newquestions.surveys[[i]],"}",sep="")
  newquestions.surveys[[i]]=paste("\\",newquestions.surveys[[i]],sep="")
  newquestions.years[[i]]=unique(questions$year[questions$question==newquestions[i]])
  newquestions.years[[i]]=paste(as.character(newquestions.years[[i]]),collapse="; ")
  newquestions.summary[[i]]=unique(questions$question_summary[questions$question==newquestions[i]])
  qs[[i]]=data.frame(question=newquestions[i],years=newquestions.years[[i]],summary=newquestions.summary[[i]],sources=newquestions.surveys[[i]])
  print(i)
}
qs<-do.call(rbind,qs)
qs<-qs%>%
  separate(question,into="category",sep="_",remove=FALSE,extra="drop")
qs<-qs%>%mutate(category=gsub(",",";",category),
                summary=gsub(",",";",summary),
                summary=gsub("%","\\\\%",summary))%>%
  mutate(category=ifelse(category=="human","attribution",category))%>%
  mutate_if(is.character,as.factor)
qs<-qs%>%
  select(category,years,summary,sources)
write_csv(qs,file="qfolder/questions.csv",col_names=FALSE)## have to manually remove quotation marks from the "sources" variable here. 
