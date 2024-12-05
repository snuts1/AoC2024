library(stringr)
library(tidyr)
library(purrr)
library(dplyr)

raw<-readLines("C:\\Users\\LabAdmin\\Documents\\data5.txt")
marker<-which(raw=='') # there's a newline between the two lists

df1<-raw[1:(marker-1)] # from start to marker
df2<-raw[(marker+1):length(raw)] # from marker to end

df1s<-list_transpose(strsplit(df1,"\\|"))
df2s<-strsplit(df2,",")


TEMP<-which(df1s[[1]]==36) # vector of the indices of all 36| instructions
TEMP2<-df1s[[2]][TEMP] # vector of the 2nd values of all 36| instructions

# function? to check an instruction against a single line
checker<-function(top,bot){
  a<-substr(top,1,2)
  b<-substr(top,4,5)
  botSplit<-strsplit(as.character(bot),b)
  return(str_detect(botSplit[[1]][2],a))
}
checkerV<-Vectorize(checker) # idk really what this does but it seems to make my function work with vectors
grid<-expand.grid(df1,df2)  # makes a grid of all instruction/line combos
grid$result<-checkerV(grid$Var1,grid$Var2) # makes a third column of the checkerV output for each combo
gridF<-grid %>% filter(result==TRUE)  # finds all the TRUEs (violations)
goodones<-setdiff(as.matrix(df2),as.matrix(gridF["Var2"])) # pulls the violating lines out of genpop
mids<-sapply(goodones, function(x) substr(x,nchar(x)/2,1+(nchar(x)/2))) # finds the middle pagenumber for each good line
answer<-sum(as.numeric(mids))

# P2

# already have the list of violator lines (with dupes): gridF["Var2"]
# need to identify the relevant instructions for each line, then figure some way to organize and apply the instructions
