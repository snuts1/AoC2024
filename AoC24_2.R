library(clipr)
df<-read_clip_tbl(header=FALSE)

# separate by space
df2<-separate(data=df, col="V1", into = c('a','b','c','d','e','f','g','h'), sep=" ")

# shifted to the right (removing column 'a')
df3<-df2[,2:8]
df3$new <-"ph" #placeholder to retain df size after shifting. will convert to NA with next step

# make numbers
df2N<-sapply(df2,function(x) as.numeric(x))
df3N<-sapply(df3,function(x) as.numeric(x))
deltas<-df2N-df3N

# safe descending is ((delta <= -1) AND (delta >= -3) OR is.na(delta))
testD<-is.na(deltas) | ((deltas<=-1) & (deltas>=-3))
sumsD<-rowSums(testD)
resultD<-sumsD==8 #returns TRUE/FALSE is the row safe
countD<-sum(sumsD==8) # if everything in a row meets the criteria then there will be 8 TRUEs

# safe ascending is ((delta <= 3) AND (delta >= 1) OR is.na(delta))
testA<-is.na(deltas) | ((deltas<=3) & (deltas>=1))
sumsA<-rowSums(testA)
resultA<-sumsA==8 #returns TRUE/FALSE is the row safe
countA<-sum(sumsA==8)

resultOriginal<-resultD+resultA
print(countD+countA)

# P2 - done in such a way as to be obnoxious

mv1<-subset(df2,select=-a) #multiverse 1, where there is no a
mv2<-subset(df2,select=-b) #multiverse 2, where there is no b
mv3<-subset(df2,select=-c) #multiverse 3, where there is no c
mv4<-subset(df2,select=-d) #multiverse 4, where there is no d
mv5<-subset(df2,select=-e) #multiverse 5, where there is no e
mv6<-subset(df2,select=-f) #multiverse 6, where there is no f
mv7<-subset(df2,select=-g) #multiverse 7, where there is no g
mv8<-subset(df2,select=-h) #multiverse 8, where there is no h

# function to re-run P1 checks but can feed it each multiverse (7col vs 8)
doer<- function(df){
  shifted<-df[,2:7]
  shifted$new<-"ph"
  num1<-sapply(df,function(x) as.numeric(x))
  num2<-sapply(shifted,function(x) as.numeric(x))
  deltas2<-num1-num2
  
  # safe descending is ((delta <= -1) AND (delta >= -3) OR is.na(delta))
  testD2<-is.na(deltas2) | ((deltas2<=-1) & (deltas2>=-3))
  sumsD2<-rowSums(testD2)
  resultD2<-sumsD2==7 #returns TRUE/FALSE is the row safe
  
  # safe ascending is ((delta <= 3) AND (delta >= 1) OR is.na(delta))
  testA2<-is.na(deltas2) | ((deltas2<=3) & (deltas2>=1))
  sumsA2<-rowSums(testA2)
  resultA2<-sumsA2==7 #returns TRUE/FALSE is the row safe
  return(resultA2+resultD2)
}

#sum all the results to find rows where ANY multiverse is TRUE. multiple TRUEs will add up but it only matters that the sum is >0
bigresult<-doer(mv1)+doer(mv2)+doer(mv3)+doer(mv4)+doer(mv5)+doer(mv6)+doer(mv7)+doer(mv8)+resultOriginal

#count up all the rows which have at least one TRUE scenario
print(sum(bigresult>0))