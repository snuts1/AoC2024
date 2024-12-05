library(stringr)
library(tidyr)

raw<-readLines("C:\\Users\\LabAdmin\\Documents\\data4.txt")
input<-separate(data=df,col="input",into=as.character(c(1:141)), sep="")
# need to convert to matrix for easier concatenation later

# aight so.. kinda rarted matrix method. functions to look up, down, left, right by shifting whole matrix
# then concatenate the shifts with original to make a list of all forward combos, all upward combos etc.
# check all combos for XMAS ??? profit

# worth noting that this gives ALL "words" of length=4 so its close to a general word puzzle solution
# after finishing this it occurs to me that i could look in only 4 directions and check for XMAS or SAMX... oh well

# shift left (look right): removes first column and adds new last column to retain shape
lookR<-function(df){
  temp<-subset(df,select=-1)
  newcolname<-as.character(as.numeric(tail(colnames(df),n=1))+1)
  temp$new<-0
  names(temp)[names(temp)=='new']<- newcolname
  return(temp)
}
#shift up (look down): removes first row and adds new last row to retain shape
lookD<-function(df) {
  temp<-df[-1,]
  temp[nrow(temp)+1,]<-0
  return(temp)
}
#shift right (look left): adds new first column, removes last column to retain shape
lookL<-function(df) {
  newcolname<-as.character(as.numeric(colnames(df)[1])-1)
  temp<- df[, 1:(ncol(df) - 1)]
  temp<-cbind(new=0,temp)
  names(temp)[names(temp)=='new']<- newcolname
  return(temp)
}
#shift down (look up): adds new first row, removes last row to retain shape
lookU<-function(df){
  temp<-rbind(0,df)
  temp<-temp[1:(nrow(temp)-1),]
  return(temp)
}

#check horizontal to the left
lookL1<-lookL(input)
lookL2<-lookL(lookL1)
lookL3<-lookL(lookL2)
lookLoutput<-paste(as.matrix(input),as.matrix(lookL1),as.matrix(lookL2),as.matrix(lookL3),sep="")
totalL<-sum(lookLoutput=="XMAS")

#check horizontal to the right
lookR1<-lookR(input)
lookR2<-lookR(lookR1)
lookR3<-lookR(lookR2)
lookRoutput<-paste(as.matrix(input),as.matrix(lookR1),as.matrix(lookR2),as.matrix(lookR3),sep="")
totalR<-sum(lookRoutput=="XMAS")

#check vertical down
lookD1<-lookD(input)
lookD2<-lookD(lookD1)
lookD3<-lookD(lookD2)
lookDoutput<-paste(as.matrix(input),as.matrix(lookD1),as.matrix(lookD2),as.matrix(lookD3),sep="")
totalD<-sum(lookDoutput=="XMAS")

#check vertical up
lookU1<-lookU(input)
lookU2<-lookU(lookU1)
lookU3<-lookU(lookU2)
lookUoutput<-paste(as.matrix(input),as.matrix(lookU1),as.matrix(lookU2),as.matrix(lookU3),sep="")
totalU<-sum(lookUoutput=="XMAS")

# there's probably a smart way to handle diagonals given that i already have the orthogonals..

#check diagonal up/right
lookUR1<-lookU(lookR1)
lookUR2<-lookU(lookR(lookUR1))
lookUR3<-lookU(lookR(lookUR2))
lookURoutput<-paste(as.matrix(input),as.matrix(lookUR1),as.matrix(lookUR2),as.matrix(lookUR3),sep="")
totalUR<-sum(lookURoutput=="XMAS")

#check diagonal up/left
lookUL1<-lookU(lookL1)
lookUL2<-lookU(lookL(lookUL1))
lookUL3<-lookU(lookL(lookUL2))
lookULoutput<-paste(as.matrix(input),as.matrix(lookUL1),as.matrix(lookUL2),as.matrix(lookUL3),sep="")
totalUL<-sum(lookULoutput=="XMAS")

#check diagonal down/right
lookDR1<-lookD(lookR1)
lookDR2<-lookD(lookR(lookDR1))
lookDR3<-lookD(lookR(lookDR2))
lookDRoutput<-paste(as.matrix(input),as.matrix(lookDR1),as.matrix(lookDR2),as.matrix(lookDR3),sep="")
totalDR<-sum(lookDRoutput=="XMAS")

#check diagonal down/left
lookDL1<-lookD(lookL1)
lookDL2<-lookD(lookL(lookDL1))
lookDL3<-lookD(lookL(lookDL2))
lookDLoutput<-paste(as.matrix(input),as.matrix(lookDL1),as.matrix(lookDL2),as.matrix(lookDL3),sep="")
totalDL<-sum(lookDLoutput=="XMAS")

bigtotal<-totalU+totalD+totalL+totalR+totalUL+totalUR+totalDL+totalDR

# P2

lookULDRoutput<-paste(as.matrix(lookUL1),as.matrix(input),as.matrix(lookDR1),sep="") #check upleft1 downright1 for SAM or MAS
test1<-(lookULDRoutput=="SAM") | (lookULDRoutput=="MAS")

lookDLURoutput<-paste(as.matrix(lookDL1),as.matrix(input),as.matrix(lookUR1),sep="") #check upright1 downleft1 for SAM or MAS
test2<-(lookDLURoutput=="SAM") | (lookDLURoutput=="MAS")

test3<-test1+test2
print(sum(test3==2))#count how many cases where both sides of the X are there