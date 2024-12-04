library(clipr)
library(stringr)

d<-read_clip()

left<-str_sub(d,1,5)
right<-str_sub(d,-5,-1)

leftN<-sort(as.numeric(left))
rightN<-sort(as.numeric(right))

diff<-abs(leftN-rightN)
print(sum(diff))

# P2

scores<-sapply(leftN, function(x) x*length(rightN[rightN==x]))
print(sum(scores))