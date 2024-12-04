library(clipr)
library(stringr)
raw<-readChar("C:\\Users\\LabAdmin\\Documents\\data3.txt", file.info("C:\\Users\\LabAdmin\\Documents\\data3.txt")$size)
input<-str_remove_all(raw,'\\n')
#input<-read_clip()
valids<-str_extract_all(input,'mul[(]\\d{1,3},\\d{1,3}[)]') # match appropriate syntax = mul(xxx,xxx) with 1-3 numbers each
valids<-unlist(valids)
#tester<-"mul(3,4)"             
parse<- function(x) {
  end1<-str_locate(x,'[,]')[1]-1 # subtract 1 to end BEFORE the ,
  print(end1)
  num1<-as.numeric(substr(x,5,end1)) #first num always starts at pos5 due to proper syntax, first 4chars are mul(
  print(num1)
  end2<-str_locate(x,'[)]')[1]-1
  print(end2)
  num2<-as.numeric(substr(x,end1+2,end2)) # add 2 because.. 1 doesnt work
  print(num2)
  return(num1*num2)
}
#print(parse(tester))
output<-sum(sapply(valids,parse))

# P2
instructions<-str_extract_all(input,'don\'t\\(\\)(?s:.*?)do\\(\\)') # the ?s:.*? matches ANY number of ANYTHINGs
instructions<-unlist(instructions)
valids2<-str_extract_all(instructions,'mul[(]\\d{1,3},\\d{1,3}[)]')
valids3<-unlist(valids2)

output2<-sum(sapply(valids3,parse))

answer<-output-output2
