###################################################
# Amy Fan 
# Last updated: 2-2022
# This file creates the interactive chord diagram
# that's in the "DoubleMajors" repository 
####################################################

# import necessary libraries 
library(chorddiag)

# read in csv file 
a<- read.csv("data.csv", header= T, na.strings=c("","NA"), colClasses= "vector")

#################
# DATA CLEANING #
#################

# delete the one row with title headings IN THE MIDDLE OF THE DATA
a<- a[-1431,] 

# delete all the BS and BA data
for (i in 1:nrow(a)){
  if (!is.na(a$X1st.Major[i])){
    if (substr(a$X1st.Major[i],  nchar(as.character(a$X1st.Major[i]))-3, nchar(as.character(a$X1st.Major[i])))== "(AB)" 
       | substr(a$X1st.Major[i],  nchar(as.character(a$X1st.Major[i]))-3, nchar(as.character(a$X1st.Major[i])))== "(BS)") {
      a$X1st.Major[i] <- substring(a$X1st.Major[i], 1, nchar(as.character(a$X1st.Major[i]))-5)
    }
    
    # change misspellings
    if(a$X1st.Major[i] %in% c("International Comparative Science", "International Comparative Stuides") ) {a$X1st.Major[i]<- "International Comparative Studies"}
  }  
  if (!is.na(a$X2nd.Major[i])) {
    if(substr(a$X2nd.Major[i],  nchar(as.character(a$X2nd.Major[i]))-3, nchar(as.character(a$X2nd.Major[i])))== "(AB)" 
      | substr(a$X2nd.Major[i],  nchar(as.character(a$X2nd.Major[i]))-3, nchar(as.character(a$X2nd.Major[i])))== "(BS)") {
    a$X2nd.Major[i] <- substring(a$X2nd.Major[i], 1, nchar(as.character(a$X2nd.Major[i]))-5)
    }
    
    if(a$X2nd.Major[i] %in% c("International Comparative Science", "International Comparative Stuides") ) {a$X2nd.Major[i]<- "International Comparative Studies"}
    if(a$X2nd.Major[i] %in% c("Asian & Middle East Studies") ) {a$X2nd.Major[i]<- "Asian & Middle Eastern Studies"}
    if(a$X2nd.Major[i] %in% c("Biology (BSE)") ) {a$X2nd.Major[i]<- "Biology"}
    if(a$X2nd.Major[i] %in% c("Environmental Science")) {a$X2nd.Major[i]<- "Environmental Sciences"}
    if(a$X2nd.Major[i]=="Interdepartmental (Biology/Computer Science)") {a$X1st.Major[i]<-NA}
    
  }  
}

a$X2nd.Major[a$X2nd.Major %in% c("Computer Science (and Economics as a third major)", "Computer Science (and Economics triple major)", "Computer Science (Mathematics as a third major)")] <- "Computer Science"

###############################
# CREATE FLAGS FOR EACH MAJOR #
###############################

majors<- sort(unique(c(a$X1st.Major, a$X2nd.Major)))

flagsm<- matrix(0, nrow= nrow(a), ncol= length(majors))

for(i in 1:length(majors)){
  flagsm[,i]<- as.numeric(a$X1st.Major %in% majors[i]  | a$X2nd.Major %in% majors[i])
}

flagsm<- data.frame(flagsm)
colnames(flagsm)<- majors

# broader groups - read in csv
mgroups<- read.csv("majors.csv", header=T)

# what groups each individual is in
pgroups<- data.frame(matrix(0, ncol = 4, nrow= nrow(a)))
colnames(pgroups)<- colnames(mgroups)[2:5]

# loop through each kind of major group 
for(i in 2:5) {
	
  # loop through each major in that major group	
  for(major in mgroups[mgroups[,i]==1,1]){
  	
  	# loop through each row (future Amy hates this so much)
    for(n in 1:nrow(flagsm)){
      
      if(flagsm[n,gsub('[^0-9A-z\\s]','',colnames(flagsm))==gsub('[^0-9A-z\\s]','', major)]==1){
        pgroups[n,i-1]<- pgroups[n, i-1]+1
      }
    }
  }
}

comb<- cbind(a, flagsm, rowSums(flagsm), pgroups)

##################################
# CREATE MATRIX OF DOUBLE MAJORS #
##################################

dm<- comb[comb$`rowSums(flagsm)`==2 &comb$Engineering!=1,c(4,5)]

doubles<- matrix(0, nrow = ncol(flagsm), ncol=ncol(flagsm), dimnames = list(colnames(flagsm), colnames(flagsm))) 

for(i in 1:nrow(flagsm)){
  if(sum(flagsm[i,])==1){
     doubles[which(flagsm[i,]==1),which(flagsm[i,]==1)]<- doubles[which(flagsm[i,]==1),which(flagsm[i,]==1)]+1
  } else if(sum(flagsm[i,])==2){
    doubles[which(flagsm[i,]==1)[1], which(flagsm[i,]==1)[2]]<- doubles[which(flagsm[i,]==1)[1], which(flagsm[i,]==1)[2]]+1
    doubles[which(flagsm[i,]==1)[2], which(flagsm[i,]==1)[1]]<- doubles[which(flagsm[i,]==1)[2],which(flagsm[i,]==1)[1]]+1
  }
}

##########################
# make the chord diagram #
##########################

chorddiag(doubles)

#####################
# NOTES ON THE DATA #
#####################


#what's up with the people who only have a second major
#nder the public or private category what do the GRADUATED NEVER ATTENDED DUKE category mean
#Neuroscience and psychology are in both ss and ns. I counted N in NS and Psych in SS
#Philosophy was in Human and SS--counted it in Humanities
#"Other" includes Program II and IDM


#####cum laudes for pratt not available, and myabe not phi beta kappa either
#black graduating group only for ppl graduating in spring of 2018... i think
#for ppl double majoing, they choose which bachelors they get, i think, so it's not a perfect metric of what ppl actually studied
#if you didn't graduate in 2017-18 we might not have your major, and if you graduated earlier than 2017 we won't have your race
#didn't include transfers into the class cause noway to keep track of them all
#idk if all the programs list interdepartmental majors, also some programs are missing some graduating according to someone from the linguistics (?) graduation ceremony
#for double major count don't include program ii's
#can also add a colum for athletes indicating whether they dropped their sport
#seems like there's disproportionately few athletes whose pictures and additional bio details are included in the freshman directory
#don't have greek life orgs for multicultural groups and missing Jam SLG
#a few ppl posted a picture in the freshman directory but not their interests/potential major
#challenge with looking at who did and didn't graduate is some ppl may have transferred

