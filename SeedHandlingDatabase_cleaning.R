# abcdefg

library(tidyr)
library(readxl)

#play around with tidyr
df <- data.frame(x = c("a", "a b", "a b c", NA))
df %>% separate(x, c("a", "b"))
# The same behaviour but no warnings
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
# Another option:
df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")

####################
#now with dataset
#setwd("~/Box Sync/Rogers Lab/Rogers Lab Meeting")

alldata<-read_excel("Seed Handling database_foranalysis_ef_20160913.xlsx", sheet=1)

test<-separate(data = alldata, col = frugivore, into = c("a","b"), sep = "\\,", extra="merge", fill="right" )

test2<-separate(data = test, col = plant, into = LETTERS[seq( from = 1, to = 6 )], sep = "\\,", extra="merge", fill="right")

test3<-gather(test2, "frugivore", "n", 7:8 )
test4<-gather(test3, "plant", "letter", 7:12)



### 

alldata <- as.data.frame(alldata)

alldata$numid <- 1:length(alldata$paperid)

head(alldata)

out.plants <- matrix(rep(NA,length(alldata$numid)*100),
                     nrow=length(alldata$numid),
                     ncol=100)

out.frugivores <- matrix(rep(NA,length(alldata$numid)*100),
                    nrow=length(alldata$numid),
                    ncol=100)

for(i in 1:length(alldata$paperid)){
  set <- alldata[i,]
  
  plants <- unlist(strsplit(set$plant,split = ","))
  frugivores <- unlist(strsplit(set$frugivore,split = ","))
  
  out.plants[i,1:length(plants)] <- plants
  out.frugivores[i,1:length(frugivores)] <- frugivores
}

# Remove trailing and leading white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

out.plants <- trim(out.plants)
out.frugivores <- trim(out.frugivores)

# Now just get a data frame with numid and species

plants.inds <- which(!is.na(out.plants),arr.ind=T)
plants.df <- as.data.frame(matrix(ncol=0,nrow=length(plants.inds[,1])))
plants.df$numid <- plants.inds[,1]
plants.df$full.name <- out.plants[plants.inds]
head(plants.df)
tail(plants.df) # checks out!


frugivores.inds <- which(!is.na(out.frugivores),arr.ind=T)
frugivores.df <- as.data.frame(matrix(ncol=0,nrow=length(frugivores.inds[,1])))
frugivores.df$numid <- frugivores.inds[,1]
frugivores.df$full.name <- out.frugivores[frugivores.inds]
head(frugivores.df)
tail(frugivores.df) # checks out!




###
take.parenth <- function(x) gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
test.char0 <- function(x) identical(unlist(x),character(0))


# Plants
plants.df$paren <- lapply(plants.df$full.name,take.parenth)
plants.df$paren <- unlist(ifelse(sapply(FUN=test.char0,plants.df$paren), NA, plants.df$paren))
plants.df$nonparen <- gsub("\\s*\\([^\\)]+\\)","",plants.df$full.name)

plants.df$scientific <- trim(ifelse(is.na(plants.df$paren), plants.df$nonparen, plants.df$paren))
plants.df$common <- trim(ifelse(is.na(plants.df$paren), NA, plants.df$nonparen))

plants.df$genus <- gsub(' [A-z ]*', '' , plants.df$scientific)
plants.df$species <- sub(".*? (.+)", "\\1", plants.df$scientific)

plants.df <- plants.df[,c("numid","full.name","scientific",
                          "genus","species","common")]


head(plants.df)

# Frugivores

frugivores.df$paren <- lapply(frugivores.df$full.name,take.parenth)
frugivores.df$paren <- unlist(ifelse(sapply(FUN=test.char0,frugivores.df$paren), NA, frugivores.df$paren))
frugivores.df$nonparen <- gsub("\\s*\\([^\\)]+\\)","",frugivores.df$full.name)

frugivores.df$scientific <- trim(ifelse(is.na(frugivores.df$paren), frugivores.df$nonparen, frugivores.df$paren))
frugivores.df$common <- trim(ifelse(is.na(frugivores.df$paren), NA, frugivores.df$nonparen))

frugivores.df$genus <- gsub(' [A-z ]*', '' , frugivores.df$scientific)
frugivores.df$species <- sub(".*? (.+)", "\\1", frugivores.df$scientific)

frugivores.df <- frugivores.df[,c("numid","full.name","scientific",
                          "genus","species","common")]

head(frugivores.df)



###
###
###

plants.df <- plants.df[order(plants.df$numid),]
frugivores.df <- frugivores.df[order(frugivores.df$numid),]

#write.csv(plants.df,"plants.csv")
#write.csv(frugivores.df,"frugivores.csv")






# Get database with each row as a paper (and unique ID) with all this info. Can we get effect size?
# Make plant list sheet with genus, spp, common, id notes, checked by
# Make a plant-frugivore combos dataset with unique id, animal genus, animal sp, plant genus, plant species, ca
# 

# 



which(duplicated(alldata$paperid))
length(alldata$paperid)

