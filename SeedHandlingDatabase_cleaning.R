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
setwd("~/Box Sync/Rogers Lab/Rogers Lab Meeting")

alldata<-read_excel("Seed Handling database_foranalysis.xlsx", sheet=1)

test<-separate(data = alldata, col = frugivore, into = c("a","b"), sep = "\\,", extra="merge", fill="right" )

test2<-separate(data = test, col = plant, into = LETTERS[seq( from = 1, to = 6 )], sep = "\\,", extra="merge", fill="right")

test3<-gather(test2, "frugivore", "n", 7:8 )
test4<-gather(test3, "plant", "letter", 7:12)

with(test4, )
