?grep
data(state)
state.name

grep(pattern = "i", x = state.name, ignore.case = T)
grep(pattern = "i", x = state.name, ignore.case = T, value = F)

grepl(pattern = "i", x = state.name, ignore.case = T)
sum(grepl(pattern = "i", x = state.name, ignore.case = T))

#Question 1
grepl(pattern = "i", x = state.name, ignore.case = T)
sum(!grepl(pattern = "i", x = state.name, ignore.case = T))

#match()
favorites <- c("peach", "banana", "bluberry", "orange", "plum", "strawberry",
	               "mandarin")

citrus <- c("kumquat", "grapefruit", "orange", "mandarin", "tangerine",
	            "tangelo", "lemon", "lime")

match(favorites, citrus)

favorites %in% citrus
favorites[favorites %in% citrus]

#sub()
sub("w", "_DoubleU_", state.name, ignore.case = T)

#Question 2
sub("is", "ys", state.name, ignore.case = T)
#it only makes changes in the first instance in the string

gsub("i", "y", state.name, ignore.case = T)

data(state)
grep("a.a", state.name, value = T, ignore.case = T)

#Question 3
grep("\\s", state.name, value = T, ignore.case = T)

#Question 4
grep("^w", state.name, value = T, ignore.case = T)

grep("[aeiou]$", state.name, value = T)
grep("[^aeiou]$", state.name, value = T)

#Question 5
grep("^[^aeiou]", state.name, value = T, ignore.case = T)

grep("[[:space:]]", state.name, value = T)
grep(" ", state.name, value = T)

#Question 6
grep("[aeiou]{2}", state.name, value = T)

#Question 7
setwd("~/Documents/Maloof Lab/R_club/plyr-tutorial/examples")
bnames <- read.csv("bnames.csv", stringsAsFactors = FALSE)
head(bnames)

stacey1 <- grep(pattern = "Stac(ey|y)$", x = bnames$name, value = T)
stacey1
length(stacey1)

stacey2 <- grep(pattern = "Stac(ey|y|i)$", x = bnames$name, value = T)
stacey2
length(stacey2)

#Question 8
John <- grep(pattern = "Jon(athan|nie|athon)", x = bnames$name, value = T)
John

ILs <- c("IL.1.1", "IL.2.2", "IL.1.3", "IL.2.1", "IL.11.1", "IL.11.3", "IL.12.1", 
         "IL.12.2")

grep("IL.1.", ILs, value = T) #this matches many more because the . is not escaped
grep("IL\\.1\\.", ILs, value = T)
#alternatively
grep("IL.1.", ILs, value = T, fixed = T)

#can refer back to previous 
grep("(.)\\1", state.name, value = T)
#putting the . in parenthesis makes it a capture group

#Excercise 9
grep("([aeiou])\\1", state.name, value = T)

#Excercise 10
sub("(.+)(\\s)(.+)", "\\3, \\1", state.name,  ignore.case = T)

#Excercise 11
sub("(^.)(.+)(\\s)(.+)", "\\1. \\4", state.name,  ignore.case = T)


########
#REGULAR EXPRESSIONS #2
#2013_05_07

#Question 1
filename.1 <- "a IL-9.2.4 crazy SH file rep1 name"
filename.2 <- "anotherIL-12.4sillySUfilerep20name"

filename.1.1 <- sub("(.+)(IL-\\d+\\.\\d+(\\.\\d+)?)(.+)(S[A-Z])(.+)(rep\\d+)(.+)",
                       "\\2,\\5,\\7", filename.1)
filename.1.1
strsplit(filename.1.1, ",")

filename.2.1 <- sub("(.+)(IL-\\d+\\.\\d+(\\.\\d+)?)(.+)(S[A-Z])(.+)(rep\\d+)(.+)",
                       "\\2,\\5,\\7", filename.2)
filename.2.1
strsplit(filename.2.1, ",")



###########
###########
data(state)
state.name

test1 <- "Oh my... Look over there!"
tolower(test1)
toupper(test1)



#Question 2
state2 <- tolower(state.name)
state2
state3 <- chartr("aeiou", "AEIOU", state2)
state3




###########
###########
#regex gradual more complicated
#first attempts
filename.1.1 <- sub("(.+)(IL-\\w+)(.+)(rep\\d+)(.+)", "\\2,\\4", filename.1)
filename.1.1

filename.1.1 <- sub("(.+)(IL-)(.+)(rep\\d+)(.+)", "\\2,\\4", filename.1)
filename.2.1 <- sub("(.+)(IL-)(.+)(rep\\d+)(.+)", "\\2,\\4", filename.2)

#working except for last capture
filename.1.1 <- sub("(.+)(IL-\\d+\\.\\d+(\\.\\d+)?)(.+)(rep\\d+)(.+)",
                       "\\2,\\5", filename.1)
filename.1.1

filename.2.1 <- sub("(.+)(IL-\\d+\\.\\d+(\\.\\d+)?)(.+)(rep\\d+)(.+)",
                        "\\2,\\5", filename.2)
filename.2.1

#working with all capture groups
filename.1.1 <- sub("(.+)(IL-\\d+\\.\\d+(\\.\\d+)?)(.+)(S[A-Z])(.+)(rep\\d+)(.+)",
                            "\\2,\\5,\\7", filename.1)
filename.1.1

filename.2.1 <- sub("(.+)(IL-\\d+\\.\\d+(\\.\\d+)?)(.+)(S[A-Z])(.+)(rep\\d+)(.+)",
                        "\\2,\\5,\\7", filename.2)
filename.2.1











