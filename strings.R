#strings in R
#creating a string
"Hello, R"

#assigning a string to a variable
new_str <- "Hello, R"

#creating multiple line string
multiline_str <- "Hello, the R programming langauge is powerful,
R is fun!"
cat(multiline_str)

#length of a string
nchar(multiline_str)
nchar(new_str)

#logical check of a string
grepl("H",multiline_str)

#string concatenation
strl1 <- "Hi, my name is"
strl2 <- "R programming language"

paste(strl1,strl2)


#escape characters in string
str2 <- "R \"programming\"\n Language!"
cat(str2)
