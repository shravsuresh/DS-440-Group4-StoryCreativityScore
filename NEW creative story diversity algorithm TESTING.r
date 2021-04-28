

#### All code in this file is licensed under a GPLv3 license, https://www.gnu.org/licenses/gpl-3.0.html
#### All code written and copyrighted by Dan Johnson, Washington and Lee University, 12-24-20, any use or modification of this code should acknowledge the copyright holder

# used to capture creativity in short creative stories
# generate all pairwise comparisons of each word in a story
# then finds semantic distance (cosine()) between each word and takes average by story and by subject
# data is called StoryResults.csv and has raw data in it
# the cue to write a 4-6 sentence story was, stamp-letter-send

library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(tidyr)

load("cbow_subtitle.rda")

library(LSAfun)






#library(LSAfun)
library(psych)
library(lavaan)
library(lme4)
library(lmerTest)
library(yarrr)
#library(MBESS)

library(tidyverse)
#library(LSAfun)
library(tidytext)
library(stringr)
#library(quanteda)
library(tm)


library(shinydashboard)
library(shinyjs)
library(shiny)
library(shinycssloaders)
library(tm)
library(lavaan)
library(DT)

library(tidyr)


load("TASA.rda")
load("cbow_6_ukwac_subtitle.rda")
load("cbow_subtitle.rda")
load("baroni.rda")
load("glove_6B.rda")
glove_matrix <- as.matrix(glove_6B_300d)






# my multiplicative composition model mod of costring() from LSAfun
# this function used to compute semantic distance between 2 words vectors
costring_multi <- function(x,y,tvectors=tvectors,breakdown=FALSE){
  
  if(class(tvectors) == "data.frame"){
    tvectors <- as.matrix(tvectors)
  }else if(class(tvectors) == "textmatrix"){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(class(tvectors) == "matrix"){
    
    if(class(x) != "character"){
      x <- as.character(x)
      message("")
    }
    
    if(class(y) != "character"){
      y <- as.character(y)
      message("")
    }
    
    if(breakdown==TRUE){
      
      satz1 <- breakdown(x)
      satz2 <- breakdown(y)
      
    }
    
    if(breakdown==FALSE){
      
      satz1 <- x
      satz2 <- y
      
    }
    
    
    if(length(satz1) == 1){
      satz1split <- strsplit(satz1,split=" ")[[1]]
    }
    if(length(satz2) == 1){
      satz2split <- strsplit(satz2,split=" ")[[1]]
    }
    
    if(length(satz1)  > 1){satz1split <- satz1}
    if(length(satz2)  > 1){satz2split <- satz2}
    
    
    used1     <- satz1split[satz1split %in% rownames(tvectors)]
    if(length(used1)==0){(warning("no element of x found in rownames(tvectors)"))
      return(NA)}
    
    used2     <- satz2split[satz2split %in% rownames(tvectors)]
    if(length(used2)==0){(warning("no element of y found in rownames(tvectors)"))
      return(NA)}
    
    rest1    <- satz1split[!(satz1split %in% rownames(tvectors))]
    rest2    <- satz2split[!(satz2split %in% rownames(tvectors))]
    
    if(length(used1) >1){satz1vec <- apply(tvectors[used1,],2,prod)}
    if(length(used1)==1){satz1vec <- tvectors[used1,]}
    
    if(length(used2) >1){satz2vec <- apply(tvectors[used2,],2,prod)}
    if(length(used2)==1){satz2vec <- tvectors[used2,]}
    
    cos      <- as.numeric(cosine(satz1vec,satz2vec))
    
    #   out <- list(cos=cos,used1=used1,used2=used2,rest1=rest1,rest2=rest2)
    #   print(out)
    
    return(cos)
    
  }else{stop("tvectors must be a matrix!")}
  
}





# strip() function from textclean package
# remove special characters from text strings for cleaning purposes
strip <- function(x, char.keep = "~~", digit.remove = TRUE, 
                  apostrophe.remove = FALSE, lower.case = TRUE){
  
  UseMethod("strip")
}

#' \code{strip.character} - factor method for \code{strip}.
#' @rdname strip
#' @export
#' @method strip character
strip.character <- function(x, char.keep = "~~", digit.remove = TRUE, 
                            apostrophe.remove = FALSE, lower.case = TRUE){
  
  x <- gsub(
    paste0(
      ifelse(digit.remove, "[0-9]|", ""), "\\\\r|\\\\n|\\n|\\\\t"), 
    " ", 
    x
  )
  
  regex1 <- sprintf(".*?($%s%s|[^[:punct:]]).*?",
                    ifelse(apostrophe.remove, "", "|'"),
                    ifelse(
                      is.null(char.keep), 
                      "", 
                      paste0("|", paste(paste0("\\", char.keep), collapse="|"))
                    )
  )
  
  white <- paste0(
    "^\\s+|\\s+$|\\s+(?=[.](?:\\D|$))|(\\s+)(?=[,]|[;:?!\\]\\}\\)]+)|", 
    "(?<=[\\(\\[\\{])(\\s+)|(\\s+)(?=[\\s])"
  )
  
  x <- gsub(regex1, "\\1", ifelse(lower.case, tolower, c)(x))
  gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", x))
}


#' \code{strip.factor} - factor method for \code{strip}.
#' @rdname strip
#' @export
#' @method strip factor 
strip.factor <- function(x, char.keep = "~~", digit.remove = TRUE, 
                         apostrophe.remove = TRUE, lower.case = TRUE){
  
  strip(as.character(x), char.keep = char.keep, digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove, lower.case = lower.case)
}

#' \code{strip.default} - factor method for \code{strip}.
#' @rdname strip
#' @export
#' @method strip default
strip.default <- function(x, char.keep = "~~", digit.remove = TRUE, 
                          apostrophe.remove = TRUE, lower.case = TRUE){
  
  strip(as.character(x), char.keep = char.keep, digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove, lower.case = lower.case)
}

#' \code{strip.list} - factor method for \code{strip}.
#' @rdname strip
#' @export
#' @method strip list
strip.list <- function(x, char.keep = "~~", digit.remove = TRUE, 
                       apostrophe.remove = TRUE, lower.case = TRUE){
  
  unlist(lapply(x, strip))
}





# function from LSAfun package
# also strips other unwanted characters from text strings for text cleaning
breakdown <- function(x){
  
  x <- tolower(x)
  
  ## Umlaute
  
  x <- gsub(x=x,pattern="\xe4",replacement="ae")
  x <- gsub(x=x,pattern="\xf6",replacement="oe")
  x <- gsub(x=x,pattern="\xfc",replacement="ue")  
  
  ## Accents
  
  x <- gsub(x=x,pattern="\xe0",replacement="a")
  x <- gsub(x=x,pattern="\xe1",replacement="a")
  x <- gsub(x=x,pattern="\xe2",replacement="a")
  
  x <- gsub(x=x,pattern="\xe8",replacement="e")
  x <- gsub(x=x,pattern="\xe9",replacement="e")
  x <- gsub(x=x,pattern="\xea",replacement="e")
  
  x <- gsub(x=x,pattern="\xec",replacement="i")
  x <- gsub(x=x,pattern="\xed",replacement="i")
  x <- gsub(x=x,pattern="\xee",replacement="i")
  
  x <- gsub(x=x,pattern="\xf2",replacement="o")
  x <- gsub(x=x,pattern="\xf3",replacement="o")
  x <- gsub(x=x,pattern="\xf4",replacement="o")
  
  x <- gsub(x=x,pattern="\xf9",replacement="u")
  x <- gsub(x=x,pattern="\xfa",replacement="u")
  x <- gsub(x=x,pattern="\xfb",replacement="u")
  
  
  x <- gsub(x=x,pattern="\xdf",replacement="ss")
  
  ## Convert to ASCII
  
  x <- iconv(x,to="ASCII//TRANSLIT")
  
  ## Punctation, Numbers and Blank lines
  
  x <- gsub(x=x,pattern="[[:punct:]]", replacement=" ")
  x <- gsub(x=x,pattern="[[:digit:]]", replacement=" ")
  x <- gsub(x=x,pattern="\n", replacement=" ")
  x <- gsub(x=x,pattern="\"", replacement=" ")
  
  
  return(x)  
}






# this .csv files has creative stories and human ratings of those stories
# story column has the creative stories
# Rater1-Rater5, 5 human raters that rated each story on 1-5 scale of creativity
# ID column is id for each subject, who wrote only one story

story_data <- read_csv("StoryResults.csv") 


# Tokenize by sentence 
# breaks stories into sentences
story_sentences <- story_data %>% 
  select(ID, Story) %>%
  unnest_tokens(output = sentence, input = Story, token = "sentences") %>%
  group_by(ID) %>%
  mutate(sentence_num = 1:n())

# remove stop words and clean
# cleans data of special characters and numbers and remove filler words like and, then, a, etc.
out <- story_sentences %>%
  rename(response = sentence)


response_c <- gsub("[^[:alnum:]///' ]", "", out$response)
out$response <- NULL
out$response <- response_c
response <- removeWords(out$response, stopwords("english"))
response <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", response)
response <- sub("'", " ", response)
response <- breakdown(response)
response <- strip(response)
out$response_nofill <- response

story_sentences <- out %>%
  rename(sentence = response_nofill) %>%
  select(-response) 


# Tokenize by word 
# breaks sentences into words
story_words <- story_sentences %>% 
  unnest_tokens(output = word, input = sentence, token = "words") %>%
  group_by(ID) %>%
  mutate(word_num = 1:n()) 


# transforms data from long to wide format so that a matrix of pairwise comparisons between each words can  be created
div_spread <- story_words %>%
  pivot_wider(names_from = word_num, values_from = word, names_prefix = "w") 

# next 2 steps separate words from the ID, so that a matrix of just the words can be created and then merged back with ID later
div_cond <- div_spread %>%
  select(ID)

div_data <- div_spread %>%
  ungroup() %>%
  select(
    -(ID)
  )


# created matrix of all pairwise comparisons
colPairs <- combn(colnames(div_data), 2)

# computes semantic distance with the costring_multi function from above
# using the subtitle semantic space, made up of thousands of film scripts
# the semantic space called "eng_subtitle_matrix"
# cbow stands for continuous bag of words, with is the word2vec approach to creating word vectors from the subtitle corpus
# see semdis.wlu.psu.edu in the "Interpreting Semantic Distance" section for detail and references on how the cbow subtitle space was created
div_matrix <- sapply(1:(ncol(colPairs)), function(ncol) {
  
  columns <- colPairs[,ncol]
  sapply(1:(nrow(div_data)), function(nrow) {
    costring_multi( div_data[nrow, columns[1]], 
                    div_data[nrow, columns[2]], 
                    eng_subtitle_matrix,
                    breakdown = TRUE
    )
  })
  
})

div_all <- as.data.frame(div_matrix)
colnames(div_all) <- paste( t(colPairs)[,1], t(colPairs)[,2], sep = "_")

# this transforms all pairwise semantic distances between each pair of words into long form
# then computes a mean per subject of all those semantic distances
div_cbowsubtitle <- bind_cols(div_cond, div_all) %>%
  gather(2:ncol(div_all), key = "pair", value = "dis") %>%
  group_by(ID) %>%
  summarize(div_cbowsubtitle = 1 - mean(dis, na.rm = TRUE)) %>%
  select(ID, div_cbowsubtitle)
