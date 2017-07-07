library(reshape2)
library(tidyr)
library(dplyr)
###### Read in and mutuate data #####################################
toydata <- read.csv("RawData/toyAssessmentData.csv",
                    header = TRUE, 
                    row.names = 1, 
                    stringsAsFactors = FALSE)


## Let's group appropriate codes:

codesToKeep <- c("ie-e", "de-r", "ie-a", "ie-m", "ie-s")
codesToChange <- c("(ep)", "(ldm)", "(ade)", "(me)", "(se)")

#this data frame is not used but can be referenced
#to see which terms are grouped. 
codeChanges <- cbind(codesToKeep,codesToChange)

#Change all error codes
for (i in 1:length(codesToChange)){
  toydata$errorcode1 <- 
    gsub(codesToChange[i],codesToKeep[i],
         toydata$errorcode1)
  toydata$errorcode2 <- 
    gsub(codesToChange[i],codesToKeep[i],
         toydata$errorcode2)
}


allcode1s <- strsplit(toydata$errorcode1, ";")
allcode2s <- strsplit(toydata$errorcode2, ";")
allcode1and2s <- list(allcode1s, allcode2s)
coursenames <- unique(toydata$Course)

byTest <- function(data, testNum) {
  coursecodes <- list()
  
  ## splitting codes by semicolon, ###########
  ## counting each & finding percentage ######
  for(i in 1:length(coursenames)){
    whichcourse <- data$Course==coursenames[i]
    if(testNum == 0) {
      code1 <- unlist(allcode1s[whichcourse])
      code2 <- unlist(allcode2s[whichcourse])
      counts <- table(c(code1, code2))
      testcount <- 2 * nrow(data[whichcourse, ])
    } 
    else {
      codes <- unlist(allcode1and2s[[testNum]][whichcourse])
      counts <- table(c(codes))
      testcount <- nrow(data[whichcourse, ])
    }
    ## This could be problematic 
    ## e.g count of any error coulbe > studcount because of 2 tests
    percs <- counts/testcount
    parentcode <- c()
    for (code in names(counts)) {
      parentcode <- c(parentcode, strsplit(code, split = "-")[[1]][1])
    }
    
    tograph <- data.frame(codes = names(counts),
                          counts = as.vector(counts), 
                          percs = as.vector(percs), 
                          testcount = testcount,
                          parentcode = parentcode)
    coursecodes[[i]] <- tograph
  }
  names(coursecodes) <- coursenames
  return(coursecodes)
}
test1 <- byTest(toydata[-c(6, 7)], 1)
test2 <- byTest(toydata[-c(4,5)], 2)
testAll <- byTest(toydata, 0)

courseresults = list()
for(i in 1:7) {
  courseresults[[i]] = list(Overall = testAll[[i]], test1 = test1[[i]], test2 = test2[[i]])
}

names(courseresults) <- coursenames

names(testAll) <- coursenames
allcodes <- melt(testAll, 
                 id.vars = c("codes", "counts", "percs", "studcount", "testcount"))
names(allcodes)[6] <- "course"
## creating labels for graph to show number of students per course
allcodes$label <- paste(allcodes$course, 
                        paste(allcodes$studcount, "students,", allcodes$testcount, "tested"), 
                        sep = " - ")

###### Means and Median per criteria##################################
c1mean <- tapply(toydata$score1, INDEX = toydata$Course, FUN = mean)
c1median <- tapply(toydata$score1, INDEX = toydata$Course, FUN = median)
c2mean <- tapply(toydata$score2, INDEX = toydata$Course, FUN = mean)
c2median <- tapply(toydata$score2, INDEX = toydata$Course, FUN = median)
allmean <- tapply(c(toydata$"score1", toydata$"score2"), 
                  INDEX = rep(toydata$Course, 2),
                  FUN = mean)
allmedian <- tapply(c(toydata$"score1", toydata$"score2"), 
                    INDEX = rep(toydata$Course, 2),
                    FUN = median)
meansmedsdf <- data.frame(c1mean, c1median, 
                          c2mean, c2median, 
                          allmean, allmedian)


###### Percent and frequency abovee 2.5 #############################
# here we pipe the data rather than using tapply/do.call
# since we want the high scores *per test* and overall

temp <- toydata %>% group_by(Course) %>% summarize(n = n())

highscores1 <- toydata %>% group_by(Course) %>%
  mutate(numberStudents1 = n()) %>%
  filter(score1>2.5) %>% 
  mutate(freq2.5Test1 = n(),
         perc2.5Test1 = freq2.5Test1/numberStudents1) %>%
  summarize(freq2.5Test1 = first(freq2.5Test1),
            perc2.5Test1 = first(perc2.5Test1),
            numberStudents1 = first(numberStudents1))


highscores2 <- toydata %>% group_by(Course) %>%
  mutate(numberStudents2 = n()) %>%
  filter(score2>2.5) %>% 
  mutate(freq2.5Test2 = n(),
         perc2.5Test2 = freq2.5Test2/numberStudents2) %>%
  summarize(freq2.5Test2 = first(freq2.5Test2),
            perc2.5Test2 = first(perc2.5Test2),
            numberStudents2 = first(numberStudents2))


highscoresnew <- cbind(highscores1[,c(2,3,4)], highscores2[,c(2,3,4)]) %>%
  mutate(highScoresAllFreq = freq2.5Test1+freq2.5Test2,
         highScoresAllPerc = highScoresAllFreq/
           (numberStudents1+numberStudents2))

highscores <- tapply(c(toydata$"score1", toydata$"score2"), 
                     INDEX = rep(toydata$Course, 2), 
                     FUN = function(x) {y <- table(x>2.5); 
                     data.frame(freqAbove2.5 = y[2], 
                                percAbove2.5 = 100*y[2]/sum(y))})
# do.call() with rbind takes the elements of  
# highscores and rbinds them into one data frame 
highscoresdf <- do.call(rbind, highscores)


###### Results table ################################################
results <- merge(meansmedsdf, highscoresdf, by = "row.names")

#lets put together the high scores for both tests.

results <- cbind(results, highscoresnew)

countCodes <- allcodes %>% group_by(course) %>%
  summarize(freqErrorPerCourse = sum(counts))

results <- cbind(results, countCodes)

# this code below gives us 3 columns for 
names(results)[1] <- "course"


