pew <- pew.jan.2016 <- as.data.frame(foreign::read.spss("/users/mohanty/Downloads/jan16/Jan16 public.sav"))
# http://www.people-press.org/category/datasets/?download=20059536
# jan16.public.que.pdf

set.seed(2016)

# Q.21 Thinking ahead to the presidential election that will take place in November 2016, regardless of who the nominees from each party are, do you think you will [READ; READ CATEGORIES IN REVERSE ORDER FOR RANDOM HALF OF SAMPLE]?
# 1 Definitely vote for the Republican
# 2 Probably vote for the Republican [OR]
# 3 Probably vote for the Democrat [OR]
# 4 Definitely vote for the Democrat
# 5 [VOL. – DO NOT READ] Other/Neither
# 6 [VOL. – DO NOT READ] Too early to say/Need to know the nominees
# 9 [VOL. – DO NOT READ] Don’t know/Refused # seems to be 7

table(as.numeric(pew$q21))
sum(as.numeric(pew$q21) > 4) # 388 of 2009 so 19.3% don't say they at least probably will vote mainstream

vote.plan <- matrix(ncol = 7, nrow = nrow(pew), 0)
vote.plan[cbind(1:nrow(pew), pew$q21)] <- 1
colnames(vote.plan) <- c("defGOP", "probGOP", "probDEM", "defDEM", "other_neither", "too_early", "dk_vote")

table(pew$reg) 

reg <- matrix(ncol=4, nrow=nrow(pew),0)
reg[cbind(1:nrow(pew), pew$reg)] <- 1
colnames(reg) <- c("def_registered", "prob_registered", "not_registered", "dk_registered") 


vote <- as.data.frame(cbind(vote.plan, reg))

pca <- princomp(vote)
pca$loadings

library(polycor)
pca.hetcor <- princomp(covmat = as.matrix(hetcor(vote)))
pca.hetcor$loadings
summary(pca.hetcor)

vote2 <- vote[,c(1:4)]
vote2 <- cbind(vote2, rowSums(vote[,5:7]), rowSums(vote[,10:11]))

pca.het2 <- princomp(x = vote2, covmat = as.matrix(hetcor(vote2)), scores = T)
pca.het2$loadings
summary(pca.het2)

pca.het2$scores

# A year from now, a new president will be inaugurated...
# Q.22 Regardless of who you currently support, I’d like to know what kind of president you think each of the following would be if elected in November 2016? First, [INSERT NAME; RANDOMIZE]. If [INSERT NAME] were to become president do you think (he/she) would be a great, good, average, poor, or terrible president? How about if [INSERT NEXT ITEM] were to become president? [IF NECESSARY: do you think he/she would be a great, good, average, poor, or terrible president? [PROGRAMMING NOTE: For item e. program “she”]

# RESPONSE CATEGORIES:
#1 Great president
#2 Good president
#3 Average president
#4 Poor president
#5 Terrible president
#8 Never heard of candidate (VOL.)
#9 Don’t know/Refused (VOL.)

trump.terrible <- as.numeric(pew$q22d)
trump.terrible[trump.terrible > 5] <- sample(trump.terrible[trump.terrible < 5], sum(trump.terrible > 5), F)
trump.great <- 5 - trump.terrible
hillary.terrible <- as.numeric(pew$q22e)
hillary.terrible[hillary.terrible > 5] <- sample(hillary.terrible[hillary.terrible < 5], sum(hillary.terrible > 5), F)
hillary.great <- 5 - hillary.terrible
# cor(hillary.great, trump.great) # -0.5089616 wth seed 2016
trump.better.than.hillary <- trump.great - hillary.great

respondents <- as.data.frame(trump.better.than.hillary) # dependent variable

respondents$female <- pew$sex == "Female"
respondents$spanish.language.interview <- pew$ilang ==  "Spanish" # 122, ~6.1% 

respondents$liberalism <- as.numeric(pew$ideo) 
respondents$liberalism[respondents$liberalism == 6] <- sample(as.numeric(pew$ideo)[as.numeric(pew$ideo) != 6], 
                                                              sum(as.numeric(pew$ideo)[as.numeric(pew$ideo) == 6]), replace=F)
respondents$approve.obama <- pew$q1 == "Approve"
respondents$follows.election <- 5 - as.numeric(pew$q20)
respondents$age <- pew$age
respondents$age[pew$age == 99] <- sample(pew$age, sum(pew$age == 99), replace = F)
edu <- as.numeric(pew$educ)
edu[edu == 9] <- sample(edu, sum(edu == 9), replace = F)
respondents$edu <- edu
# for categorical...
#tmp <- model.matrix(~ as.factor(edu))[, 2:8]
#colnames(tmp) <- c("didnt_finish_HS", "HS_diploma", "some_college", "associates", "bachelors", 
#                   "some_postgrad", "professional_degree") # reference category = "didn't start high school"
#respondents$edu <- tmp
class <- pew$q75
class[class == "(VOL) Don't know/Refused"] <- sample(class[class != "(VOL) Don't know/Refused"], sum(class == "(VOL) Don't know/Refused"), replace=F)
respondents$class <- 5 - as.numeric(class)
# for categorical
#class <- model.matrix(~class)[,2:5]
#colnames(class) <- c("upper_middle", "middle", "lower_middle", "lower") 
#upper.middle       middle lower.middle        lower 
#0.179        0.526        0.198        0.069 
#respondents$class <- class
respondents$population.density <- pew$density
respondents$Hispanic <- pew$hisp == "Yes" # 284 = ~14% 
tmp1 <- pew$racem1
tmp1[tmp1 == "(VOL) Refused (e.g., non-race answers like American, Human, purple)"] <- sample(tmp1, sum(tmp1 == "(VOL) Refused (e.g., non-race answers like American, Human, purple)"), replace = F)
tmp1[tmp1 == "(VOL) Don't know"] <- sample(tmp1, sum(tmp1 == "(VOL) Don't know"), replace = F)
race <- model.matrix(~ as.factor(as.numeric(tmp1)))
for(i in 2:7){
  race[which(pew$racem2 == levels(pew$racem1)[i]), i] <- 1
}
colnames(race)[2:7] <- c("Black", "Asian", "Other", "Native_American", "Pacific_Islander", "Hispanic.VOL")
respondents$race <- race[,2:7]
religion <- model.matrix(~pew$relig)[,2:15]
colnames(religion) <- c("Catholic", "Mormon", "Orthodox", "Jewish", "Muslim", "Buddhist", "Hindu", "Atheist", "Agnotic", 
                       "Other", "Nothing", "Christian.VOL", "Unitarian.VOL", "dont_know.VOL")
respondents$religion  <- religion # leaving refused alone as own category

state <- model.matrix(~ pew$state)[,2:58] # state or territory
colnames(state) <- levels(pew$state)[2:58]
state <- state[, -which(apply(state, 2, sd) == 0)] # dropping pew$stateAS pew$stateFM pew$stateGU pew$stateMH pew$stateMP pew$statePR pew$stateVI 
respondents$state <- state
region <- model.matrix(~ pew$cregion)[,2:4]
colnames(region) <- c("Midwest", "South", "West")
respondents$region <- region

respondents <- as.matrix(respondents)
head(respondents, 2)





## for useR!
pew.results <- bigKRLS(y = as.matrix(respondents[,1]), X = as.matrix(respondents[,-1]))
save(pew.results, file="pew.results.seed2016.rdata")
summary(pew.results)

# states_with_DC <- sort(unique(pew$state))[table(pew$state) > 0][match(pew$state, sort(unique(pew$state))[table(pew$state) > 0]) - 1]

barplot(by(pew.results$derivatives[,3] - mean(pew.results$derivatives[,3]), 
                     pew$state, mean)[1:51])

barplot(by(pew.results$derivatives[,1], pew$state, mean)[1:51]) 
# gender gap high variance by state?


by(pew.results$derivatives[pew$racem1 == "White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)"
,8], respondents[pew$racem1 == "White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)"
,8], mean)

t.test(pew.results$derivatives[pew$racem1 == "White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)",8])

plot(y = pew.results$derivatives[pew$racem1 == "White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)"
,8], x = respondents[pew$racem1 == "White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)"
,8])

pew$racem1 == "White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)"


# pew.out <- bigKRLS(y = as.matrix(respondents[,5]), X = as.matrix(respondents[,-5])) # started 7:32:27 pm, found lambda by 7:34:53, started the 50 states 20 mins later
# finished 8:23:13 pm. elapsed time 49 minutes.

cow.data <- read.csv("/users/mohanty/downloads/Non-StateWarData_v4.0.csv")
