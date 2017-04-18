pew <- pew.jan.2016 <- as.data.frame(foreign::read.spss("/users/mohanty/Downloads/jan16/Jan16 public.sav"))
# http://www.people-press.org/category/datasets/?download=20059536

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

set.seed(2016)

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

N <- nrow(pew)
edu.tmp <- matrix(nrow=N, ncol=length(unique(pew$educ)),0)
for(i in 1:N){
  edu.tmp[i,match(pew$educ[i], unique(pew$educ))] <- 1
}
colnames(edu.tmp) <- c("Bachelors", "Associates", "Some Postgrad", "High School", 
                       "Postgrad", "Some college", "Refused", "Some High School", "No High School")
respondents$edu <- edu.tmp

class.tmp <- matrix(nrow=N, ncol=length(unique(pew$q75)), 0)
for(i in 1:N){
  class.tmp[i,match(pew$q75[i], unique(pew$q75))] <- 1
}
colnames(class.tmp) <- unique(pew$q75)
colnames(class.tmp)[3] <- "DK or Refused"

respondents$population.density <- pew$density
respondents$Hispanic <- pew$hisp == "Yes" # 284 = ~14% 

race.tmp <- matrix(nrow=N, ncol=length(unique(pew$racem1)),0)
for(i in 1:N){
  race.tmp[i,match(pew$racem1[i], unique(pew$racem1))] <- 1
  if(!is.na(pew$racem2[i])){race.tmp[i,match(pew$racem2[i], unique(pew$racem1))] <- 1}
  if(!is.na(pew$racem3[i])){race.tmp[i,match(pew$racem3[i], unique(pew$racem1))] <- 1}
  if(!is.na(pew$racem4[i])){race.tmp[i,match(pew$racem4[i], unique(pew$racem1))] <- 1}
}
race.tmp[,6] <- race.tmp[,6] + race.tmp[,7]
race.tmp <- race.tmp[,-7]
colnames(race.tmp) <- c("White", "Refused", "Hispanic Latino",
                        "African American", "Native American",
                        "Other", "Asian or Asian American", "Pacific Islander or Hawaiian")

respondents$race <- race.tmp

rel.tmp <- matrix(nrow=N, ncol=length(unique(pew$relig)),0)
rel.tmp[cbind(1:N, match(pew$relig, unique(pew$relig)))] <- 1
colnames(rel.tmp) <- c("Protestant", "Nothing", "Agnostic", "Catholic", "Hindu","Orthodox",
                       "Jewish", "Buddhist", "Refused", "Christian_VOL", "Other", 
                       "Atheist", "Mormon", "Muslim", "Unitarian")

region.tmp <- matrix(nrow=N, ncol=4, 0)
region.tmp[cbind(1:N, match(pew$cregion, unique(pew$cregion)))] <- 1
colnames(region.tmp) <- unique(pew$cregion)
respondents$region <- region.tmp

state <- model.matrix(~ pew$state)[,2:58] # state or territory with DC
colnames(state) <- levels(pew$state)[2:58]
state <- state[, -which(apply(state, 2, sd) == 0)] # dropping territories pew$stateAS pew$stateFM pew$stateGU pew$stateMH pew$stateMP pew$statePR pew$stateVI 
state <- cbind(0, state)
colnames(state)[1] <- "AL"
state[,1] <- ifelse(rowSums(state[,2:51]) == 0, 1, 0)
respondents$state <- state

respondents <- as.matrix(respondents)

save(respondents, file="pew_apsa.rdata")

load("pew_apsa.rdata")
