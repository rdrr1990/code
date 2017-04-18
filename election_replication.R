library(bigKRLS)
library(knitr)
library(ggplot2)
library(stringr)
library(maps)

# for notes on sources see https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnxwZXRlbW9oYW50eXxneDo2ZjM5NWRlZjNhY2EzYmVi
# or email pmohanty@stanford.edu

# exploration and prep
data$dem_2016_percent <- 100*data$dem_2016/(data$dem_2016 + data$gop_2016)
data$gop_2016_percent <- 100*data$gop_2016/(data$dem_2016 + data$gop_2016)
data$dem_2012_percent <- 100*data$dem_2012/(data$dem_2012 + data$gop_2012) # 2012 missing a few, handled below
data$gop_2012_percent <- 100*data$gop_2012/(data$dem_2012 + data$gop_2012)

data$white_population[is.na(data$white_population)] <- 0
data$latino_population[is.na(data$latino_population)] <- 0
data$black_population[is.na(data$black_population)] <- 0
data$asian_population[is.na(data$asian_population)] <- 0

data$percent_white <- 100*data$white_population/data$total_population
data$percent_latino <- 100*data$latino_population/data$total_population
data$percent_black <- 100*data$black_population/data$total_population
data$percent_asian <- 100*data$asian_population/data$total_population

data$all_mortality_2009.2011 <- data$all_mortality_2009.2011/100
data$all_mortality_2013.2015 <- data$all_mortality_2013.2015/100
data$mortality_delta <- data$all_mortality_2013.2015 - data$all_mortality_2009.2011

data$gop_2016_delta <- data$gop_2016_percent - data$gop_2012_percent

data$percent_poverty <- 100*data$POVALL_2015/data$total_population

data$Median_Household_Income_2015 <- data$Median_Household_Income_2015/10000
data$AGE050210D <- data$AGE050210D/10

# creating the model preliminaries
X <- data.frame('all_mortality' = data$all_mortality_2013.2015)
X$mortality_delta <- data$mortality_delta

X$unemployment <- data$Unemployment_rate_2015
X$rural <- data$Rural.urban_Continuum_Code_2013

X$age <- data$AGE050210D
X$income <- data$Median_Household_Income_2015
X$poverty <- data$percent_poverty

X$high_school_dropout <- data$Percent.of.adults.with.less.than.a.high.school.diploma..2011.2015
X$high_school_grad <- data$Percent.of.adults.with.a.high.school.diploma.only..2011.2015
X$some_college <- data$Percent.of.adults.completing.some.college.or.associate.s.degree..2011.2015
X$college_grad <- data$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2011.2015

X$percent_white <- data$percent_white
X$percent_latino <- data$percent_latino
X$percent_black <- data$percent_black
X$percent_asian <- data$percent_asian

X$lat <- data$lat
X$lon <- data$lon

expanded_medicaid <- data$state %in% c('AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'HI', 'IL', 'IN', 'IA', 'KY', 'MD',
                                       'MA', 'MI', 'MN', 'NV', 'NH', 'NJ', 'NM', 'NY', 'ND', 'OH', 'OR', 'PA', 'RI', 'VT', 'WA', 
                                       'WV')

# alaska is the excluded category
states <- model.matrix(~data$state)[,2:51]
colnames(states) <- sort(unique(data$state))[2:51]

X <- cbind(X, states)

# fitting the model
gop_2016_delta <- data$gop_2016_delta
complete <- complete.cases(X) & !is.na(gop_2016_delta)
gop_2016_delta <- gop_2016_delta[complete]
X <- X[complete,]
X <- as.matrix(X)

d <- list(gop_2016_delta, X)
save(d, file="election2016.rdata")

out <- bigKRLS(gop_2016_delta, X)
X = bigKRLS_out$X
out = bigKRLS_out

# AME table
labels <- c(c('Age-Adjusted Mortality',
              'Change in 3-Year Mortality',
              'Unemployment',
              'Urban-Rural Continuum',
              'Age',
              'Household Income',
              'Poverty',
              'High School Dropout',
              'High School Graduate',
              'Some College',
              'College Graduate',
              'Proportion White',
              'Proportion Latino',
              'Proportion Black',
              'Proportion Asian'),
            colnames(X)[16:67])
kable(summary(out, labs=labels)[[1]][1:15,], digit=5)


# mapping effects - change j to change value being mapped
j <- 1
geo_df <- data.frame('region'=character(), 'subregion'=character(), 'coeff'=numeric(), stringsAsFactors = F)
for(m in 1:nrow(X)){
  geo_df <- rbind(geo_df,
                  data.frame('region'= data[complete,]$state[m],
                             'fips'=data[complete,]$fips[m],
                             'coeff'=out$derivatives[m,j]))
}

geo_df$region <- state.name[match(geo_df$region,state.abb)]
geo_df$region <- tolower(geo_df$region)
fips <- cbind(county.fips, matrix(unlist(strsplit(as.character(county.fips$polyname), ',')), ncol=2, byrow=T))
names(fips) <- c('fips', 'polyname', 'region', 'subregion')
counties_geo <- map_data("county")
counties_geo <- merge(counties_geo, fips, by=c('region', 'subregion'))
states_geo <- map_data("state")
map.df <- merge(counties_geo,geo_df, by=c("region", "fips"), all.x=T)
map.df <- map.df[order(map.df$order),]
countymap = ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=coeff))+
  geom_path()+ 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(title=colnames(X)[j])) + coord_map()
countymap2 = countymap + geom_path(colour = "white", data = states_geo) 


# plotting non-linear effects - change k
k <- 1

print(colnames(X)[k])
ggplot(NULL) + geom_point(aes(as.numeric(X[,k]) ,y=out$derivatives[,k]), alpha = 1, size=.1, color='grey') + 
  geom_smooth(aes(as.numeric(x=X[,k]), y=out$derivatives[,k]), method='loess') + xlab('Mortality') + ylab('Marginal Derivative of Mortality') + 
  geom_hline(aes(yintercept=0), linetype='dashed') +
  theme_minimal() +
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  coord_cartesian(ylim=c(-4e-5, 4e-5))

# interacting effects with %white - change k
k <- 1
ggplot(NULL) + geom_point(aes(as.numeric(X[,12]) ,y=out$derivatives[,k]), alpha = 1, size=.1, color='grey') + 
  geom_smooth(aes(as.numeric(x=X[,12]), y=out$derivatives[,k]), method='loess') + xlab('Percent White') + ylab('Marginal Derivative of Mortality') + 
  geom_hline(aes(yintercept=0), linetype='dashed') +
  theme_minimal() + 
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  coord_cartesian(ylim=c(-4e-5, 4e-5))

# interacting by certain states
k <- 1
ggplot(NULL) + geom_point(aes(as.numeric(X[,1]) ,y=out$derivatives[,k], color=expanded_medicaid), 
                          alpha = 0.3, size=.1) + 
  geom_smooth(aes(as.numeric(x=X[,1]), y=out$derivatives[,k], color=expanded_medicaid), method='loess') + 
  xlab('Mortality') + ylab('Marginal Derivative of Mortality') + 
  geom_hline(aes(yintercept=0), linetype='dashed') +
  theme_minimal() + 
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  scale_color_discrete(guide_legend(title='Expanded Medicaid')) +
  coord_cartesian(ylim=c(-0.4, 0.4))

# plotting predicted changes - change j
j <- 1
to_pred <- X
to_pred[,j] <- to_pred[,j] + sd(to_pred[,j])
p <- predict.bigKRLS(out, to_pred)
p_delta <- p$fit - out$yfitted

geo_df <- data.frame('region'=character(), 'subregion'=character(), 'coeff'=numeric(), stringsAsFactors = F)
for(m in 1:nrow(X)){
  geo_df <- rbind(geo_df,
                  data.frame('region'= data[complete,]$state[m],
                             'fips'=data[complete,]$fips[m],
                             'change'=p_delta[m]))
}

geo_df$region <- state.name[match(geo_df$region,state.abb)]
geo_df$region <- tolower(geo_df$region)

fips <- cbind(county.fips, matrix(unlist(strsplit(as.character(county.fips$polyname), ',')), ncol=2, byrow=T))
names(fips) <- c('fips', 'polyname', 'region', 'subregion')
counties_geo <- map_data("county")
counties_geo <- merge(counties_geo, fips, by=c('region', 'subregion'))

map.df <- merge(counties_geo,geo_df, by=c("region", "fips"), all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=change))+
  geom_path(size=.05)+ 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(title='Mortality'))+
  coord_map() + theme_minimal() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(-2, 0, -2, 0),units = 'line'))

library(glmnet)
Xglm = d[[2]]
Xglm[,1:2] <- X[,1:2] * 10000 # putting mortality on similar scale
glmout <- glmnet(Xglm, gop_2016_delta) #glmnet((d[[2]] - colMeans(d[[2]]))/apply(d[[2]],2,sd),
#       (gop_2016_delta - mean(gop_2016_delta))/sd(gop_2016_delta))
plot(glmout)
print(glmout)

Xinteractions <- Xglm
for(i in 1:17){
  tmp <- Xinteractions[,i]*Xinteractions[,1:17]
  colnames(tmp) <- paste(colnames(Xinteractions)[1:17], "_X_", 
                         colnames(Xinteractions)[i], sep="")
  Xinteractions <- cbind(Xinteractions, tmp[,(i-1):ncol(tmp)])
}

glminteractions = glmnet(Xinteractions, gop_2016_delta)
betasX = as.matrix(glminteractions$beta)
sort(unlist(lapply(apply(betasX, 1, function(x) which(x != 0)), min)))

glminteractions.cv = cv.glmnet(Xinteractions, gop_2016_delta)
coef(glminteractions.cv, s="lambda.min")

betasACM = as.matrix(glminteractions$beta[grep("all_mortality", colnames(Xinteractions)), ])
sort(unlist(lapply(apply(betasACM, 1, function(x) which(x != 0)), min)))

round(glmout$beta[1:15,], 3)
betas = as.matrix(glmout$beta[1:15,])
sort(unlist(lapply(apply(betas, 1, function(x) which(x != 0)), min)))

x = glmout
glmnet:::plotCoef(x$beta[1:15,], lambda = x$lambda, df = x$df, dev = x$dev.ratio, 
                  label = FALSE, xvar = c("norm", "lambda", "dev"), ylab = "Coefficients (Beta)")

rf = randomForest(X, gop_2016_delta)
print(rf)



