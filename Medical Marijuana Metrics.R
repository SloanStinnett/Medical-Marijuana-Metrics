# pulling in the data from the file on the Oklahoma Medical Merijuana Authority's website
library(dplyr)
library(tabulizer)
# loading the information from the pdf supplied by OMMA
location <- "OMMA Dispensaries List.pdf"
data <- extract_tables(location)
# creating the frame work for the dataframe with proper variable names
table.final <- as.data.frame(t(rep(NA,4)))
colnames(table.final) <- c("Name","City","ZIP","County")
# since each of the tables for every page of the report is brought in as a matrix and then put into a list 
# I have a loop go through and take each of the matrices and convert into a data frame where each row is a licensed dispensary.
for ( i in 1:128){
  # converting the matrix to a dataframe
  table <- as.data.frame(data[[i]])
  # removing epty rows and rows that only contained the license ID number
  table.cleaned <- table[table$V1 != '', ]
  table.cleaned <- table.cleaned[!grepl("License ID #",table.cleaned$V1),]
  # setting the column names to match the final structure so that they can be easily bound
  colnames(table.cleaned)<- c("Name","City","ZIP","County")
  # binding the new observations to the dataframe
  table.final<-rbind(table.final,table.cleaned)
}
# removing the empty row that we created in line 9 to make the dataframe
table.final<-table.final[-1,]
# converting all the variables to factor variables as they are all categorical
for (i in 1:4){
  table.final[,i]<-as.factor(table.final[,i])
}
# seeing if any of the observations are missing the city or the county column 
no.city <- table.final[table.final[,2]=='',]
no.county <- table.final[table.final[,4]=='',]
# there are but they all seem to be spill over charecters from a particular chain with a very long name
# we will remove these to keep things clean, they are also the same lines so we only need to use one line 
table.final <- table.final[table.final[,2]!='',]
# seeing which counties  and cities have dispensaries and how many they have 
counts.disp.counties <- as.data.frame(table(table.final$County))
counts.disp.counties <- counts.disp.counties[-1,]

counts.disp.cities <- as.data.frame(table(table.final$City))
counts.disp.cities <- counts.disp.cities[-1,]
# out of the 77 counties in Oklahoma 71 have atleast one registered counties
# out of the 600 municipalities in the shapefile 243 have atleast one licensed dispensary
# average number of dispensaries in a municipality and county
avg.disps.count <- sum(counts.disp.counties$Freq)/77
avg.disps.municipality <- sum(counts.disp.cities$Freq)/(594)


# Pulling in Census data on poverty, income, and demographic variables
library(censusapi)
Sys.setenv(CENSUS_KEY="13629de82ee4b4001ff142ca1560de9b8be6e76a")
#  The variables are in this order Name,Totpop,median household income,population white,population in poverty, pop 20-24, pop 25-34,total working population,pop in the workforce, pop 18-64, pop18-64 unisured, pop18-64 insured public no dis,pop18-64 insured public dis, pop18-64 insured private no dis, pop18-64 insured private dis
# pop18-64 with a disability
# unnote the nextline if you want to see a list of all variables that can be brought in from the acs
# variables <- listCensusMetadata(name = "acs/acs5", vintage = 2015 )
variables.municipalities <- getCensus(name = "acs/acs5", 
                                vintage =2018 , 
                                vars = c("NAME","B01003_001E","B19013I_001E","B02001_002E","B17001I_002E","B07001_005E","B06001_005E","B23025_001E","B23025_002E",
                                         "B18135_013E","C27001F_007E","B18135_022E","B18135_017E","B18135_021E","B18135_016E","B18101E_006E"),
                                region = "place:*" )
variables.municipalities.17 <- getCensus(name = "acs/acs5", 
                                      vintage = 2017 , 
                                      vars = c("NAME","B01003_001E","B19013I_001E","B02001_002E","B17001I_002E","B07001_005E","B06001_005E","B23025_001E","B23025_002E",
                                               "B18135_013E","C27001F_007E","B18135_022E","B18135_017E","B18135_021E","B18135_016E","B18101E_006E"),
                                      region = "place:*" )
# extracting only the results for Oklahoma by the fips code 
variables.model.muni <- variables.municipalities[variables.municipalities$state == 40,]
variables.model.muni.17 <- variables.municipalities.17[variables.municipalities.17$state == 40,]
# changing the Place variable to a numerical variable so that
# It can be matched to the shapefile for municipalities
variables.model.muni$place <- as.numeric(variables.model.muni$place)
variables.model.muni.17$place <- as.numeric(variables.model.muni.17$place)

# creating more readable variable names 
colnames(variables.model.muni) <- c("StateFIP","CountyFIP","Name","Totpop","MedHosHolInc","PopWhite","PopPov","Pop20-24","Pop25-34","TotWorkingPop","PopInWF","Pop18-64","PopUnisured","PopINP","PopIDP","PopINPr","PopIDPr", "PopDis")
colnames(variables.model.muni.17) <- c("StateFIP","CountyFIP","Name","Totpop","MedHosHolInc","PopWhite","PopPov","Pop20-24","Pop25-34","TotWorkingPop","PopInWF","Pop18-64","PopUnisured","PopINP","PopIDP","PopINPr","PopIDPr", "PopDis")

# using the 2017 data to fill in as many blanks as possible
variables.model.muni$MedHosHolInc.17 <- variables.model.muni.17$MedHosHolInc
# we need to look to see if there is any mis match is the name vectors
variables.model.muni$Name.17 <- variables.model.muni.17$Name
variables.model.muni$match  <- variables.model.muni$Name == variables.model.muni$Name.17
# there is one place where they dont match but that is because a town became a city (skiatook) but because its still the same place we can move forward
for (i in nrow(variables.model.muni)){ if (variables.model.muni[i,5] == -666666666){ variables.model.muni[i,5] <- variables.model.muni[i,19]}}
variables.model.muni <- variables.model.muni[,- c(19,20,21)]

# using these variables to create the final variables for the model 
variables.model.muni$PercentPopNonWhite <- ((variables.model.muni$Totpop - variables.model.muni$PopWhite)/variables.model.muni$Totpop) * 100
variables.model.muni$PercentPov <- ( variables.model.muni$PopPov / variables.model.muni$Totpop ) * 100
variables.model.muni$"Percent20-34" <- ((variables.model.muni$`Pop20-24` + variables.model.muni$`Pop25-34`)/ variables.model.muni$Totpop ) * 100
variables.model.muni$LaborPart <- (variables.model.muni$PopInWF/variables.model.muni$TotWorkingPop) * 100
variables.model.muni$PercentPrivIns <- ((variables.model.muni$PopINPr + variables.model.muni$PopIDPr)/variables.model.muni$`Pop18-64`) * 100
variables.model.muni$PercentPubIns <- ((variables.model.muni$PopINP + variables.model.muni$PopIDP)/variables.model.muni$`Pop18-64`) * 100
variables.model.muni$PercentUnins <- ( variables.model.muni$PopUnisured / variables.model.muni$`Pop18-64` ) * 100

variables.model.muni$PrivateIns <- (variables.model.muni$PopINPr + variables.model.muni$PopIDPr)
variables.model.muni$PublicIns <- (variables.model.muni$PopIDP + variables.model.muni$PopINP)
variables.model.muni$"20-34" <- (variables.model.muni$`Pop20-24` + variables.model.muni$`Pop25-34`)
variables.model.muni$PopNonWhite <- variables.model.muni$Totpop - variables.model.muni$PopWhite

# replacing the disabled estimate 

variables.model.muni$PopDis <- variables.model.muni$PopIDP + variables.model.muni$PopIDPr


# selecting out the variabels for our two models

variables.model.muni <- variables.model.muni[,c(1,2,3,4,5,19,20,21,22,23)]

#creating variables for two more models we would like to run.
#1. a model where we include the square of population
#2. a model where we have frequancy per 100 people as the dependant and dont include pop
#we cant do two until everything is in the data frame 


# recovering spatial dataframe of Oklahoma municipalities and binding the information to them
library(maptools)
municipal_boundaries <- readShapeSpatial("C:/Users/14055/Downloads/munibnd/munibnd.shp")
# limiting to those with a state FIP that matchs Oklahomas
municipal_boundaries <- subset(municipal_boundaries, ! municipal_boundaries@data$ST_FIPS != 40)
# cleaing the dataset so that we only have one row for each city 
# first we remove some rows 
municipal_boundaries@data <- municipal_boundaries@data[,- c(5,7,8)]
municipal_boundaries@data <- base::unique(municipal_boundaries@data)
#plotting to see how things look
plot(municipal_boundaries) #Funky but also pretty cool
# changeing the city names to all uppercase in the sp.data.frame so they match the labels in the pdf
municipal_boundaries@data$CITYNAME <- toupper(municipal_boundaries@data$CITYNAME)
# merging the data on number of medical merijuana dispensaries into the dataframe
municipal_boundaries@data <- sp::merge(x = municipal_boundaries@data , y = counts.disp.cities , by.x = "CITYNAME" , by.y = "Var1" , all.x = T)
# inserting a value of zero for cities that did not have any dispensaries and so where missing from the counts datafame
municipal_boundaries@data$Freq[is.na(municipal_boundaries@data$Freq)] <- 0
# adding the variables we want to model into a dataframe with the frequencies 
municipal_boundaries@data <- sp::merge(x = municipal_boundaries@data , y = variables.model.muni , by.x = "FIPS" , by.y = "CountyFIP" , all.x = T)
# it appears that there are municipalities that are not also incorparated places this so we will have to limit the scope of our investigation to those municipalities
# that are also incoporated places
municipal_boundaries <- subset(municipal_boundaries, !is.na(StateFIP))
# We also need to examine how many places have had their median houshold income supressed. 
nrow(municipal_boundaries@data[municipal_boundaries@data$MedHosHolInc == -666666666 | is.na(municipal_boundaries@data$MedHosHolInc),])

# for now lets just use non-supressed data and hope that it is missing at random. 
municipal_boundaries <- subset(municipal_boundaries, ! MedHosHolInc == -666666666 & ! is.na(MedHosHolInc))

##################################################################################

#looking at the correlations between all the variables for both counties and municipalities
cor(municipal_boundaries@data[,9:15])

library(stargazer)
# creating a global model based on count to see how well it will fit the state of oklahoma 
attach(municipal_boundaries@data)
formula.muni <- Freq ~ Totpop + MedHosHolInc + PercentPopNonWhite + PercentPov + `Percent20-34` + PercentPrivIns + LaborPart
model.muni <- lm(formula = formula.muni, data = municipal_boundaries@data)
summary(model.muni)
detach(municipal_boundaries@data)


# It would appear that the model for municipalities is signifigant but the only variable of signifigance is population.

# lets check the linearity assumption
par(mfrow = c(2,2))
plot(model.muni)

# creating pairs chart of variables in model 1
par(mfrow = c(1,1))
library(corrplot)
corr.data <- cor(municipal_boundaries@data[,9:15])
corrplot(corr.data)


# Adding the residuals so that we can test for geographic stationarity 
municipal_boundaries@data$residuals <- model.muni$residuals

#lets look for multicolinearity amoungst the predictors
library(car)
vif(model.muni)
VIF1<-vif(model.muni)
# there does not appear to be multicolinearity

#lets see if there is heteroskedasticity in the model 
library(olsrr)
ols_test_breusch_pagan(model.muni)
# We reject the null hypothesis and so we do believe that their is heteroskedasticity so we calculate the robust variance
library(sandwich)
library(lmtest)
coeftest(model.muni, vcov = vcovHC(model.muni))
model.muni.rob <- coeftest(model.muni, vcov = vcovHC(model.muni))
# It would appear that total population stays signifigant but the intercept has lost its signifigance

# Lets examine whether or not there is spatial autocorrelation in the residuals that could call for a GWR
library(spdep)
# creating a neighbors object
city_names <- municipal_boundaries@data$CITYNAME
Oklahoma.nb <- dnearneigh(coordinates(municipal_boundaries), d1 = 0 , d2 = 90, longlat = TRUE, row.names = city_names)
# Step 2: turn the nb object in a listw object
Oklahoma.lw <- nb2listw(Oklahoma.nb)
# Step 3: calculate Moran's I on the residuals from the global model
moran.test(municipal_boundaries@data$residuals, Oklahoma.lw, alternative = "two.sided")
# It would seem it does not vary spatial (I don't know whether to be happy or sad)

# creating a clean output for the models
stargazer(model.muni, model.muni.rob , type = "latex", out = "model 1.txt" , title = "Figure 2")
stargazer(VIF1 , type = "latex", out = "model 1.txt" , title = "VIF for model 1")


#Going through the same validation steps as above except with the alterations that we wanted to make to the model. 

## adding the square of total population 

# creating a global model based on count to see how well it will fit the state of oklahoma 
attach(municipal_boundaries@data)
formula.muni <- Freq ~ Totpop + MedHosHolInc + PercentPopNonWhite + PercentPov + `Percent20-34` + PercentPrivIns + LaborPart + I(Totpop*Totpop)
model.muni <- lm(formula = formula.muni, data = municipal_boundaries@data)
summary(model.muni)
detach(municipal_boundaries@data)
# the intercept becomes insignifigant.


# It would appear that the model for municipalities is signifigant but the only variable of signifigance is population and its interaction with itself.

# checking linearity 
par(mfrow = c(2,2))
plot(model.muni)

# Adding the residuals so that we can test for geographic stationarity 
municipal_boundaries@data$residuals <- model.muni$residuals

#lets look for multicolinearity amoungst the predictors
library(car)
vif(model.muni)
VIF1<- vif(model.muni)
# there does not appear to be unexpected multicolinearity

#lets see if there is heteroskedasticity in the model 
library(olsrr)
ols_test_breusch_pagan(model.muni)
# We reject the null hypothesis and so we do believe that their is heteroskedasticity so we calculate the robust variance
library(sandwich)
library(lmtest)
coeftest(model.muni, vcov = vcovHC(model.muni))
model.muni.rob <- coeftest(model.muni, vcov = vcovHC(model.muni))
# It would appear that total population stays signifigant but the intercept has lost its signifigance and so has the interaction term. percent in poverty has picked up some signifigance 

# Lets examine whether or not there is spatial autocorrelation in the residuals that could call for a GWR
library(spdep)
# creating a neighbors object
city_names <- municipal_boundaries@data$CITYNAME
Oklahoma.nb <- dnearneigh(coordinates(municipal_boundaries), d1 = 0 , d2 = 90, longlat = TRUE, row.names = city_names)
# Step 2: turn the nb object in a listw object
Oklahoma.lw <- nb2listw(Oklahoma.nb)
# Step 3: calculate Moran's I on the residuals from the global model
moran.test(municipal_boundaries@data$residuals, Oklahoma.lw, alternative = "two.sided")
# It would seem it does not vary spatial

# creating a clean output for the models
stargazer(model.muni, model.muni.rob, type = "latex", out = "model 2.tex" , title = "Figure 3.")
stargazer(VIF1 , type = "latex", out = "model 1.txt" , title = "VIF for model 1")

## changing it to dispensaries per capota and dropping tot pop

# creating a global model based on count to see how well it will fit the state of oklahoma 
attach(municipal_boundaries@data)
formula.muni <- Freq/(Totpop/100) ~ MedHosHolInc + PercentPopNonWhite + PercentPov + `Percent20-34` + PercentPrivIns + LaborPart
model.muni <- lm(formula = formula.muni, data = municipal_boundaries@data)
summary(model.muni)
detach(municipal_boundaries@data)
# the intercept becomes signifigant, percent poverty becomes signifigant, percent private insurance bacomes signifigant.

#checking linearity
par(mfrow = c(2,2))
plot(model.muni)

# Adding the residuals so that we can test for geographic stationarity 
municipal_boundaries@data$residuals <- model.muni$residuals

#lets look for multicolinearity amoungst the predictors
library(car)
vif(model.muni)
VIF1 <- vif(model.muni)
# there does not appear to be multicolinearity

#lets see if there is heteroskedasticity in the model 
library(olsrr)
ols_test_breusch_pagan(model.muni)
# We reject the null hypothesis and so we do believe that their is heteroskedasticity so we calculate the robust variance
library(sandwich)
library(lmtest)
coeftest(model.muni, vcov = vcovHC(model.muni))
model.muni.rob <- coeftest(model.muni, vcov = vcovHC(model.muni))
# It would appear that total population stays signifigant but the intercept has lost its signifigance and so has the interaction term. percent in poverty has picked up some signifigance 

# Lets examine whether or not there is spatial autocorrelation in the residuals that could call for a GWR
library(spdep)
# creating a neighbors object
city_names <- municipal_boundaries@data$CITYNAME
Oklahoma.nb <- dnearneigh(coordinates(municipal_boundaries), d1 = 0 , d2 = 90, longlat = TRUE, row.names = city_names)
# Step 2: turn the nb object in a listw object
Oklahoma.lw <- nb2listw(Oklahoma.nb)
# Step 3: calculate Moran's I on the residuals from the global model
moran.test(municipal_boundaries@data$residuals, Oklahoma.lw, alternative = "two.sided")
# It would seem it does not vary spatial 

# creating a clean output for the models
stargazer(model.muni,model.muni.rob, type = "latex", out = "model 3.html" , title = "Figure 4.")
stargazer(VIF1 , type = "latex", out = "model 1.txt" , title = "VIF for model 1")
#obtaining the citations for the packages used/ 

libvec <- c("dplyr","tabulizer","censusapi","maptools","stargazer","corrplot","car","olsrr","sandwich","lmtest","spdep")
citation(libvec[11])

