#1 
install.packages("mice")

#2
library(mice)

#3
View(mammalsleep)

#4
#Write the mammalsleep dataset from package mice to the work directory as a tab-delimited text file with . as a decimal seperator. Name the file mammalsleep.txt
write.table(mammalsleep, "mammalsleep.txt", sep = "\t", dec = ".", row.names = FALSE)

#5 import the previously created txt file
sleepdata <- read.table("mammalsleep.txt", sep = "\t", dec = ".", header = TRUE)

getwd() #retrieve in which directory you are working
setwd() #set new directory to work in

#6 inspecting the data
summary(sleepdata)
str(sleepdata)

#7 save the current workspace and sleepdata file
save.image("Practical_C.RData") #save workspace
save(sleepdata,file = "Sleepdata.RData") #allows you to save any object in the workspace

#8 Exclude the following animals from the sleepdata dataset: Echidna, Lesser short-tailed shrew and Musk shrew. Save the dataset as sleepdata2.
exclude <- c("Echidna", "Lesser short-tailed shrew", "Musk shrew")
which <- sleepdata$species %in% exclude #Indicate the species that match the names in exclude. 
sleepdata2 <- sleepdata[!which,]

#9 plot brain weight (brw) as a function of species
plot(brw ~ species, data = sleepdata2) # option 1: plotting formula
plot(sleepdata2$species, sleepdata2$brw) # option 2: plotting directly from datatset
with(sleepdata2, plot(species, brw)) # option 3: with this data, plot this

#10 
# step 1: find out names of animals with brw > 1SD of the mean
sd.brw <- sd(sleepdata2$brw)
mean.brw <- mean(sleepdata2$brw)
which <- sleepdata2$brw > (mean.brw + (1 * sd.brw)) #indicate which brains are too large
as.character(sleepdata2$species[which])

plot(brw ~ species, data = sleepdata2[which, ])

sleepdata2$species[which]

#get rid of unsed factor levels
sleepdata3 <- sleepdata2[which, ]
sleepdata3$species <- factor(sleepdata3$species)
sleepdata3$species

plot(brw ~ species, data = sleepdata3)

#11

plot(log(brw) ~ sws, data = sleepdata2, type = "p", pch = 2, col = "dark red",
     xlab = "Short-wave sleep",
     ylab = "Brain weight",
     main = "How sleep type relates to brain weight")


