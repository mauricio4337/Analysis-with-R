install.packages("tidyverse")
install.packages("jsonlite")
#Read in data from csv file into MechaCar table
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#generate multiple linear regression model
x1 <-- MechaCar_table$"vehicle length"
x2 <-- MechaCar_table$"vehicle weight"
x3 <-- MechaCar_table$"spoiler angle"
x4 <-- MechaCar_table$"ground clearance"
x5 <-- MechaCar_table$"AWD"
y <-- MechaCar_table$"mpg"

lm(y ~ x1 + x2 + x3 + x4 + x5)

# generate a summary of the model
summary(lm(y ~ x1 + x2 + x3 + x4 + x5))

# Load data from Suspension_Coil.csv into a table
Suspension_Coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#Get a summary of the PSI values
summary(Suspension_Coil_table$PSI)

#Find the mean PSI and standard deviation by Lot  
summarize_by_Lot <- Suspension_Coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Lot_PSI=mean(PSI),SDEV= sd(PSI))

plt <- ggplot(Suspension_Coil_table,aes(x=Manufacturing_Lot,y=PSI)) #import dataset into ggplot2
plt + geom_boxplot()

plt <- ggplot(Suspension_Coil_table,aes(y=PSI)) #import dataset into ggplot2
plt + geom_boxplot()

