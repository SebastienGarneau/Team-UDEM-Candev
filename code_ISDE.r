library(plotrix)
library(ggplot2)

# Read Subset 3 taken from 
# https://open.canada.ca/data/en/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/63f972a8-8bf2-4584-a735-e5d0c07a9eb6

data <- read.csv("C:\\Users\\yanwa\\Desktop\\CANDEV\\subset-3-sous-ensemble-3.csv")


#######################################################################################
################################## CLEANING THE DATA ##################################
#######################################################################################

# Remove the french columns from the data
data1 <- subset(data, select = -c(9,13,29,32,35))

# Select the ISDE department only in the data
data.ISDE <- data1[which(data1$DEPT_E == "Innovation, Science and Economic Development Canada"),]

# Remove the columns 1 to 5, since they have the same number
data.ISDE.1 <- data.ISDE[,-c(1:5)]

# Remove the NA in the whole dataset
data.ISDE.1 <- na.omit(data.ISDE.1) 

# Verify if there are any NA in data
sum(is.na(data.ISDE.1)) # 0


#######################################################################################
############################ DESCRIPTIVE STATISTICS (2020) ############################
#######################################################################################

### Only use the data for the 2020 year
data.ISDE.2020 <- data.ISDE.1[which(data.ISDE.1$SURVEYR==2020),]

### Function that graphs a pie chart for the descriptive statistics with a given vector
### (nmbr.vec) of the the average ANSCOUNT for each sub-group, a given vector of labels
### containing the sub-group names, the actual group name (group.name) and the subtitle
### for the ggplot subtitle

fn.graph.pie <- function(nmbr.vec, labels, group.name, subtitle){
  group.sum <- sum(nmbr.vec)
  percentages <- nmbr.vec/group.sum
  
  # Creation of dataframe (df) for pie chart in ggplot
  df = data.frame(subject = rep(group.name, length(percentages)),
                  Levels = labels,
                  value = round(percentages*100,1))
  df$subject <- factor(df$subject)
  df$Levels <- factor(df$Levels)
  
  # ggplot to plot a pie chart
  ggplot(data=df, aes(x="", y=value, fill=Levels)) +
    geom_bar(width = 1, stat = "identity", color = "black") +
    coord_polar("y") +
    theme_void() + ggtitle(group.name, subtitle = subtitle) +
    geom_text(aes(label = value),
              position = position_stack(vjust = 0.5), show.legend = FALSE) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

#######################################################################################
### COUNT THE NUMBER OF MALES AND FEMALES ON AVERAGE

nmbr.male <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Male gender"),]$ANSCOUNT))
nmbr.female <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Female gender"),]$ANSCOUNT))
nmbr.gen.div <- 0

# Fill a vector with approximate count of each sub-group
nmbr.vec.gen <- c(nmbr.male, nmbr.female, nmbr.gen.div)

# Specify labels for each sub-group
labels.gen=c("Male gender", "Female gender", "Gender diverse")

# Print the pie chart
fn.graph.pie(nmbr.vec=nmbr.vec.gen, labels=labels.gen, group.name="Separation among gender",
           subtitle = "")

#######################################################################################
### COUNT THE NUMBER OF INDIGENOUS PEOPLE IN EACH SUB-GROUP

nmbr.non.ind <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Non-Indigenous"),]$ANSCOUNT))
nmbr.first.nation <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="First Nation (North American Indian)"),]$ANSCOUNT))
nmbr.metis <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Métis"),]$ANSCOUNT))
nmbr.innuk <- 0

# Fill a vector with approximate count of each sub-group
nmbr.vec.ind <- c(nmbr.first.nation, nmbr.metis, nmbr.innuk)

# Specify labels for each sub-group
labels.ind=c("First nations", "Métis", "Innuk")

# Percentage of indigenous among all people (approximation)
ind.perc <- round(sum(nmbr.vec.ind)*100/(nmbr.non.ind + sum(nmbr.vec.ind)),1)

# Print the pie chart
fn.graph.pie(nmbr.vec=nmbr.vec.ind, labels=labels.ind, group.name="Separation among indigenous people",
           subtitle = paste("Indigenous people reprensent :", ind.perc,"% of all ISED workers"))

#######################################################################################
### COUNT THE NUMBER OF MINORITIES IN EACH SUB-GROUP

nmbr.non.min <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Non-visible minority" ),]$ANSCOUNT))
nmbr.black <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Black"),]$ANSCOUNT))
nmbr.chin <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Chinese" ),]$ANSCOUNT))
nmbr.fili <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Filipino"),]$ANSCOUNT))
nmbr.south.asian <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="South Asian/East Indian (including: Indian from India; Bangladeshi; Pakistani; East Indian from Guyana, Trinidad, East Africa; etc.)" ),]$ANSCOUNT))
nmbr.southeast.asian <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Southeast Asian (including: Burmese; Cambodian; Laotian; Thai; Vietnamese; etc.)" ),]$ANSCOUNT))
nmbr.non.white.west.asian <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Non-White West Asian, North African or Arab (including: Egyptian; Libyan; Lebanese; Iranian; etc.)" ),]$ANSCOUNT))
nmbr.non.white.latin.ame <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Non-White Latin American (including: Indigenous persons from Central and South America, etc.)"),]$ANSCOUNT))  
nmbr.mixed.min <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Person of mixed origin (with one parent in one of the visible minority groups)"),]$ANSCOUNT))
nmbr.other.min <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Other visible minority group"),]$ANSCOUNT))

# Fill a vector with approximate count of each sub-group
nmbr.vec.min <- c(nmbr.black, nmbr.chin, nmbr.fili, nmbr.south.asian, nmbr.southeast.asian, nmbr.non.white.west.asian, nmbr.non.white.latin.ame, nmbr.mixed.min, nmbr.other.min)

# Specify labels for each sub-group
labels.min=c("Black", "Chinese", "Filipino", "South Asian/East Indian", "Southeast Asian", "Non-White West Asian, North African \n or Arab", "Non-White Latin American", "Person of mixed origin", "Other visible minority group")

# Percentage of minorities among all people (approximation)
min.perc <- round(sum(nmbr.vec.min)*100/(nmbr.non.min + sum(nmbr.vec.min)),1)

# Print the pie chart
fn.graph.pie(nmbr.vec=nmbr.vec.min, labels=labels.min, group.name="Separation among minorities",
           subtitle = paste("Minorities represent :", min.perc,"% of all ISED workers"))

#######################################################################################
### COUNT THE NUMBER OF DISABILITIES IN EACH SUB-GROUP

nmbr.non.dis <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Not a person with a disability"),]$ANSCOUNT))
nmbr.vision <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A seeing disability affects vision, including total blindness, partial sight and visual distortion"),]$ANSCOUNT))
nmbr.hearing <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A hearing disability affects ability to hear, including being hard of hearing, deafness or acoustic distortion"  ),]$ANSCOUNT))
nmbr.mobility <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A mobility issue affects ability to move your body, including the required use of a wheelchair or a cane, or other issues impacting your mobility" ),]$ANSCOUNT))
nmbr.dexterity <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="An issue with flexibility or dexterity affects ability to move joints or perform motor tasks, especially with your hands"),]$ANSCOUNT))
nmbr.mental <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A mental health issue affects psychology or behaviour, such as anxiety, depression or social / compulsive disorder or phobia or psychiatric illness" ),]$ANSCOUNT))
nmbr.sensory <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A sensory / environmental disability affects sensitivity to light, sounds or other distractions, as well as allergens and other environmental sensitivities"  ),]$ANSCOUNT))
nmbr.chron <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A chronic health condition or pain affects ability to function on a regular or episodic basis due to migraines, Crohn's disease, colitis, and other disabilities or health conditions"),]$ANSCOUNT))
nmbr.cogn <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="A cognitive disability affects ability to carry out tasks involving executive functioning, such as planning and organization, learning information, communication and memory, including autism or Asperger's syndrome, attention deficit disorder, and learning disabilities"),]$ANSCOUNT))
nmbr.other <- ceiling(mean(data.ISDE.2020[which(data.ISDE.2020$DESCRIP_E=="Other disability (including: learning disabilities, developmental disabilities and all other types of disabilities)"  ),]$ANSCOUNT))

# Fill a vector with approximate count of each sub-group
nmbr.vec.dis <- c(nmbr.vision, nmbr.hearing, nmbr.mobility, nmbr.dexterity, nmbr.mental,nmbr.sensory, nmbr.chron, nmbr.cogn, nmbr.other)

# Specify labels for each sub-group
labels.dis=c("Vision", "Hearing", "Mobility issue", "Flexibility or dexterity", "Mental health", "Sensory/Environmental", "Chronic health condition", "Cognitive disability", "Other disability")

# Percentage of disabilities among all people (approximation)
dis.perc <- round( sum(nmbr.vec.dis)*100/(nmbr.non.dis + sum(nmbr.vec.dis)),1)

# Print the pie chart
fn.graph.pie(nmbr.vec=nmbr.vec.dis, labels=labels.dis, group.name="Separation among people with disabilities",
           subtitle = paste("People with disabilities represent :", dis.perc, "% of all ISED workers"))


#######################################################################################
########################## DESCRIPTIVE STATISTICS Q53 (2020) ##########################
#######################################################################################


### Function that creates a data.frame for the ggplot based on the 3 answers given
### on question 53 of the survey (Yes, No. Not Sure). Returns the data.frame to be put 
### ggplot object to better understand difference between groups

fn.df.Q53 <- function(data){
  slices <- c(data$ANSWER1,data$ANSWER2,data$ANSWER3)
  lbls <- c("Yes", "No", "Not sure")
  df = data.frame(subject = rep(data$DESCRIP_E, 3), Answers = lbls, value = slices)
  df$subject <- factor(df$subject)
  df$Answers <- factor(df$Answers, levels = df$Answers)
  return(df)
}

# Creating data.frame with only question 53 of the survey 
data.ISDE.q53 <- data.ISDE.2020[which(data.ISDE.2020$QUESTION=="Q53"),]

df.Q53.female <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Female gender"),])
df.Q53.ind <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Indigenous"),])
df.Q53.dis <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Person with a disability"),])
df.Q53.min <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Visible minority"),])

df.Q53.male<- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Male gender"),])
df.Q53.non.ind <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Non-Indigenous" ),])
df.Q53.non.dis <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Not a person with a disability"  ),])
df.Q53.non.min <- fn.df.Q53(data.ISDE.q53[which(data.ISDE.q53$DESCRIP_E=="Non-visible minority" ),])

df.Q53.total <- rbind(df.Q53.female, df.Q53.ind, df.Q53.dis, df.Q53.min, 
                   df.Q53.male, df.Q53.non.ind, df.Q53.non.dis, df.Q53.non.min)

# Creating the plot 
ggplot(data=df.Q53.total, aes(x=" ", y=value, group=Answers, colour=Answers, fill=Answers)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start=1) + 
  facet_grid(.~ subject) + 
  theme_void() + 
  facet_wrap( ~ subject,ncol = 4, nrow = 2) + 
  ggtitle("Do you intend to leave your current position in the next two years?") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 6), legend.position="bottom")


#######################################################################################
########################## DESCRIPTIVE STATISTICS Q54 (2020) ##########################
#######################################################################################

### Function that creates a data.frame for the ggplot based on the 6 answers given
### on question 54 of the survey. Returns the data.frame to be put ggplot object to 
### better understand why ine each group people want to quit there job

fn.df.Q54 <- function(data){
  slices <- c(data$ANSWER1, data$ANSWER2, data$ANSWER3, data$ANSWER4, data$ANSWER5, data$ANSWER6)
  lbls <- c("To retire", 
            "To pursue another position", 
            "To pursue a position in another \n department or agency", 
            "To pursue a position outside \n the federal public service", 
            "End of my term, casual or \n student employment",
            "Other")
  df = data.frame(subject = rep(data$DESCRIP_E, 6), Answers = lbls, value = slices)
  df$subject <- factor(df$subject)
  if(df$subject[1] == "Person with a disability") df$subject <- rep("Person with a\n disability")
  df$Answers <- factor(df$Answers, levels = df$Answers)
  return(df)
}

# Creating data.frame with only question 54 of the survey 
data.ISDE.q54 <- data.ISDE.2020[which(data.ISDE.2020$QUESTION=="Q54"),]

df.Q54.female <- fn.df.Q54(data.ISDE.q54[which(data.ISDE.q54$DESCRIP_E=="Female gender"),])
df.Q54.ind <- fn.df.Q54(data.ISDE.q54[which(data.ISDE.q54$DESCRIP_E=="Indigenous"),])
df.Q54.dis <- fn.df.Q54(data.ISDE.q54[which(data.ISDE.q54$DESCRIP_E=="Person with a disability"),])
df.Q54.min <- fn.df.Q54(data.ISDE.q54[which(data.ISDE.q54$DESCRIP_E=="Visible minority"),])

df.total <- rbind(df.Q54.female, df.Q54.ind, df.Q54.dis, df.Q54.min)

# Creating the plot 
ggplot(data=df.total, aes(x=" ", y=value, group=Answers, colour=Answers, fill=Answers)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  facet_grid(.~ subject) + theme_void()+
  theme(legend.position="bottom")


#######################################################################################
########################## STATISTICAL ANALYSIS (2018-2020) ###########################
#######################################################################################

### Function that will calculate difference of propotion between two demcodes, The function
### calculates the weighted average for each factor using the COUNT variable that is created
### by multiplying the ANSCOUNT and AGREE column. This gives the COUNT and with ANSCOUNT, it
### is possible to do a prop.test and retain the confidence intervals and p.value. This is 
### done for each factor and each demcode.

prop.analysis <- function(data, demcode1, demcode2){
  
  par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
  l <- length(unique(data$SUBINDICATORID))
  x1=c(); x2=c(); n1=c(); n2=c(); pvalue=c()
  interval=matrix(ncol=2,nrow = l)
  prop.diff <- c() ;name=c()
  k <- 1
  
  for (i in sort(unique(data$SUBINDICATORID))){
    
    nligne1 = which(data$SUBINDICATORID==i & data$DEMCODE==demcode1)
    nligne2 = which(data$SUBINDICATORID==i & data$DEMCODE==demcode2)
    x1 = c(x1, sum(data$COUNT[nligne1]))
    x2 = c(x2,sum(data$COUNT[nligne2]))
    n1 = c(n1,sum(data$ANSCOUNT[nligne1]))
    n2 = c(n2,sum(data$ANSCOUNT[nligne2]))
    
    analyse=prop.test(x=c(x1[k],x2[k]),n=c(n1[k],n2[k]),correct=FALSE)
    
    interval[k,] = analyse$conf.int[1:2]
    name[k] <- data$SUBINDICATORENG[nligne1[1]]
    pvalue[k] = analyse$p.value
    k <- k + 1
  }
  
  table=cbind(name,round(interval,4),round(pvalue,4))
  colnames(table)<-c("Factor","Inf","Sup","pvalue")
  
  return(table)
}

# This section will serve to calculate the weighted average of the questions for each 
# factor using the AGREE columns. Once the weighted average is calculated fore each group
# we can do a test of proportion, prop.test().

# REFERENCE TO DEMCODE in following code
# DEMCODE==2011 : Male gender
# DEMCODE==2012 : Female gender
# DEMCODE==2014 : Indigenous
# DEMCODE==2015 : Non-indigenous
# DEMCODE==2019 : Person with a disability
# DEMCODE==2020 : Not a person with a disability
# DEMCODE==2041 : Visible minority
# DEMCODE==2042 : Non-visible minority

### Remove questions 54, 56-59, 63-66 from our statistical analysis

q54 <- which(data.ISDE.1$QUESTION == "Q54")
q56a <- min(which(data.ISDE.1$QUESTION == "Q56a"))
q59p <- max(which(data.ISDE.1$QUESTION == "Q59p"))
q63a <- min(which(data.ISDE.1$QUESTION == "Q63a"))
q66p <- max(which(data.ISDE.1$QUESTION == "Q66p"))

data.ISDE.1 <- data.ISDE.1[-c(q54, q56a:q59p, q63a:q66p),]

### Get the names of all the factors in order
data.ISDE.1 <- data.ISDE.1[order(data.ISDE.1$SUBINDICATORID),]

### This vector will contain all the factor names (1 to 23)
name.factor <- unique(data.ISDE.2$SUBINDICATORENG)

####################################################################
### DATA CREATION TO SEPERATE GROUPS IN DIFFERENT data.frame. For 
### example, data.ISDE.sex will only have Men, women and dender
### diverse for 2018, 2019, 2020

### Data using only DEMCODE 2011 to 2013 corresponding to Gender (Male, Female, Gender diverse)

data.ISDE.sex <-  data.ISDE.1[which(data.ISDE.1$DEMCODE == 2011 | data.ISDE.1$DEMCODE == 2012 | data.ISDE.1$DEMCODE == 2013),]
### Data using only DEMCODE 2014 and 2015 corresponding to Indigenous people

data.ISDE.indigenous <- data.ISDE.1[which(data.ISDE.1$DEMCODE == 2014 | data.ISDE.1$DEMCODE == 2015),]

### Data using only DEMCODE 2019 and 2020 corresponding to people with disabilities
data.ISDE.disability <- data.ISDE.1[which(data.ISDE.1$DEMCODE == 2019 | data.ISDE.1$DEMCODE == 2020),]

### Data using only DEMCODE 2041 and 2042 corresponding to visible minorities
data.ISDE.race <- data.ISDE.1[which(data.ISDE.1$DEMCODE == 2041 | data.ISDE.1$DEMCODE == 2042),]


### By year and sex gender
data.ISDE.sex.2020 <- data.ISDE.sex[which(data.ISDE.sex$SURVEYR == 2020),]
data.ISDE.sex.2019 <- data.ISDE.sex[which(data.ISDE.sex$SURVEYR == 2019),]
data.ISDE.sex.2018 <- data.ISDE.sex[which(data.ISDE.sex$SURVEYR == 2018),]

### By year and race
data.ISDE.race.2020 <- data.ISDE.race[which(data.ISDE.race$SURVEYR == 2020),]
data.ISDE.race.2019 <- data.ISDE.race[which(data.ISDE.race$SURVEYR == 2019),]
data.ISDE.race.2018 <- data.ISDE.race[which(data.ISDE.race$SURVEYR == 2018),]

### By year and indigenous
data.ISDE.indigenous.2020 <- data.ISDE.indigenous[which(data.ISDE.indigenous$SURVEYR == 2020),]
data.ISDE.indigenous.2019 <- data.ISDE.indigenous[which(data.ISDE.indigenous$SURVEYR == 2019),]
data.ISDE.indigenous.2018 <- data.ISDE.indigenous[which(data.ISDE.indigenous$SURVEYR == 2018),]

### By year and disability
data.ISDE.disability.2020 <- data.ISDE.disability[which(data.ISDE.disability$SURVEYR == 2020),]
data.ISDE.disability.2019 <- data.ISDE.disability[which(data.ISDE.disability$SURVEYR == 2019),]
data.ISDE.disability.2018 <- data.ISDE.disability[which(data.ISDE.disability$SURVEYR == 2018),]


### Creating a new variable count for each group and year to be able to have a X.
data.ISDE.sex.2020$COUNT <- round(data.ISDE.sex.2020$ANSCOUNT*data.ISDE.sex.2020$AGREE/100)
data.ISDE.race.2020$COUNT <- round(data.ISDE.race.2020$ANSCOUNT*data.ISDE.race.2020$AGREE/100)
data.ISDE.disability.2020$COUNT <- round(data.ISDE.disability.2020$ANSCOUNT*data.ISDE.disability.2020$AGREE/100)
data.ISDE.indigenous.2020$COUNT <- round(data.ISDE.indigenous.2020$ANSCOUNT*data.ISDE.indigenous.2020$AGREE/100)

data.ISDE.sex.2019$COUNT <- round(data.ISDE.sex.2019$ANSCOUNT*data.ISDE.sex.2019$AGREE/100)
data.ISDE.race.2019$COUNT <- round(data.ISDE.race.2019$ANSCOUNT*data.ISDE.race.2019$AGREE/100)
data.ISDE.disability.2019$COUNT <- round(data.ISDE.disability.2019$ANSCOUNT*data.ISDE.disability.2019$AGREE/100)
data.ISDE.indigenous.2019$COUNT <- round(data.ISDE.indigenous.2019$ANSCOUNT*data.ISDE.indigenous.2019$AGREE/100)

data.ISDE.sex.2018$COUNT <- round(data.ISDE.sex.2018$ANSCOUNT*data.ISDE.sex.2018$AGREE/100)
data.ISDE.race.2018$COUNT <- round(data.ISDE.race.2018$ANSCOUNT*data.ISDE.race.2018$AGREE/100)
data.ISDE.disability.2018$COUNT <- round(data.ISDE.disability.2018$ANSCOUNT*data.ISDE.disability.2018$AGREE/100)
data.ISDE.indigenous.2018$COUNT <- round(data.ISDE.indigenous.2018$ANSCOUNT*data.ISDE.indigenous.2018$AGREE/100)



####################################################################
### Analysis

# 2018
table.sex.2018 <- prop.analysis(data.ISDE.sex.2018, 2011, 2012)
table.race.2018 <- prop.analysis(data.ISDE.race.2018, 2041, 2042)
table.dis.2018 <- prop.analysis(data.ISDE.disability.2018, 2019, 2020)
table.ind.2018 <- prop.analysis(data.ISDE.indigenous.2018, 2014, 2015)

# 2019
table.sex.2019 <- prop.analysis(data.ISDE.sex.2019, 2011, 2012)
table.race.2019 <- prop.analysis(data.ISDE.race.2019, 2041, 2042)
table.dis.2019 <- prop.analysis(data.ISDE.disability.2019, 2019, 2020)
table.ind.2019 <- prop.analysis(data.ISDE.indigenous.2019, 2014, 2015)

# 2020
table.sex.2020 <- prop.analysis(data.ISDE.sex.2020, 2011, 2012)
table.race.2020 <- prop.analysis(data.ISDE.race.2020, 2041, 2042)
table.dis.2020 <- prop.analysis(data.ISDE.disability.2020, 2019, 2020)
table.ind.2020 <- prop.analysis(data.ISDE.indigenous.2020, 2014, 2015)


# Function that plots a time graph from 2018 to 2020 for the most significant
# parameters

graph <- function(alpha, title){
  
  Signif.temps.sexe <- rep(0,3)
  Signif.temps.race <- rep(0,3)
  Signif.temps.ind <- rep(0,3)
  Signif.temps.dis <- rep(0,3)
  
  Signif.temps.sexe[1] <- length(table.sex.2018[which(table.sex.2018[,4] < alpha),1])
  Signif.temps.sexe[2] <- length(table.sex.2019[which(table.sex.2019[,4] < alpha),1])
  Signif.temps.sexe[3] <- length(table.sex.2020[which(table.sex.2020[,4] < alpha),1])
  
  Signif.temps.race[1] <- length(table.race.2018[which(table.race.2018[,4] < alpha),1])
  Signif.temps.race[2] <- length(table.race.2019[which(table.race.2019[,4] < alpha),1])
  Signif.temps.race[3] <- length(table.race.2020[which(table.race.2020[,4] < alpha),1])
  
  Signif.temps.ind[1] <- length(table.ind.2018[which(table.ind.2018[,4] < alpha),1])
  Signif.temps.ind[2] <- length(table.ind.2019[which(table.ind.2019[,4] < alpha),1])
  Signif.temps.ind[3] <- length(table.ind.2020[which(table.ind.2020[,4] < alpha),1])
  
  Signif.temps.dis[1] <- length(table.dis.2018[which(table.dis.2018[,4] < alpha),1])
  Signif.temps.dis[2] <- length(table.dis.2019[which(table.dis.2019[,4] < alpha),1])
  Signif.temps.dis[3] <- length(table.dis.2020[which(table.dis.2020[,4] < alpha),1])
  
  plot(1:3,Signif.temps.sexe, ylim = c(0,18),type = "b", 
       main = title,
       xlab = "Year", xaxt = "n",
       ylab = " Significant parameters", pch = 16, font.main = 1)
  axis(1, at = 1:3, labels = c("2018", "2019", "2020"))
  points(1:3,Signif.temps.race, col = 2, type = "b", pch =16, lwd = 2)
  points(1:3,Signif.temps.ind, col = 3,type = "b", pch = 16, lwd = 2)
  points(1:3,Signif.temps.dis, col = 4,type = "b", pch = 16, lwd = 2)
}

# Resets graphs 
dev.off() # optional

par(oma = c(6,3,3,1), mfrow = c(1,3), mar = c(2, 2, 2, 1), cex.axis = 1, cex.lab = 1, cex.sub = 1)
graph(alpha = 0.05, title = expression(paste(alpha, " = 0.05")))
graph(alpha = 0.01, title = expression(paste(alpha, " = 0.01")))
graph(alpha = 0.001, title = expression(paste(alpha, " = 0.001")))

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 0, 0, 0), new = TRUE)

legend('bottom',legend = c("Gender", "Race", "Indigenous", "Disabilities"), 
       col = 1:4, 
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1)
mtext("Significant factors for different years", line = -2, outer = TRUE)
mtext("Significant factors", side = 2, line = -2, cex = 0.8)


#######################################################################################
############################ STATISTICAL ANALYSIS (2020) ##############################
#######################################################################################

### Function that implements a graph (using the plotrix package and library) to illustrate
### all difference of proportion confidence intervals for all factors by specyfing a title
### and a table

graph.CI <- function(table, title){
  col <- c()
  table <- table[order(table[,1], decreasing = T),]
  means <- (as.numeric(table[,3])+as.numeric(table[,2]))/2
  maximum.IC <- which.max(abs(means))
  for(k in 1:23){
    if(0 < as.numeric(table[k,3]) & 0 > as.numeric(table[k,2])){
      col<- c(col, "black")
    }else{col<- c(col, "blue")}
  }
  par(mar=c(5,22,2,1),las=2)
  plotCI(x=means, y=1:23, 
         li = as.numeric(table[,2]), 
         ui=as.numeric(table[,3]), 
         err = "x", col = col,ylab="", yaxt="n",
         xlab="Estimate of confidence interval for the difference in proportion",
         main = title)
  axis(side=2,at=1:23, label=table[,1])
  abline(v=0, col = 2)
}

# Construction of plots
graph.CI(table = table.dis.2020, title = "Person with disabilities vs Not a person with disabilities")
graph.CI(table = table.ind.2020, title = "Indigenous people vs Non-indigenous people")
graph.CI(table = table.sex.2020, title = "Men vs Women")
graph.CI(table = table.race.2020, title = "Minority vs Non-minority")




#######################################################################################
######################### STATISTICAL ANALYSIS (SUB-GROUPS) ###########################
#######################################################################################


## Sub levels for groups

### Data using only DEMCODE 2015, 2016, 2017 corresponding to Non-Indigenous 
#First Nation (North American Indian),Métis

data.ISDE.sub.indigenous <- data.ISDE.1[data.ISDE.1$DEMCODE %in% 2015:2017,]

### Data using only DEMCODE 2020(non disability),
#2021, 2023, 2025, 2027, 2029, 2031, 2033, 2035, 2037, 2039 corresponding to disabilities subgroups

data.ISDE.sub.disability <- data.ISDE.1[data.ISDE.1$DEMCODE %in% c(2020,seq(2021,2035,by=2),2039), ]

### Data using only DEMCODE 2042(non visible minority),
# 2043 2045 2047 2053 2055 2057 2059 2061 2063 corresponding to minoritiy subgroups

data.ISDE.sub.race <- data.ISDE.1[data.ISDE.1$DEMCODE %in% c(2042,seq(2043,2047,2),seq(2053,2063,2)), ]


####Subgroup of indigenous by year 2020
#2018 will not be included in analysis because subgroups dont exist
data.ISDE.sub.indigenous.2020 <- data.ISDE.sub.indigenous[which(data.ISDE.sub.indigenous$SURVEYR == 2020),]


####Subgroup of disability by year 2020

data.ISDE.sub.disability.2020 <- data.ISDE.sub.disability[which(data.ISDE.sub.disability$SURVEYR == 2020),]

####Subgroup of race by year 2020

data.ISDE.sub.race.2020 <- data.ISDE.sub.race[which(data.ISDE.sub.race$SURVEYR == 2020),]


#Fisher exact test is used because some proportions are equal to 0, and z-test is not applicable in this case
prop.analysis2 <- function(data, demcode1, demcode2){
  
  data$COUNT<- round(data$ANSCOUNT*data$AGREE/100)
  
  #par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
  l <- length(unique(data$SUBINDICATORID))
  x1=c(); x2=c(); n1=c(); n2=c(); pvalue=c()
  interval=matrix(ncol=2,nrow = l)
  prop.diff <- c() ;name=c();group<-c()
  k <- 1
  
  for (i in sort(unique(data$SUBINDICATORID))){
    
    nligne1 = which(data$SUBINDICATORID==i & data$DEMCODE==demcode1)
    nligne2 = which(data$SUBINDICATORID==i & data$DEMCODE==demcode2)
    x1 = c(x1, sum(data$COUNT[nligne1]))
    x2 = c(x2,sum(data$COUNT[nligne2]))
    n1 = c(n1,sum(data$ANSCOUNT[nligne1])-sum(data$COUNT[nligne1]))
    n2 = c(n2,sum(data$ANSCOUNT[nligne2])-sum(data$COUNT[nligne2]))
    contingence<-data.frame(rbind(c(x1[k],x2[k]),c(n1[k],n2[k])))
    analyse=fisher.test(contingence)
    
    interval[k,] = analyse$conf.int[1:2]
    name[k] <- data$SUBINDICATORENG[nligne1[1]]
    pvalue[k] = analyse$p.value
    group[k]<-data[data$DEMCODE==demcode1,"DESCRIP_E"]
    k <- k + 1
  }
  
  table=cbind(name,round(interval,4),round(pvalue,4),group)
  colnames(table)<-c("Factor","Inf","Sup","pvalue","Subgroup")
  
  return(table)
}

#Visualize multiple confidence interval of odd ratios
graph.CI2 <- function(table){
  col <- c()
  table<-table[order(table[,1],decreasing = TRUE),]
  means <- (as.numeric(table[,3])+as.numeric(table[,2]))/2
  for(k in 1:dim(table)[1]){
    if(1 < as.numeric(table[k,3]) & 1 > as.numeric(table[k,2])){
      col<- c(col, "black")
    }
    else{col<- c(col, "blue")}
  }
  par(mar=c(5,20,2,1),las=1)
  plotCI(x=means, y=1:dim(table)[1], 
         li = as.numeric(table[,2]), 
         ui=as.numeric(table[,3]), 
         err = "x", col = col,ylab="",yaxt="n",
         xlab="Estimate of confidence interval for odds ratio",
         main=table[1,5])
  
  axis(side=2,at=1:dim(table)[1],  
       label=table[,1])
  abline(v=1, col = 2)
}


# indigenous
table.sub.ind.2020.2<-lapply(c(2016,2017),prop.analysis2,data=data.ISDE.sub.indigenous.2020, demcode2=2015)

#Take métis people as example
graph.CI2(table.sub.ind.2020.2[[2]])

#All subgroups in indigenous people
par(mfrow=c(2,1))
lapply(table.sub.ind.2020.2,graph.CI2)



# disability
table.sub.dis.2020<-lapply(c(seq(2021,2035,by=2),2039),prop.analysis2,data=data.ISDE.sub.disability.2020, demcode2=2020)
table.sub.dis.2020<-lapply(table.sub.dis.2020,na.omit)
par(mfrow=c(3,3))
for (i in 1:9){
  title<-c("Seeing disability affects vision","Hearing disability",
           "Mobility issue ","Flexibility or dexterity issue","Mental health issue ",
           "Sensory / environmental disability ",
           "Chronic health condition or pain",
           "Cognitive disability",
           "Other disability")
  table.sub.dis.2020[[i]][,5]<-title[i]
}

#Take people with mental health issue as example
table.sub.dis.2020[[5]][,5]<-"People with mental health issue"
graph.CI2(table.sub.dis.2020[[5]])
#All subgroups in people with disabilities 
lapply(table.sub.dis.2020,graph.CI2)


# race
table.sub.race.2020<-lapply(c(seq(2043,2047,2),seq(2053,2063,2)),prop.analysis2,data=data.ISDE.sub.race.2020, demcode2=2042)
table.sub.race.2020<-lapply(table.sub.race.2020,na.omit)


#All subgroups in visible minorities
par(mfrow=c(3,3))
lapply(table.sub.race.2020,graph.CI2)



############################## OPTIONAL GRAPHS #####################################

#Take métis people as example

dev.off() # optional
graph.CI2(table.sub.ind.2020.2[[2]])

#Take Chinese as example
graph.CI2(table.sub.race.2020[[2]])

#Take people with mental health issue as example
table.sub.dis.2020[[5]][,5]<-"People with mental health issue"
graph.CI2(table.sub.dis.2020[[5]])



