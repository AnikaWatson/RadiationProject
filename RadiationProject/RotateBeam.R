#510 Final Project
#installing necessary packages
install.packages("matlab")
library(matlab)
#create input+output folder 
wd <- getwd()

#check if input folder already exists otherwise create one
#####ATTENTION:data to work with should be stored in this folder before running the code!!
folders <- "data_input"
if (file.exists(folders) == FALSE) {
  dir.create(file.path(wd, folders), showWarnings = FALSE) 
} else print("Input Folder Already exists")

#output folder check 
figures <- "figures"
if (file.exists(figures) == FALSE) {
  dir.create(file.path(wd, figures), showWarnings = FALSE) 
} else print("Output Folder Already exists")

#map folders to R structure
outputlocation <- paste(wd, "/figures/" , sep = "")
inputlocation <- paste(wd, "/data_input/" , sep = "")
data.files <- list.files(inputlocation)

#inital load of data
dose <- read.csv(paste(inputlocation, data.files[1], sep=""), stringsAsFactors = FALSE )
#Let's see what the data looks like
str(dose)
#looks good

for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    dose[i,j] <- (dose[i,j])/1000
  }
}

######## Rotating the Beam #####
#We are going to populate a new grid of numbers with the strength of the angled beam at each point
#First let's tell the computer how many iterations we need to fill the grid
iterationsx = dim(dose)[1]
iterationsy = dim(dose)[2]
#now let's set the angle that we want to rotate the beam
theta1 = pi/3
#and define the rotation matrix that will rotate a vector by that amount
rotation1 = c(cos(theta1), sin(theta1), -sin(theta1), cos(theta1))
A1 = matrix(rotation1, nrow = 2, ncol = 2)
#Now let's make an empty grid (its technically a matrix) to populate with the dosage values
output1 <- matrix(ncol=iterationsy, nrow=iterationsx)
#And now we can populate that grid with the new dosage values from the rotated beam using a for loop
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    vec = matrix(c((i-25),(j-25)), nrow = 2, ncol = 1)
    rot = A1 %*% vec
    if(round(rot[1])<=-25){
      output1[i,j] <- 0
    } else {if(round(rot[1])>=25){
      output1[i,j] <- 0
    } else {if(round(rot[2])>=25){
      output1[i,j] <- 0
    } else {if(round(rot[2])<=-25){
      output1[i,j] <- 0
    } else {output1[i,j] <- dose[(round(rot[1])+25), (round(rot[2])+25)]
    }}
    }
    }
  }
}

#Let's check to make sure that the matrix was populated properly
output1
#And let's take a peek to ensure we rotated the beam and didn't cause complete chaos
pdf(paste(wd, "/figures/Beam Rotated by theta_1.pdf", sep=""))
imagesc(output1, xlab="", ylab="", col=jet.colors(16))
dev.off()

#Now that we have the code for one rotated beam we can repeat this process for however many beams we have
#set the angle
theta2 = -pi/3
#define the rotation matrix
rotation2 = c(cos(theta2), sin(theta2), -sin(theta2), cos(theta2))
A2 = matrix(rotation2, nrow = 2, ncol = 2)
#make an empty grid 
output2 <- matrix(ncol=iterationsy, nrow=iterationsx)
#populate that grid with the new dosage values
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    vec = matrix(c((i-25),(j-25)), nrow = 2, ncol = 1)
    rot = A2 %*% vec
    if(round(rot[1])<=-25){
      output2[i,j] <- 0
    } else {if(round(rot[1])>=25){
      output2[i,j] <- 0
    } else {if(round(rot[2])>=25){
      output2[i,j] <- 0
    } else {if(round(rot[2])<=-25){
      output2[i,j] <- 0
    } else {output2[i,j] <- dose[(round(rot[1])+25), (round(rot[2])+25)]
    }}
    }
    }
  }
}
#make sure that the matrix was populated properly
output2
#ensure we rotated the beam and didn't cause complete chaos
pdf(paste(wd, "/figures/Beam Rotated by theta_2.pdf", sep=""))
imagesc(output2, xlab="", ylab="", col=jet.colors(16))
dev.off()


#set the angle
theta3 = pi/58
#define the rotation matrix
rotation3 = c(cos(theta3), sin(theta3), -sin(theta3), cos(theta3))
A3 = matrix(rotation3, nrow = 2, ncol = 2)
#make an empty grid
output3 <- matrix(ncol=iterationsy, nrow=iterationsx)
#populate that grid
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    vec = matrix(c((i-25),(j-25)), nrow = 2, ncol = 1)
    rot = A3 %*% vec
    if(round(rot[1])<=-25){
      output3[i,j] <- 0
    } else {if(round(rot[1])>=25){
      output3[i,j] <- 0
      } else {if(round(rot[2])>=25){
        output3[i,j] <- 0
      } else {if(round(rot[2])<=-25){
        output3[i,j] <- 0
      } else {output3[i,j] <- dose[(round(rot[1])+25), (round(rot[2])+25)]
      }}
      }
    }
  }
}
#make sure that the matrix was populated   
output3
#ensure we rotated the beam and didn't cause complete chaos
pdf(paste(wd, "/figures/Beam Rotated by theta_3.pdf", sep=""))
imagesc(output3, xlab="", ylab="", col=jet.colors(16))
dev.off()


####### Finding the Total Dosage at Each Point #######
#Now we know how much dose each point gets from each beam

#Let's make a data frame of the total dose received at each  point
TotalDose <- matrix(ncol=iterationsy, nrow=iterationsx)

#And now we can populate that grid with the sum of the doses we found above
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    TotalDose[i,j] <- output1[i,j]+output2[i,j]+output3[i,j]
        }
}
#make sure that the matrix was populated
TotalDose
#ensure we rotated the beam and didn't cause complete chaos
pdf(paste(wd, "/figures/Total Dose.pdf", sep=""))
imagesc(TotalDose, xlab="", ylab="", col=jet.colors(16))
dev.off()



##### Isolating the Tumour #####
#Since we have been asked to predict how much of the tumour survives we only care about the tumour 
#So, let's throw out all the data except for the cells that are in the tumour
#To do so we need a tumour, so let's load some tumour data
Tumour1 <- read.csv(paste(inputlocation, data.files[3], sep=""), stringsAsFactors = FALSE )
#Let's see what the data looks like
pdf(paste(wd, "/figures/Tumour1.pdf", sep=""))
imagesc(as.matrix(Tumour1), xlab="", ylab="", col=jet.colors(16))
dev.off()
#looks good

#Let's make a data frame of the total dose received at each point in the tumour
#again, we start with a grid
TumourDose1 <- matrix(ncol=iterationsy, nrow=iterationsx)

#And we populate that grid
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    TumourDose1[i,j] <- TotalDose[i,j]*Tumour1[i,j]
  }
}
#make sure that the matrix was populated
TumourDose1
#take a peek
pdf(paste(wd, "/figures/Tumour1 Dose.pdf", sep=""))
imagesc(TumourDose1, xlab="", ylab="", col=jet.colors(16))
dev.off()


##### Calculating the Survival Rate at Each Point in the Tumour ####
#Now we know the dose received at each point in the tumour, but we want to know if it lives
#First we set alpha and beta as defined in the paper
a=0.01
b=0.0025
exp(-(a*(TumourDose1[20,30])+b*(TumourDose1[20,30])^2))
options(digits = 22)
#Next we need a data frame of the cell survival
CellSurvival1 <- matrix(ncol=iterationsy, nrow=iterationsx)

for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    CellSurvival1[i,j] <- 100*exp(-(a*(TumourDose1[i,j])+b*(TumourDose1[i,j])^2))
  }
}
#make sure that the matrix was populated
CellSurvival1
#Take a peek
pdf(paste(wd, "/figures/Tumor1 S(p).pdf", sep=""))
imagesc(CellSurvival1, xlab="", ylab="", col=jet.colors(16))
dev.off()
#turn the grid into a data frame


#Now let's make a histogram to visualize how zapped the tumour was
#First we need to get rid of all the zeros everywhere outside the tumour
#because that will throw the histogram off
HowZapped1 <- matrix(ncol=iterationsy, nrow=iterationsx)
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    if(Tumour1[i,j] == 1){
      HowZapped1[i,j] <- CellSurvival1[i,j]
    } else {
      HowZapped1[i,j] <- NA}
  }
}
HowZapped1 <- data.frame(HowZapped1)

#now we can safely create a histogram
pdf(paste(wd, "/figures/Tumor1 S(p) Histogram.pdf", sep=""))
hist(unlist(HowZapped1), xlab = "S(p)", col = "red")
dev.off()

##### Quantifying How Much of the Tumour Survives #####

# To do so we must define a cutoff for cells we decide have survived
cutoff = 1000000000

#now let's count the number of cells that received a high enough dose to be presumed dead
deadcells1 = 0
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    if(CellSurvival1[i,j]>cutoff){
      deadcells1 <- deadcells1+1
    }
    }
}
deadcells1

#this numer bepends heavily on the number of cells we chose for our grid
#so let's calculate the percentage of the tumour that survived to be more general
#to do this we need to quantify the size of the original tumour by counting its cells
totcells1 = 0
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    if(Tumour1[i,j] == 1){
      totcells1 <- totcells1+1
    }
  }
}
totcells1

#now we can calculate the percentage of the tumour that survives
survived1 = 100*(totcells1-deadcells1)/totcells1

#check the number
survived1


#And we  can easily repeat this process for various  tumour shapes, and locations
#To do so we need a new tumour
Tumour2 <- read.csv(paste(inputlocation, data.files[4], sep=""), stringsAsFactors = FALSE )
#Let's see what the data looks like
str(Tumour2)
#looks good
pdf(paste(wd, "/figures/Tumor2.pdf", sep=""))
imagesc(as.matrix(Tumour2), xlab="", ylab="", col=jet.colors(16))
dev.off()

#Let's make a data frame of the total dose received at each point in the tumour
TumourDose2 <- matrix(ncol=iterationsy, nrow=iterationsx)

#And now we can populate that grid
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    TumourDose2[i,j] <- TotalDose[i,j]*Tumour2[i,j]
  }
}
#make sure that the matrix was populated
TumourDose2
#check it out
pdf(paste(wd, "/figures/Tumor2 Dose.pdf", sep=""))
imagesc(TumourDose2, xlab="", ylab="", col=jet.colors(16))
dev.off()

#Let's make a data frame of the cell survival
CellSurvival2 <- matrix(ncol=iterationsy, nrow=iterationsx)

#And now we can populate that grid
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    CellSurvival2[i,j] <- 100*exp(-(a*(TumourDose2[i,j])+b*(TumourDose2[i,j])^2))
  }
}
#make sure that the matrix was populated
CellSurvival2
#visualize
pdf(paste(wd, "/figures/Tumor2 S(p).pdf", sep=""))
imagesc(CellSurvival2, xlab="", ylab="", col=jet.colors(16))
dev.off()

#Now let's make a histogram' to visualize how zapped the tumour was
#Get rid of unecessary the zeros
HowZapped2 <- matrix(ncol=iterationsy, nrow=iterationsx)
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    if(TumourDose2[i,j]>1){
      HowZapped2[i,j] <- CellSurvival2[i,j]
    } else {
      HowZapped2[i,j] <- NA}
  }
}
HowZapped2 <- data.frame(HowZapped2)
pdf(paste(wd, "/figures/Tumor2 S(p) Histogram.pdf", sep=""))
hist(unlist(HowZapped2), xlab = "S(p)", col = "red")
dev.off()

# Now let's see how many tumour cells died
deadcells2 = 0
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    if(CellSurvival1[i,j]>cutoff){
      deadcells2 <- deadcells2+1
    }
  }
}
deadcells2

#now let's calculate the percentage of the tumour that survived
#to do so we need  to quantify the size  of the original tumour
totcells2 = 0
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    if(Tumour2[i,j] == 1){
      totcells2 <- totcells2+1
    }
  }
}
totcells2

#now we can calculate the percentage of the tumour that survives
survived2 = 100*(totcells2-deadcells2)/totcells2





