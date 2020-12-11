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
#use only fish data from 2011
dose <- read.csv(paste(inputlocation, data.files[1], sep=""), stringsAsFactors = FALSE )
#Let's see what the data looks like
str(dose)
#looks good

######## Rotating the Beam#####
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
#Ans now we can populate that grid with the new dosage values from the rotated beam using a for loop
for(i in 1:iterationsx){
  for(j in 1:iterationsy){
    vec = matrix(c((i-25),(j-25)), nrow = 2, ncol = 1)
    rot = A1 %*% vec
    if(norm(rot)<24){
    output1[i,j] <- dose[(round(rot[1])+25), (round(rot[2])+25)]
    } else {
      output1[i,j] <- 0}
    }
}
#Let's check to make sure that the matrix was populated properly
output1
#And let's take a peek to ensure we rotated the beam and didn't cause complete chaos
imagesc(output1, xlab="cols", ylab="rows", col=jet.colors(16))
#looks good, now let's turn that grid which is technically a matrix into a data frame
output1 <- data.frame(output1)
#and double check that the last step worked
class(output1)

#Now that we have the code for one rotated beam we can repeat this process for however many beams we have




