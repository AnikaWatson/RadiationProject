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

str(dose)

iterationsx = dim(dose)[1]
iterationsy = dim(dose)[2]

theta1 = pi/3
rotation1 = c(cos(theta1), sin(theta1), -sin(theta1), cos(theta1))
A1 = matrix(rotation1, nrow = 2, ncol = 2)
  i=1
  j=2
output1 <- matrix(ncol=iterationsy, nrow=iterationsx)

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

output1
imagesc(output1, xlab="cols", ylab="rows", col=jet.colors(16))
output1 <- data.frame(output1)
class(output1)


