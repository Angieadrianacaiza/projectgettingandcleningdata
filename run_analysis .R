DESCRIBING THE SCRIPT 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.


# Establecer el directorio de trabajo (ajusta según tu caso)
setwd("~/Desktop/coursera ds/run_analysis.R")

# Descomprimir el archivo ZIP en una carpeta llamada "datos"
unzip(zipfile = "DATA.zip", exdir = "datos")

# Listar los archivos extraídos (para verificar)
list.files("datos")

install.packages("data.table") # Instalar el paquete
library(data.table)            # Cargarlo en la sesión

setwd("~/Desktop/coursera ds/run_analysis.R/datos/UCI HAR Dataset")

##PASO 3-4 
##load activity_labels.txt and features.txt archivos de la carpeta UCI HAR Dataset 

path <- getwd() #directorio a la carpeta UCI HAR Dataset 
activityLabels <- fread(
  file.path(path, "activity_labels.txt"),
  col.names = c("classLabels", "activityNames"))

features <-fread(
  file.path(path, "features.txt"),
  col.names = c("index", "featureNames"))

install.packages("gsubfn")
install.packages("proto")
library(proto)
library(gsubfn)

##filtrar las columnas relevantes (las que contienen "mean()" o "std()")
## y renombrar las columnas con los nombres descriptivos que creaste usando gsubfn()

featuresNeeded <- grep("(mean|std)\\(\\)", features$featureNames)
measurements <- features[featuresNeeded,][["featureNames"]]
measurements <- gsubfn(     
  "(^t|^f|Acc|Gyro|Mag|BodyBody|\\(\\))",
  list(
    "t" = "time",
    "f" = "Frequency",
    "Acc" = "Accelerometer",
    "Gyro" = "Gyroscope",
    "Mag" = "Magnitude",
    "BodyBody" = "Body",
    "()" = ""
  ),
  measurements
)

##paso 1 y 2
#load train data
setwd("~/Desktop/coursera ds/proyect 3 getting and cleaning data/datos/UCI HAR Dataset/train")
path <- getwd()
train <- fread(file.path(path, "X_train.txt"))[, featuresNeeded, with = FALSE]
setnames(train, colnames(train), measurements)
head(colnames(train),3)
activityTrain <- fread(file.path(path,"y_train.txt"),
                       col.names = "Activity")
subjectTrain <- fread(file.path(path,"subject_train.txt"),
                      col.names = "SubjectNo.")
train <- cbind(activityTrain, subjectTrain, train) # bind all columns together

#load test data
setwd("~/Desktop/coursera ds/proyect 3 getting and cleaning data/datos/UCI HAR Dataset/test")
path <- getwd()
test <- fread(file.path(path, "X_test.txt"))[, featuresNeeded, with = FALSE]
setnames(test, colnames(test), measurements)
activityTest <-fread(file.path(path, "y_test.txt"),col.names = "Activity")
subjectTest <-fread(file.path(path, "subject_test.txt"),col.names = "SubjectNo.")

test <- cbind(activityTest, subjectTest, test)
# merge test and train by rows
testTrain <- rbind(train, test)
testTrain[["Activity"]] <- factor(
  testTrain[, Activity],
  levels = activityLabels[["classLabels"]],  # Niveles (códigos numéricos: 1, 2, ...)
  labels = activityLabels[["activityNames"]] # Etiquetas (nombres: "WALKING", "SITTING", ...)
)

testTrain[["SubjectNo."]] <- as.factor(testTrain[, SubjectNo.])

# melt then cast the data table
testTrain <- melt.data.table(testTrain, id=c("SubjectNo.", "Activity")) # melt down to variable & value
testTrain <- dcast(testTrain, SubjectNo. + Activity ~ variable, mean) # average of SubjectNo & Activity

# write final tidy data into new file
fwrite(testTrain, file="tidyData.txt")








