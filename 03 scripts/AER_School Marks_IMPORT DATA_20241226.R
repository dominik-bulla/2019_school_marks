# Description --------------------------- --------------------------- ---------------------------
# project:
# file:
# author:
# Start date:



# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/2019_school_marks")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



# packages --------------------------- --------------------------- ---------------------------
library(dplyr)
library(readxl)



# import data [Kagarama] --------------------------- --------------------------- ---------------------------
files <- list.files(path = "01 raw data/School_Kagarama")
files <- files[c(grep("Annual", files), grep("Term 3", files))]
classes <- unique(substr(files, 1, 3))

for (class in classes) {
  
  fileA <- files[grep(class, files)]
  fileA <- fileA[grep("Annual", fileA)]
  fileA <- read.csv(paste0("01 raw data/School_Kagarama/", fileA))
  
  file3 <- files[grep(class, files)]
  file3 <- file3[grep("Term 3", file3)]
  file3 <- read.csv(paste0("01 raw data/School_Kagarama/", file3))
  file3 <- file3 %>%
    select(-c(Pos))

  file <- merge(file3, fileA, by = "Names", all = TRUE)
  file <- file %>%
    mutate(class = class) %>%
    select(class, Pos, Names, 
           Mathematics_CAT, Mathematics_EX, Mathematics_TOT, `Mathematics..`,
           English_CAT, English_EX, English_TOT, `English..`,
           Biology_CAT, Biology_EX, Biology_TOT, `Biology..`, 
           Chemistry_CAT, Chemistry_EX, Chemistry_TOT, `Chemistry..`, 
           Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, `Kinyarwanda..`,
           Physics_CAT, Physics_EX, Physics_TOT, `Physics..`,
           Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, `Entrepreneurship..`,
           Geography_CAT, Geography_EX, Geography_TOT, `Geography..`,
           History_CAT, History_EX, History_TOT, `History..`,
           French_CAT, French_EX, French_TOT, `French..`,
           ICT_CAT, ICT_EX, ICT_TOT, `ICT..`,
           Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, `Kiswahili..`,
           Literature.in.English_CAT, Literature.in.English_EX, Literature.in.English_TOT, `Literature.in.English..`,
           Religious.Studies_CAT, Religious.Studies_EX, Religious.Studies_TOT, `Religious.Studies..`,
           Sports_CAT, Sports_EX, Sports_TOT, `Sports..`) %>%
    rename("Mathematics_Annual" = `Mathematics..`,
           "English_Annual" = `English..`,
           "Biology_Annual" = `Biology..`, 
           "Chemistry_Annual" = `Chemistry..`, 
           "Kinyarwanda_Annual" = `Kinyarwanda..`,
           "Physics_Annual" = `Physics..`,
           "Entrepreneurship_Annual" = `Entrepreneurship..`,
           "Geography_Annual" = `Geography..`,
           "History_Annual" = `History..`,
           "French_Annual" = `French..`,
           "ICT_Annual" = `ICT..`,
           "Kiswahili_Annual" = `Kiswahili..`,
           "Literature_CAT" = Literature.in.English_CAT, 
           "Literature_EX" = Literature.in.English_EX, 
           "Literature_TOT" = Literature.in.English_TOT,
           "Literature_Annual" = `Literature.in.English..`,
           "Religion_CAT" = Religious.Studies_CAT, 
           "Religion_EX" = Religious.Studies_EX, 
           "Religion_TOT" = Religious.Studies_TOT,
           "Religion_Annual" = `Religious.Studies..`,
           "Sports_Annual" = `Sports..`)
  missing_indices <- which(is.na(file$Pos))
  start_serial <- max(file$Pos, na.rm = TRUE) + 1
  new_serials <- seq(from = start_serial, length.out = length(missing_indices))
  file$Pos[missing_indices] <- new_serials
  assign(class, file)
}  
rm(file, fileA, file3)
kagarama <- S1A
for (class in classes[2: length(classes)]) {
  data <- get(class)
  kagarama <- rbind(kagarama, data)
}  
kagarama <- kagarama %>%
  mutate(school = "Kagarama",
         gender = "") %>%
  rename("sno" = Pos,
         "name" = Names,) %>%
  select(school, class, sno, name, gender,
         Mathematics_CAT, Mathematics_EX, Mathematics_TOT, Mathematics_Annual, 
         English_CAT, English_EX, English_TOT, English_Annual, 
         Biology_CAT, Biology_EX, Biology_TOT, Biology_Annual, 
         Chemistry_CAT, Chemistry_EX, Chemistry_TOT, Chemistry_Annual, 
         Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, Kinyarwanda_Annual, 
         Physics_CAT, Physics_EX, Physics_TOT, Physics_Annual, 
         Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, Entrepreneurship_Annual, 
         Geography_CAT, Geography_EX, Geography_TOT, Geography_Annual, 
         History_CAT, History_EX, History_TOT, History_Annual, 
         French_CAT, French_EX, French_TOT, French_Annual, 
         ICT_CAT, ICT_EX, ICT_TOT, ICT_Annual, 
         Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, Kiswahili_Annual, 
         Literature_CAT, Literature_EX, Literature_TOT, Literature_Annual, 
         Religion_CAT, Religion_EX, Religion_TOT, Religion_Annual, 
         Sports_CAT, Sports_EX, Sports_TOT, Sports_Annual) %>%
  mutate(Mathematics_Annual = as.numeric(gsub("%", "", Mathematics_Annual)),
         English_Annual = as.numeric(gsub("%", "", English_Annual)),
         Biology_Annual = as.numeric(gsub("%", "", Biology_Annual)),
         Chemistry_Annual = as.numeric(gsub("%", "", Chemistry_Annual)),
         Kinyarwanda_Annual = as.numeric(gsub("%", "", Kinyarwanda_Annual)),
         Physics_Annual = as.numeric(gsub("%", "", Physics_Annual)),
         Entrepreneurship_Annual = as.numeric(gsub("%", "", Entrepreneurship_Annual)),
         Geography_Annual = as.numeric(gsub("%", "", Geography_Annual)),
         History_Annual = as.numeric(gsub("%", "", History_Annual)),
         French_Annual = as.numeric(gsub("%", "", French_Annual)),
         ICT_Annual = as.numeric(gsub("%", "", ICT_Annual)),
         Kiswahili_Annual = as.numeric(gsub("%", "", Kiswahili_Annual)),
         Literature_Annual = as.numeric(gsub("%", "", Literature_Annual)),
         Religion_Annual = as.numeric(gsub("%", "", Religion_Annual)),
         Sports_Annual = as.numeric(gsub("%", "", Sports_Annual)))
rm(data, S1A, S1B, S1C, S2A, S2B, S2C, S2D, class, classes, files, missing_indices, new_serials, start_serial)



# import data [Nyamata] --------------------------- --------------------------- ---------------------------
files <- list.files(path = "01 raw data/School_Nyamata")
files <- files[c(grep("Annual", files), grep("Term 3", files))]
classes <- unique(substr(files, 1, 3))

for (class in classes) {
  
  fileA <- files[grep(class, files)]
  fileA <- fileA[grep("Annual", fileA)]
  fileA <- read.csv(paste0("01 raw data/School_Nyamata/", fileA))
  
  fileA <- fileA %>%
    mutate(class = class,
           name = paste(LAST.NAME, FIRST.NAME)) %>%
    mutate( Mathematics_CAT = NA, 
            Mathematics_EX = NA, 
            Mathematics_TOT = NA, 
            English_CAT = NA, 
            English_EX = NA, 
            English_TOT = NA, 
            Biology_CAT = NA, 
            Biology_EX = NA, 
            Biology_TOT = NA,
            Chemistry_CAT = NA, 
            Chemistry_EX = NA, 
            Chemistry_TOT = NA,
            Kinyarwanda_CAT = NA, 
            Kinyarwanda_EX = NA, 
            Kinyarwanda_TOT = NA,
            Physics_CAT = NA, 
            Physics_EX = NA, 
            Physics_TOT = NA,
            Entrepreneurship_CAT = NA, 
            Entrepreneurship_EX = NA, 
            Entrepreneurship_TOT = NA, 
            Geography_CAT = NA, 
            Geography_EX = NA, 
            Geography_TOT = NA,
            History_CAT = NA, 
            History_EX = NA, 
            History_TOT = NA,
            French_CAT = NA, 
            French_EX = NA, 
            French_TOT = NA,
            ICT_CAT = NA, 
            ICT_EX = NA, 
            ICT_TOT = NA, 
            Kiswahili_CAT = NA, 
            Kiswahili_EX = NA, 
            Kiswahili_TOT = NA,
            Literature_CAT = NA, 
            Literature_EX = NA, 
            Literature_TOT = NA, 
            Religion_CAT = NA, 
            Religion_EX = NA, 
            Religion_TOT = NA,
            Sports_CAT = NA, 
            Sports_EX = NA, 
            Sports_TOT = NA) %>%
    rename("sno" = SNO,
           "gender" = SEX,
           "Biology_Annual" = `BIOLOGY.H..SCIENCE`,
           "Chemistry_Annual" = CHEMISTRY,
           "English_Annual" = ENGLISH,
           "Entrepreneurship_Annual" = ENTREPRENEURSHIP,
           "French_Annual" = FRENCH,
           "Geography_Annual" = `GEOGRAPHY...ENV`,
           "History_Annual" = `HISTORY...CITIZENSHIP`,
           "ICT_Annual" = ICT,
           "Kinyarwanda_Annual" = `KINYARWANDA`, 
           "Kiswahili_Annual" = `KISWAHILI`,
           "Literature_Annual" = `LIT.IN.ENGLISH`,
           "Mathematics_Annual" = MATHEMATICS,
           "Physics_Annual" = PHYSICS,
           "Religion_Annual" = `RELIGIOUS...ETHICS`,
           "Sports_Annual" = `P.E.S`,) 

  fileA <- fileA %>%
    filter(name != " ")
  
  assign(class, fileA)
}  
rm(fileA)
nyamata <- S1A
for (class in classes[2: length(classes)]) {
  data <- get(class)
  nyamata <- rbind(nyamata, data)
}  
nyamata <- nyamata %>%
  mutate(school = "Nyamata") %>%
  select(school, class, sno, name, gender, 
         Mathematics_CAT, Mathematics_EX, Mathematics_TOT, Mathematics_Annual, 
         English_CAT, English_EX, English_TOT, English_Annual, 
         Biology_CAT, Biology_EX, Biology_TOT, Biology_Annual, 
         Chemistry_CAT, Chemistry_EX, Chemistry_TOT, Chemistry_Annual, 
         Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, Kinyarwanda_Annual, 
         Physics_CAT, Physics_EX, Physics_TOT, Physics_Annual, 
         Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, Entrepreneurship_Annual, 
         Geography_CAT, Geography_EX, Geography_TOT, Geography_Annual, 
         History_CAT, History_EX, History_TOT, History_Annual, 
         French_CAT, French_EX, French_TOT, French_Annual, 
         ICT_CAT, ICT_EX, ICT_TOT, ICT_Annual, 
         Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, Kiswahili_Annual, 
         Literature_CAT, Literature_EX, Literature_TOT, Literature_Annual, 
         Religion_CAT, Religion_EX, Religion_TOT, Religion_Annual, 
         Sports_CAT, Sports_EX, Sports_TOT, Sports_Annual) %>%
  mutate(Mathematics_Annual = as.numeric(gsub("%", "", Mathematics_Annual)),
         English_Annual = as.numeric(gsub("%", "", English_Annual)),
         Biology_Annual = as.numeric(gsub("%", "", Biology_Annual)),
         Chemistry_Annual = as.numeric(gsub("%", "", Chemistry_Annual)),
         Kinyarwanda_Annual = as.numeric(gsub("%", "", Kinyarwanda_Annual)),
         Physics_Annual = as.numeric(gsub("%", "", Physics_Annual)),
         Entrepreneurship_Annual = as.numeric(gsub("%", "", Entrepreneurship_Annual)),
         Geography_Annual = as.numeric(gsub("%", "", Geography_Annual)),
         History_Annual = as.numeric(gsub("%", "", History_Annual)),
         French_Annual = as.numeric(gsub("%", "", French_Annual)),
         ICT_Annual = as.numeric(gsub("%", "", ICT_Annual)),
         Kiswahili_Annual = as.numeric(gsub("%", "", Kiswahili_Annual)),
         Literature_Annual = as.numeric(gsub("%", "", Literature_Annual)),
         Religion_Annual = as.numeric(gsub("%", "", Religion_Annual)),
         Sports_Annual = as.numeric(gsub("%", "", Sports_Annual)))
rm(data, S1A, S1B, S1C, S1D, S2A, S2B, S2C, S2D, class, classes, files)



# import data [Nyanza] --------------------------- --------------------------- ---------------------------

files <- list.files(path = "01 raw data/School_Nyanza")
files <- files[c(grep("Annual", files), grep("Term 3", files))]
classes <- unique(substr(files, 1, 3))

for (class in classes) {
  
  fileA <- files[grep(class, files)]
  fileA <- fileA[grep("Annual", fileA)]
  fileA <- read.csv(paste0("01 raw data/School_Nyanza/", fileA))
  fileA <- fileA %>%
    rename("sno" = SNO,
           "name" = `First.Name`,
           "Mathematics_Annual" = Mathematics,
           "English_Annual" = English, 
           "Kinyarwanda_Annual" = Kinyarwanda, 
           "Physics_Annual" = Physics, 
           "Chemistry_Annual" = Chemistry, 
           "Biology_Annual" = Biology, 
           "Geography_Annual" = Geography, 
           "History_Annual" = History, 
           "Entrepreneurship_Annual" = Entrepreneurship, 
           "ICT_Annual" = `Computer.Science`, 
           "French_Annual" = French, 
           "Kiswahili_Annual" = Kiswahili, 
           "Sports_Annual" = PES,
           "Literature_Annual" = Literature) %>%
    mutate(Religion_Annual = NA)
  
  file3 <- files[grep(class, files)]
  file3 <- file3[grep("Term 3", file3)]
  file3 <- read.csv(paste0("01 raw data/School_Nyanza/", file3))
  file3 <- file3 %>%
    rename("name" = `First.Name`,
           "Mathematics_TOT" = Mathematics,
           "English_TOT" = English, 
           "Kinyarwanda_TOT" = Kinyarwanda, 
           "Physics_TOT" = Physics, 
           "Chemistry_TOT" = Chemistry, 
           "Biology_TOT" = Biology, 
           "Geography_TOT" = Geography, 
           "History_TOT" = History, 
           "Entrepreneurship_TOT" = Entrepreneurship, 
           "ICT_TOT" = `Computer.Science`, 
           "French_TOT" = French, 
           "Kiswahili_TOT" = Kiswahili, 
           "Sports_TOT" = PES,
           "Literature_TOT" = Literature) %>%
    mutate(Mathematics_CAT = NA,
           Mathematics_EX = NA,
           English_CAT = NA,
           English_EX = NA, 
           Biology_CAT = NA, 
           Biology_EX = NA, 
           Chemistry_CAT = NA, 
           Chemistry_EX = NA, 
           Kinyarwanda_CAT = NA, 
           Kinyarwanda_EX = NA, 
           Physics_CAT = NA, 
           Physics_EX = NA, 
           Entrepreneurship_CAT = NA, 
           Entrepreneurship_EX = NA, 
           Geography_CAT = NA, 
           Geography_EX = NA, 
           History_CAT = NA, 
           History_EX = NA, 
           French_CAT = NA, 
           French_EX = NA, 
           ICT_CAT = NA, 
           ICT_EX = NA, 
           Kiswahili_CAT = NA, 
           Kiswahili_EX = NA, 
           Literature_CAT = NA, 
           Literature_EX = NA, 
           Religion_CAT = NA, 
           Religion_EX = NA, 
           Religion_TOT = NA,
           Sports_CAT = NA, 
           Sports_EX = NA)
  
  file <- merge(file3, fileA, by = "name", all = TRUE)
  file <- file %>%
    mutate(class = class) %>%
    select(class, sno, name, 
           Mathematics_CAT, Mathematics_EX, Mathematics_TOT, `Mathematics_Annual`,
           English_CAT, English_EX, English_TOT, `English_Annual`,
           Biology_CAT, Biology_EX, Biology_TOT, `Biology_Annual`, 
           Chemistry_CAT, Chemistry_EX, Chemistry_TOT, `Chemistry_Annual`, 
           Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, `Kinyarwanda_Annual`,
           Physics_CAT, Physics_EX, Physics_TOT, `Physics_Annual`,
           Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, `Entrepreneurship_Annual`,
           Geography_CAT, Geography_EX, Geography_TOT, `Geography_Annual`,
           History_CAT, History_EX, History_TOT, `History_Annual`,
           French_CAT, French_EX, French_TOT, `French_Annual`,
           ICT_CAT, ICT_EX, ICT_TOT, `ICT_Annual`,
           Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, `Kiswahili_Annual`,
           Literature_CAT, Literature_EX, Literature_TOT, `Literature_Annual`,
           Religion_CAT, Religion_EX, Religion_TOT, `Religion_Annual`,
           Sports_CAT, Sports_EX, Sports_TOT, `Sports_Annual`)
  assign(class, file)
}  

nyanza <- S1A
for (class in classes[2: length(classes)]) {
  data <- get(class)
  nyanza <- rbind(nyanza, data)
}  
nyanza <- nyanza %>%
  filter(name != "") %>%
  mutate(school = "Nyanza",
         gender = "")  %>%
  select(school, class, sno, name, gender,
         Mathematics_CAT, Mathematics_EX, Mathematics_TOT, Mathematics_Annual, 
         English_CAT, English_EX, English_TOT, English_Annual, 
         Biology_CAT, Biology_EX, Biology_TOT, Biology_Annual, 
         Chemistry_CAT, Chemistry_EX, Chemistry_TOT, Chemistry_Annual, 
         Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, Kinyarwanda_Annual, 
         Physics_CAT, Physics_EX, Physics_TOT, Physics_Annual, 
         Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, Entrepreneurship_Annual, 
         Geography_CAT, Geography_EX, Geography_TOT, Geography_Annual, 
         History_CAT, History_EX, History_TOT, History_Annual, 
         French_CAT, French_EX, French_TOT, French_Annual, 
         ICT_CAT, ICT_EX, ICT_TOT, ICT_Annual, 
         Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, Kiswahili_Annual, 
         Literature_CAT, Literature_EX, Literature_TOT, Literature_Annual, 
         Religion_CAT, Religion_EX, Religion_TOT, Religion_Annual, 
         Sports_CAT, Sports_EX, Sports_TOT, Sports_Annual) %>%
  mutate(Mathematics_Annual = as.numeric(gsub("%", "", Mathematics_Annual)),
         English_Annual = as.numeric(gsub("%", "", English_Annual)),
         Biology_Annual = as.numeric(gsub("%", "", Biology_Annual)),
         Chemistry_Annual = as.numeric(gsub("%", "", Chemistry_Annual)),
         Kinyarwanda_Annual = as.numeric(gsub("%", "", Kinyarwanda_Annual)),
         Physics_Annual = as.numeric(gsub("%", "", Physics_Annual)),
         Entrepreneurship_Annual = as.numeric(gsub("%", "", Entrepreneurship_Annual)),
         Geography_Annual = as.numeric(gsub("%", "", Geography_Annual)),
         History_Annual = as.numeric(gsub("%", "", History_Annual)),
         French_Annual = as.numeric(gsub("%", "", French_Annual)),
         ICT_Annual = as.numeric(gsub("%", "", ICT_Annual)),
         Kiswahili_Annual = as.numeric(gsub("%", "", Kiswahili_Annual)),
         Literature_Annual = as.numeric(gsub("%", "", Literature_Annual)),
         Religion_Annual = as.numeric(gsub("%", "", Religion_Annual)),
         Sports_Annual = as.numeric(gsub("%", "", Sports_Annual)))
rm(data, S1A, S1B, S2A, S2B, S2C, class, classes, file, file3, fileA, files)



# import data [Rango] --------------------------- --------------------------- ---------------------------

files <- list.files(path = "01 raw data/School_Rango")
files <- files[c(grep("Annual", files), grep("Term 3", files))]
classes <- unique(substr(files, 1, 3))

for (class in classes) {
  
  fileA <- files[grep(class, files)]
  fileA <- fileA[grep("Annual", fileA)]
  fileA <- read.csv(paste0("01 raw data/School_Rango/", fileA))
  fileA <- fileA %>%
    mutate(`First.Name` = paste(`First.Name`, `Last.Name`)) %>%
    rename("name" = `First.Name`,
           "gender" = Sex,
           "Mathematics_Annual" = MATHEMATICS,
           "English_Annual" = ENGLISH, 
           "Kinyarwanda_Annual" = KINYARWANDA, 
           "Physics_Annual" = PHYSICS, 
           "Chemistry_Annual" = CHEMISTRY, 
           "Biology_Annual" = `BIO.AND.H..SCIENCE`, 
           "Geography_Annual" = `GEOGRAPHY.AND.ENV`, 
           "History_Annual" = `HISTORY.AND.CITIZENSHIP`, 
           "Entrepreneurship_Annual" = ENTREPRENEURSHIP, 
           "ICT_Annual" = ICT, 
           "French_Annual" = FRENCH, 
           "Kiswahili_Annual" = KISWAHILI, 
           "Sports_Annual" = `PHYSICAL.EDU..AND.SPORT`,
           "Literature_Annual" = `LITERATURE.IN.ENGLISH`,
           "Religion_Annual" = `RELIGION.AND.ETHICS`) %>%
    mutate(Mathematics_Annual = Mathematics_Annual * 100, 
           English_Annual =  English_Annual * 100,
           Kinyarwanda_Annual = Kinyarwanda_Annual * 100, 
           Physics_Annual = Physics_Annual * 100, 
           Chemistry_Annual = Chemistry_Annual * 100,  
           Biology_Annual = Biology_Annual * 100, 
           Geography_Annual = Geography_Annual * 100, 
           History_Annual = History_Annual * 100, 
           Entrepreneurship_Annual = Entrepreneurship_Annual * 100,
           ICT_Annual = ICT_Annual * 100, 
           French_Annual =  French_Annual * 100,
           Kiswahili_Annual = Kiswahili_Annual * 100, 
           Sports_Annual = Sports_Annual * 100,
           Literature_Annual = Literature_Annual * 100,
           Religion_Annual = Religion_Annual * 100)
  
  file3 <- files[grep(class, files)]
  file3 <- file3[grep("Term 3", file3)]
  file3 <- read.csv(paste0("01 raw data/School_Rango/", file3))
  file3 <- file3 %>%
    mutate(`First.Name` = paste(`First.Name`, `Last.Name`)) %>%
    rename("name" = `First.Name`,
           "gender" = Sex,
           "Mathematics_CAT" = MATHEMATICS_Test, 
           "Mathematics_EX" = MATHEMATICS_Exam, 
           "English_CAT" = ENGLISH_Test, 
           "English_EX" = ENGLISH_Exam, 
           "Biology_CAT" = BIOLOGY_Test, 
           "Biology_EX" = BIOLOGY_Exam, 
           "Chemistry_CAT" = CHEMISTRY_Test, 
           "Chemistry_EX" = CHEMISTRY_Exam, 
           "Kinyarwanda_CAT" = KINYARWANDA_Test, 
           "Kinyarwanda_EX" = KINYARWANDA_Exam, 
           "Physics_CAT" = PHYSICS_Test, 
           "Physics_EX" = PHYSICS_Exam, 
           "Entrepreneurship_CAT" = ENTREPRENEURSHIP_Test, 
           "Entrepreneurship_EX" = ENTREPRENEURSHIP_Exam, 
           "Geography_CAT" = GEOGRAPHY_Test, 
           "Geography_EX" = GEOGRAPHY_Exam, 
           "History_CAT" = HISTORY_Test, 
           "History_EX" = HISTORY_Exam, 
           "French_CAT" = FRENCH_Test, 
           "French_EX" = FRENCH_Exam, 
           "ICT_CAT" = `COMPUTER.SCIENCE_Test`, 
           "ICT_EX" = `COMPUTER.SCIENCE_Exam`, 
           "Kiswahili_CAT" = KISWAHILI_Test, 
           "Kiswahili_EX" = KISWAHILI_Exam, 
           "Literature_CAT" = `LITERATURE.IN.ENGLISH_Test`, 
           "Literature_EX" = `LITERATURE.IN.ENGLISH_Exam`, 
           "Religion_CAT" = RELIGION_Test, 
           "Religion_EX" = RELIGION_Exam, 
           "Sports_CAT" = SPORT_Test, 
           "Sports_EX" = SPORT_Exam) %>%
    mutate(Mathematics_CAT = as.numeric(Mathematics_CAT),
           Mathematics_EX = as.numeric(Mathematics_EX), 
           English_CAT = as.numeric(English_CAT), 
           English_EX = as.numeric(English_EX), 
           Biology_CAT = as.numeric(Biology_CAT), 
           Biology_EX = as.numeric(Biology_EX), 
           Chemistry_CAT = as.numeric(Chemistry_CAT), 
           Chemistry_EX = as.numeric(Chemistry_EX), 
           Kinyarwanda_CAT = as.numeric(Kinyarwanda_CAT), 
           Kinyarwanda_EX = as.numeric(Kinyarwanda_EX), 
           Physics_CAT = as.numeric(Physics_CAT), 
           Physics_EX = as.numeric(Physics_EX), 
           Entrepreneurship_CAT = as.numeric(Entrepreneurship_CAT), 
           Entrepreneurship_EX = as.numeric(Entrepreneurship_EX), 
           Geography_CAT = as.numeric(Geography_CAT), 
           Geography_EX = as.numeric(Geography_EX), 
           History_CAT = as.numeric(History_CAT), 
           History_EX = as.numeric(History_EX),
           French_CAT = as.numeric(French_CAT), 
           French_EX = as.numeric(French_EX), 
           ICT_CAT = as.numeric(ICT_CAT), 
           ICT_EX = as.numeric(ICT_EX), 
           Kiswahili_CAT = as.numeric(Kiswahili_CAT), 
           Kiswahili_EX = as.numeric(Kiswahili_EX), 
           Literature_CAT = as.numeric(Literature_CAT), 
           Literature_EX = as.numeric(Literature_EX), 
           Religion_CAT = as.numeric(Religion_CAT), 
           Religion_EX = as.numeric(Religion_EX), 
           Sports_CAT = as.numeric(Sports_CAT), 
           Sports_EX = as.numeric(Sports_EX)) %>%
    mutate(Mathematics_TOT = Mathematics_CAT + Mathematics_EX, 
           English_TOT = English_CAT + English_EX, 
           Biology_TOT = Biology_CAT + Biology_EX, 
           Chemistry_TOT = Chemistry_CAT + Chemistry_EX, 
           Kinyarwanda_TOT = Kinyarwanda_CAT + Kinyarwanda_EX, 
           Physics_TOT = Physics_CAT + Physics_EX, 
           Entrepreneurship_TOT = Entrepreneurship_CAT + Entrepreneurship_EX, 
           Geography_TOT = Geography_CAT + Geography_EX, 
           History_TOT = History_CAT + History_EX,
           French_TOT = French_CAT + French_EX, 
           ICT_TOT = ICT_CAT + ICT_EX, 
           Kiswahili_TOT = Kiswahili_CAT + Kiswahili_EX, 
           Literature_TOT = Literature_CAT + Literature_EX, 
           Religion_TOT = Religion_CAT + Religion_EX, 
           Sports_TOT = Sports_CAT + Sports_EX) %>%
    select(-c(sno, `Last.Name`, gender))
  
  file <- merge(file3, fileA, by = "name", all = TRUE)
  file <- file %>%
    mutate(class = class) %>%
    select(class, sno, name, gender, 
           Mathematics_CAT, Mathematics_EX, Mathematics_TOT, `Mathematics_Annual`,
           English_CAT, English_EX, English_TOT, `English_Annual`,
           Biology_CAT, Biology_EX, Biology_TOT, `Biology_Annual`, 
           Chemistry_CAT, Chemistry_EX, Chemistry_TOT, `Chemistry_Annual`, 
           Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, `Kinyarwanda_Annual`,
           Physics_CAT, Physics_EX, Physics_TOT, `Physics_Annual`,
           Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, `Entrepreneurship_Annual`,
           Geography_CAT, Geography_EX, Geography_TOT, `Geography_Annual`,
           History_CAT, History_EX, History_TOT, `History_Annual`,
           French_CAT, French_EX, French_TOT, `French_Annual`,
           ICT_CAT, ICT_EX, ICT_TOT, `ICT_Annual`,
           Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, `Kiswahili_Annual`,
           Literature_CAT, Literature_EX, Literature_TOT, `Literature_Annual`,
           Religion_CAT, Religion_EX, Religion_TOT, `Religion_Annual`,
           Sports_CAT, Sports_EX, Sports_TOT, `Sports_Annual`)
  assign(class, file)
}  

rango <- S1A
for (class in classes[2: length(classes)]) {
  data <- get(class)
  rango <- rbind(rango, data)
}  
rango <- rango %>%
  mutate(school = "Rango")  %>%
  select(school, class, sno, name, gender,
         Mathematics_CAT, Mathematics_EX, Mathematics_TOT, Mathematics_Annual, 
         English_CAT, English_EX, English_TOT, English_Annual, 
         Biology_CAT, Biology_EX, Biology_TOT, Biology_Annual, 
         Chemistry_CAT, Chemistry_EX, Chemistry_TOT, Chemistry_Annual, 
         Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, Kinyarwanda_Annual, 
         Physics_CAT, Physics_EX, Physics_TOT, Physics_Annual, 
         Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, Entrepreneurship_Annual, 
         Geography_CAT, Geography_EX, Geography_TOT, Geography_Annual, 
         History_CAT, History_EX, History_TOT, History_Annual, 
         French_CAT, French_EX, French_TOT, French_Annual, 
         ICT_CAT, ICT_EX, ICT_TOT, ICT_Annual, 
         Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, Kiswahili_Annual, 
         Literature_CAT, Literature_EX, Literature_TOT, Literature_Annual, 
         Religion_CAT, Religion_EX, Religion_TOT, Religion_Annual, 
         Sports_CAT, Sports_EX, Sports_TOT, Sports_Annual)
rm(data, S1A, S1B, S2A, S2B, class, classes, file, file3, fileA, files)



# merge data --------------------------- --------------------------- ---------------------------

school_marks <- rbind(kagarama, nyamata, nyanza, rango)
school_marks <- school_marks %>%
  filter(name != "") %>%
  filter(name != " ")
school_marks <- school_marks %>%
  mutate(sno2 = 1: nrow(school_marks)) %>%
  mutate(sno2 = paste0(school, sno2)) 
school_marks <- school_marks %>%                                             # These are the newly added school mark data.
  mutate(sno2 = ifelse(sno2 == "Rango998", "Rango1998", sno2),               # To make sure they do not collide with other
         sno2 = ifelse(sno2 == "Rango1027", "Rango2027", sno2),               # data points, they have been increased by 1000.
         sno2 = ifelse(sno2 == "Rango1037", "Rango2037", sno2),
         sno2 = ifelse(sno2 == "Rango1038", "Rango2038", sno2),
         sno2 = ifelse(sno2 == "Rango1039", "Rango2039", sno2),
         sno2 = ifelse(sno2 == "Rango1086", "Rango2086", sno2),
         sno2 = ifelse(sno2 == "Rango1087", "Rango2087", sno2),
         sno2 = ifelse(sno2 == "Rango1088", "Rango2088", sno2),
         sno2 = ifelse(sno2 == "Rango1089", "Rango2089", sno2),
         sno2 = ifelse(sno2 == "Rango1042", "Rango2042", sno2),
         sno2 = ifelse(sno2 == "Rango1091", "Rango2091", sno2),
         sno2 = ifelse(sno2 == "Rango1043", "Rango2043", sno2),
         sno2 = ifelse(sno2 == "Rango1044", "Rango2044", sno2),
         sno2 = ifelse(sno2 == "Rango1045", "Rango2045", sno2),
         sno2 = ifelse(sno2 == "Rango1092", "Rango2092", sno2),
         sno2 = ifelse(sno2 == "Rango1093", "Rango2093", sno2),
         sno2 = ifelse(sno2 == "Rango1094", "Rango2094", sno2),
         sno2 = ifelse(sno2 == "Rango1095", "Rango2095", sno2))



# include baseline index numbers --------------------------- --------------------------- ---------------------------

student_ids <- read_excel("01 raw data/school_marks_sno_20241228_V01.xlsx")
student_ids <- student_ids %>%
  filter(SCHOOLMARKS_SNO != "") %>%
  rename("baseline_id" = SNO,
         "baseline_name" = Name) %>%
  select(-c(School, Class, Age, Sex, Name1))
school_marks <- merge(school_marks, student_ids, by.x = "sno2", by.y = "SCHOOLMARKS_SNO", all.x = TRUE)
school_marks <- school_marks %>%
  select(school, class, sno2, baseline_id,
         name, baseline_name, gender, 
         Mathematics_CAT, Mathematics_EX, Mathematics_TOT, Mathematics_Annual, 
         English_CAT, English_EX, English_TOT, English_Annual, 
         Biology_CAT, Biology_EX, Biology_TOT, Biology_Annual, 
         Chemistry_CAT, Chemistry_EX, Chemistry_TOT, Chemistry_Annual, 
         Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, Kinyarwanda_Annual, 
         Physics_CAT, Physics_EX, Physics_TOT, Physics_Annual, 
         Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, Entrepreneurship_Annual, 
         Geography_CAT, Geography_EX, Geography_TOT, Geography_Annual, 
         History_CAT, History_EX, History_TOT, History_Annual, 
         French_CAT, French_EX, French_TOT, French_Annual, 
         ICT_CAT, ICT_EX, ICT_TOT, ICT_Annual, 
         Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, Kiswahili_Annual, 
         Literature_CAT, Literature_EX, Literature_TOT, Literature_Annual, 
         Religion_CAT, Religion_EX, Religion_TOT, Religion_Annual, 
         Sports_CAT, Sports_EX, Sports_TOT, Sports_Annual)



# save school-mark data --------------------------- --------------------------- ---------------------------

write.csv(school_marks, "02 processed data/2019_school_marks_20241227.csv", row.names = FALSE)








