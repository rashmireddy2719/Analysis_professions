library(tidyr)
library(dplyr)
library(tidyverse)
library(factoextra)

mydata <- read.csv("E:/qualtrics/qualtrics.csv",stringsAsFactors = FALSE)

names(mydata) <- c("StartDate",	"EndDate",	"Status",	"IPAddress",	"Progress",	"Duration (in seconds)",	"Finished",	"RecordedDate",	"ResponseId",	"RecipientLastName",	"RecipientFirstName",	"RecipientEmail",	"ExternalReference",	"LocationLatitude",	"LocationLongitude",	"DistributionChannel",
                   "UserLanguage",	"TurkID",	"Age",	"Sex",	"Sex_3_TEXT",	"Gender",	"Gender_TEXT",	"Masc_1",	"Fem_1",	"Race",	"Race_TEXT",	"Political",	"Political_TEXT",	"Profession",	"Profession_TEXT",	"q1",	"qf1",	"ql1",	"qp1",	"qc1",	"q2",	"qf2",	"ql2",	"qp2",	"qc2",	"q3",	"qf3",	"ql3",	"qp3",	"qc3",	"q4",	"qf4",	"ql4",	"qp4",	"qc4",	"q5",	"qf5",	"ql5",	"qp5",	"qc5",	"q6",	"qf6",	"ql6",	"qp6",
                   "qc6",	"q7",	"qf7",	"ql7",	"qp7",	"qc7",	"q8",	"qf8",	"ql8",	"qp8",	"qc8",	"c1",	"cf1",	"cl1",	"cp1",	"cc1",	"q9",	"qf9",	"ql9",	"qp9",	"qc9",	"q10",	"qf10",	"ql10",	"qp10",	"qc10",	"q11",	"qf11",	"ql11",	"qp11",	"qc11",	"q12",	"qf12",	"ql12",	"qp12",	"qc12",	"q13",	"qf13",	"ql13",	"qp13",	"qc13",	"q14",	"qf14",	"ql14",	"qp14",	"qc14",	"q15",	"qf15",	"ql15",	"qp15",	"qc15",	"q16",	"qf16",	"ql16",
                   "qp16",	"qc16",	"q17",	"qf17",	"ql17",	"qp17",	"qc17",	"q18",	"qf18",	"ql18",	"qp18",	"qc18",	"q19",	"qf19",	"ql19",	"qp19",	"qc19",	"q20",	"qf20",	"ql20",	"qp20",	"qc20",	"q21",	"qf21",	"ql21",	"qp21",	"qc21",	"q22",	"qf22",	"ql22",	"qp22",	"qc22",	"q23",	"qf23",	"ql23",	"qp23",	"qc23",	"q24",	"qf24",	"ql24",	"qp24",	"qc24",	"c2",	"cf2",	"cl2",	"cp2",	"cc2",	"q25",	"qf25",	"ql25",	"qp25",	"qc25",	"q26",
                   "qf26",	"ql26",	"qp26",	"qc26",	"q27",	"qf27",	"ql27",	"qp27",	"qc27",	"q28",	"qf28",	"ql28",	"qp28",	"qc28",	"q29",	"qf29",	"ql29",	"qp29",	"qc29",	"q30",	"qf30",	"ql30",	"qp30",	"qc30",	"q31",	"qf31",	"ql31",	"qp31",	"qc31",	"q32",	"qf32",	"ql32",	"qp32",	"qc32",	"q33",	"qf33",	"ql33",	"qp33",	"qc33",	"q34",	"qf34",	"ql34",	"qp34",	"qc34",	"q35",	"qf35",	"ql35",	"qp35",	"qc35",	"q36",	"qf36",	"ql36",	"qp36",
                   "qc36",	"q37",	"qf37",	"ql37",	"qp37",	"qc37",	"q38",	"qf38",	"ql38",	"qp38",	"qc38",	"q39",	"qf39",	"ql39",	"qp39",	"qc39",	"q40",	"qf40",	"ql40",	"qp40",	"qc40",	"c3",	"cf3",	"cl3",	"cp3",	"cc3",	"q41",	"qf41",	"ql41",	"qp41",	"qc41",	"q42",	"qf42",	"ql42",	"qp42",	"qc42",	"q43",	"qf43",	"ql43",	"qp43",	"qc43",	"q44",	"qf44",	"ql44",	"qp44",	"qc44",	"q45",	"qf45",	"ql45",	"qp45",	"qc45",	"q46",	"qf46",
                   "ql46",	"qp46",	"qc46",	"q47",	"qf47",	"ql47",	"qp47",	"qc47",	"q48",	"qf48",	"ql48",	"qp48",	"qc48",	"q49",	"qf49",	"ql49",	"qp49",	"qc49",	"q50",	"qf50",	"ql50",	"qp50",	"qc50",	"q51",	"qf51",	"ql51",	"qp51",	"qc51",	"q52",	"qf52",	"ql52",	"qp52",	"qc52",	"q53",	"qf53","ql53",	"qp53",	"qc53",	"q54",	"qf54",	"ql54",	"qp54",	"qc54",	"q55",	"qf55",	"ql55",	"qp55",	"qc55",	"q56",	"qf56",	"ql56",	"qp56",
                   "qc56",	"c4",	"cf4",	"cl4",	"cp4",	"cc4",	"q57",	"qf57",	"ql57",	"qp57",	"qc57",	"q58",	"qf58",	"ql58",	"qp58",	"qc58",	"q59",	"qf59",	"ql59",	"qp59",	"qc59",	"q60",	"qf60",	"ql60",	"qp60",	"qc60",	"q61",	"qf61",	"ql61",	"qp61",	"qc61",	"q62",	"qf62",	"ql62",	"qp62",	"qc62",	"q63",	"qf63",	"ql63",	"qp63",	"qc63",	"q64",	"qf64",	"ql64",	"qp64",	"qc64",	"mTurkCode",	"professions",	"stereotypes",	"pair",
                   "Sex_3_TEXT - Topics")

# removing top rows as they have no information
mydata <- mydata[-c(1:2),]

# subsetting data for pair and responses
mydata1 <- mydata[,c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10",
                     "q11","q12","q13","q14","q15","q16","q17","q18","q19","q20",
                     "q21","q22","q23","q24","q25","q26","q27","q28","q29","q30",
                     "q31","q32","q33","q34","q35","q36","q37","q38","q39","q40",
                     "q41","q42","q43","q44","q45","q46","q47","q48","q49","q50",
                     "q51","q52","q53","q54","q55","q56","q57","q58","q59","q60",
                     "q61","q62","q63","q64","c1","c2","c3","c4","pair")]

# subsetting data for timing

timingData <- mydata[,c("qf1","ql1","qp1","qc1","qf2","ql2","qp2","qc2","qf3","ql3","qp3","qc3","qf4","ql4","qp4","qc4","qf5","ql5","qp5","qc5","qf6","ql6","qp6","qc6","qf7","ql7","qp7","qc7","qf8","ql8","qp8","qc8","qf9","ql9","qp9","qc9","qf10","ql10","qp10","qc10",
                        "qf11","ql11","qp11","qc11","qf12","ql12","qp12","qc12","qf13","ql13","qp13","qc13","qf14","ql14","qp14","qc14","qf15","ql15","qp15","qc15","qf16","ql16","qp16","qc16","qf17","ql17","qp17","qc17","qf18","ql18","qp18","qc18","qf19","ql19","qp19","qc19","qf20","ql20","qp20","qc20",
                        "qf21","ql21","qp21","qc21","qf22","ql22","qp22","qc22","qf23","ql23","qp23","qc23","qf24","ql24","qp24","qc24","qf25","ql25","qp25","qc25","qf26","ql26","qp26","qc26","qf27","ql27","qp27","qc27","qf28","ql28","qp28","qc28","qf29","ql29","qp29","qc29","qf30","ql30","qp30","qc30",
                        "qf31","ql31","qp31","qc31","qf32","ql32","qp32","qc32","qf33","ql33","qp33","qc33","qf34","ql34","qp34","qc34","qf35","ql35","qp35","qc35","qf36","ql36","qp36","qc36","qf37","ql37","qp37","qc37","qf38","ql38","qp38","qc38","qf39","ql39","qp39","qc39","qf40","ql40","qp40","qc40",
                        "qf41","ql41","qp41","qc41","qf42","ql42","qp42","qc42","qf43","ql43","qp43","qc43","qf44","ql44","qp44","qc44","qf45","ql45","qp45","qc45","qf46","ql46","qp46","qc46","qf47","ql47","qp47","qc47","qf48","ql48","qp48","qc48","qf49","ql49","qp49","qc49","qf50","ql50","qp50","qc50",
                        "qf51","ql51","qp51","qc51","qf52","ql52","qp52","qc52","qf53","ql53","qp53","qc53","qf54","ql54","qp54","qc54","qf55","ql55","qp55","qc55","qf56","ql56","qp56","qc56","qf57","ql57","qp57","qc57","qf58","ql58","qp58","qc58","qf59","ql59","qp59","qc59","qf60","ql60","qp60","qc60",
                        "qf61","ql61","qp61","qc61","qf62","ql62","qp62","qc62","qf63","ql63","qp63","qc63","qf64","ql64","qp64","qc64","cf1","cl1","cp1","cc1","cf2","cl2","cp2","cc2","cf3","cl3","cp3","cc3","cf4","cl4","cp4","cc4" )]

# subestting data for demographic information
demoData <- mydata[,c("TurkID","Age","Sex","Sex_3_TEXT","Gender","Gender_TEXT","Masc_1","Fem_1","Race","Race_TEXT","Political","Political_TEXT",
                      "Profession","Profession_TEXT")]

# creating empty lists to store our data 
datalist = list()
timinglist = list()
Demolist = list()


# retieving data in the required format
for(i in 1:nrow(mydata)){
  ################## pairs and response
  practiceData <- mydata1[i,"pair"]
  result <-strsplit(practiceData, "\\;")[[1]]
  my.array <- array(result, dim=c(68,1))
  my.array[65] <- "RABBIS-JEWISH"
  my.array[66] <- "WOMEN-FEMALE"
  my.array[67] <- "UNITED STATES PRESIDENTS-FEMALE"
  my.array[68] <- "POPES-CATHOLIC"
  
  my.array <- as.data.frame(my.array)
  namesArray <- c("pair")
  names(my.array) <- namesArray
  
  # removing hypthens because they cause issues in separate function later
  my.array <- data.frame(lapply(my.array, function(x) {
    gsub("HARD-WORKING", "HARD WORKING", x)
  }))
  my.array <- data.frame(lapply(my.array, function(x) {
    gsub("WELL-GROOMED", "WELL GROOMED", x)
  }))
  my.array <- data.frame(lapply(my.array, function(x) {
    gsub("WELL-DRESSED", "WELL DRESSED", x)
  }))
  my.array <- data.frame(lapply(my.array, function(x) {
    gsub("MID-LEVEL MANAGERS", "MID LEVEL MANAGERS", x)
  }))
  
  
  practice <- mydata1[i,]
  resultArray <- as.data.frame(array(practice,dim=c(68,1)))
  IdArray <- as.data.frame(rep(c(i),times=68))
  df <-separate(my.array, pair, into = c("prof", "ster"), sep = "-")
  MainData <- as.data.frame(cbind(IdArray,my.array,df,resultArray))
  #insertRows(MainDataFrame, i,rcurrent=FALSE)
  names <- c("ID","pair","Profession","Stereotype","Response")
  names(MainData) <- names
  MainData <- unique(MainData)
  datalist[[i]] <- MainData
  
  #Extralist[[i]] <- df
  
  ######## timing 
  
  tryTiming <- timingData[i,]
  
  timing_firstClick <- as.data.frame(c(rbind(tryTiming$qf1,tryTiming$qf2,tryTiming$qf3,tryTiming$qf4,tryTiming$qf5,tryTiming$qf6,tryTiming$qf7,tryTiming$qf8,tryTiming$qf9,tryTiming$qf10,
                                             tryTiming$qf11,tryTiming$qf12,tryTiming$qf13,tryTiming$qf14,tryTiming$qf15,tryTiming$qf16,tryTiming$qf17,tryTiming$qf18,tryTiming$qf19,tryTiming$qf20,
                                             tryTiming$qf21,tryTiming$qf22,tryTiming$qf23,tryTiming$qf24,tryTiming$qf25,tryTiming$qf26,tryTiming$qf27,tryTiming$qf28,tryTiming$qf29,tryTiming$qf30,
                                             tryTiming$qf31,tryTiming$qf32,tryTiming$qf33,tryTiming$qf34,tryTiming$qf35,tryTiming$qf36,tryTiming$qf37,tryTiming$qf38,tryTiming$qf39,tryTiming$qf40,
                                             tryTiming$qf41,tryTiming$qf42,tryTiming$qf43,tryTiming$qf44,tryTiming$qf45,tryTiming$qf46,tryTiming$qf47,tryTiming$qf48,tryTiming$qf49,tryTiming$qf50,
                                             tryTiming$qf51,tryTiming$qf52,tryTiming$qf53,tryTiming$qf54,tryTiming$qf55,tryTiming$qf56,tryTiming$qf57,tryTiming$qf58,tryTiming$qf59,tryTiming$qf60,
                                             tryTiming$qf61,tryTiming$qf62,tryTiming$qf63,tryTiming$qf64,tryTiming$cf1,tryTiming$cf2,tryTiming$cf3,tryTiming$cf4)))
  
  
  
  timing_lastClick <- as.data.frame(c(rbind(tryTiming$ql1,tryTiming$ql2,tryTiming$ql3,tryTiming$ql4,tryTiming$ql5,tryTiming$ql6,tryTiming$ql7,tryTiming$ql8,tryTiming$ql9,tryTiming$ql10,
                                            tryTiming$ql11,tryTiming$ql12,tryTiming$ql13,tryTiming$ql14,tryTiming$ql15,tryTiming$ql16,tryTiming$ql17,tryTiming$ql18,tryTiming$ql19,tryTiming$ql20,
                                            tryTiming$ql21,tryTiming$ql22,tryTiming$ql23,tryTiming$ql24,tryTiming$ql25,tryTiming$ql26,tryTiming$ql27,tryTiming$ql28,tryTiming$ql29,tryTiming$ql30,
                                            tryTiming$ql31,tryTiming$ql32,tryTiming$ql33,tryTiming$ql34,tryTiming$ql35,tryTiming$ql36,tryTiming$ql37,tryTiming$ql38,tryTiming$ql39,tryTiming$ql40,
                                            tryTiming$ql41,tryTiming$ql42,tryTiming$ql43,tryTiming$ql44,tryTiming$ql45,tryTiming$ql46,tryTiming$ql47,tryTiming$ql48,tryTiming$ql49,tryTiming$ql50,
                                            tryTiming$ql51,tryTiming$ql52,tryTiming$ql53,tryTiming$ql54,tryTiming$ql55,tryTiming$ql56,tryTiming$ql57,tryTiming$ql58,tryTiming$ql59,tryTiming$ql60,
                                            tryTiming$ql61,tryTiming$ql62,tryTiming$ql63,tryTiming$ql64,tryTiming$cl1,tryTiming$cl2,tryTiming$cl3,tryTiming$cl4)))
  
  
  timing_pageSubmit <- as.data.frame(c(rbind(tryTiming$qp1,tryTiming$qp2,tryTiming$qp3,tryTiming$qp4,tryTiming$qp5,tryTiming$qp6,tryTiming$qp7,tryTiming$qp8,tryTiming$qp9,tryTiming$qp10,
                                             tryTiming$qp11,tryTiming$qp12,tryTiming$qp13,tryTiming$qp14,tryTiming$qp15,tryTiming$qp16,tryTiming$qp17,tryTiming$qp18,tryTiming$qp19,tryTiming$qp20,
                                             tryTiming$qp21,tryTiming$qp22,tryTiming$qp23,tryTiming$qp24,tryTiming$qp25,tryTiming$qp26,tryTiming$qp27,tryTiming$qp28,tryTiming$qp29,tryTiming$qp30,
                                             tryTiming$qp31,tryTiming$qp32,tryTiming$qp33,tryTiming$qp34,tryTiming$qp35,tryTiming$qp36,tryTiming$qp37,tryTiming$qp38,tryTiming$qp39,tryTiming$qp40,
                                             tryTiming$qp41,tryTiming$qp42,tryTiming$qp43,tryTiming$qp44,tryTiming$qp45,tryTiming$qp46,tryTiming$qp47,tryTiming$qp48,tryTiming$qp49,tryTiming$qp50,
                                             tryTiming$qp51,tryTiming$qp52,tryTiming$qp53,tryTiming$qp54,tryTiming$qp55,tryTiming$qp56,tryTiming$qp57,tryTiming$qp58,tryTiming$qp59,tryTiming$qp60,
                                             tryTiming$qp61,tryTiming$qp62,tryTiming$qp63,tryTiming$qp64,tryTiming$cp1,tryTiming$cp2,tryTiming$cp3,tryTiming$cp4)))
  
  timing_clickCount <- as.data.frame(c(rbind(tryTiming$qc1,tryTiming$qc2,tryTiming$qc3,tryTiming$qc4,tryTiming$qc5,tryTiming$qc6,tryTiming$qc7,tryTiming$qc8,tryTiming$qc9,tryTiming$qc10,
                                             tryTiming$qc11,tryTiming$qc12,tryTiming$qc13,tryTiming$qc14,tryTiming$qc15,tryTiming$qc16,tryTiming$qc17,tryTiming$qc18,tryTiming$qc19,tryTiming$qc20,
                                             tryTiming$qc21,tryTiming$qc22,tryTiming$qc23,tryTiming$qc24,tryTiming$qc25,tryTiming$qc26,tryTiming$qc27,tryTiming$qc28,tryTiming$qc29,tryTiming$qc30,
                                             tryTiming$qc31,tryTiming$qc32,tryTiming$qc33,tryTiming$qc34,tryTiming$qc35,tryTiming$qc36,tryTiming$qc37,tryTiming$qc38,tryTiming$qc39,tryTiming$qc40,
                                             tryTiming$qc41,tryTiming$qc42,tryTiming$qc43,tryTiming$qc44,tryTiming$qc45,tryTiming$qc46,tryTiming$qc47,tryTiming$qc48,tryTiming$qc49,tryTiming$qc50,
                                             tryTiming$qc51,tryTiming$qc52,tryTiming$qc53,tryTiming$qc54,tryTiming$qc55,tryTiming$qc56,tryTiming$qc57,tryTiming$qc58,tryTiming$qc59,tryTiming$qc60,
                                             tryTiming$qc61,tryTiming$qc62,tryTiming$qc63,tryTiming$qc64,tryTiming$cc1,tryTiming$cc2,tryTiming$cc3,tryTiming$cc4)))
  
  totalTimingData <- cbind(timing_firstClick,timing_lastClick,timing_pageSubmit,timing_clickCount)
  names_timing <- c("Timing - First Click","Timing - Last Click"," Timing - Page Submit","Timing - clickCount")
  names(totalTimingData) <- names_timing
  timinglist[[i]] <- totalTimingData
  
  ############## demographic
  tryDemo <- demoData[i,]
  FinalDemoData <- as.data.frame(cbind(rep(c(tryDemo$TurkID),times=68),rep(c(tryDemo$Age),times=68),rep(c(tryDemo$Sex),times=68),rep(c(tryDemo$Sex_3_TEXT),times=68),
                                       rep(c(tryDemo$Gender),times=68),rep(c(tryDemo$Gender_TEXT),times=68),rep(c(tryDemo$Masc_1),times=68),rep(c(tryDemo$Fem_1),times=68),
                                       rep(c(tryDemo$Race),times=68),rep(c(tryDemo$Race_TEXT),times=68),rep(c(tryDemo$Political),times=68),rep(c(tryDemo$Political_TEXT),times=68),
                                       rep(c(tryDemo$Profession),times=68),rep(c(tryDemo$Profession_TEXT),times=68)))
  
  #FinalDemoData <- cbind(turkID,Age,Sex,Sex_3_TEXT,Gender,Gender_TEXT,Masc_1,Fem_1,Race,Race_TEXT,Political,Political_TEXT,Profession,Profession_TEXT)
  namesDemo <- c("TurkID","Age","Sex","Sex_3_TEXT","Gender","Gender_TEXT","Masc_1","Fem_1","Race","Race_TEXT","Political","Political_TEXT",
                 "Actual Profession","Profession_TEXT")
  names(FinalDemoData) <- namesDemo
  Demolist[[i]] <- FinalDemoData
  
  
}

##################### storing in lists
big_data = do.call(rbind, datalist)
big_timingdata = do.call(rbind, timinglist)
big_demoData = do.call(rbind, Demolist)


##### for cluster analysis of professions skip steps 158,159 and 160 and start executing here.

######### there are some blank cells in Turk ID , so replacing them With NA values
big_data <- as.data.frame(apply(big_data,2,as.character))
big_data$converted_res <- recode(big_data$Response, "0%-20%" = 1, "20%-40%" = 2, "40%-60%" = 3, "60%-80%" = 4, "80%-100%" = 5)
big_data$Response[big_data$Response==""] <- "NA"

## removing entries which does not have TurkID
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

big_data <- completeFun(big_data, "Response")
#TotalData$Stereotype <- as.numeric(TotalData$Stereotype)
big_data <- subset(big_data, Profession!="UNITED STATES PRESIDENTS")
big_data <- subset(big_data, Profession!="WOMEN")
big_data <- subset(big_data, Profession!="RABBIS")
big_data <- subset(big_data, Profession!="POPES")

big_data <- big_data[,c("Profession","Stereotype","converted_res")]

prof <- c("ACCOUNTANTS","ACTORS","ADMINISTRATIVE ASSISTANTS/SECRETARY","ADVERTISING PROFESSIONALS","ANTHROPOLOGISTS","ARCHAEOLOGISTS",
          "ART HISTORY PROFESSIONALS","ARTISTS","AUDITORS","BANK TELLERS","BANKERS","BARTENDERS","BIOCHEMISTRY PROFESSIONALS","BIOLOGISTS","CARPENTERS",
          "CASHIERS","CHEMISTS","CHIEF EXECUTIVE OFFICERS (CEO'S)","CHILDCARE WORKERS","COMMUNICATION STUDIES PROFESSIONALS","COMPUTER SCIENTISTS","CONSTRUCTION WORKERS","COOKS/CHEFS",
          "CUSTOMER SERVICE REPRESENTATIVES","DATA SCIENTISTS","DENTAL ASSISTANTS","DENTISTS","DISHWASHERS","DOCTORS","EARTH SCIENCE PROFESSIONALS","ECONOMISTS","ELECTRICIANS",
          "EMERGENCY MEDICAL TECHNICIANS","ENGINEERS","FACTORY WORKERS","FINANCE PROFESSIONALS","FIRE FIGHTERS","FLIGHT ATTENDANTS","FOREIGN LANGUAGE TEACHERS","GARBAGE COLLECTORS",
          "HAIRSTYLISTS/COSMETOLOGISTS","HISTORIANS","HUMAN RESOURCES PROFESSIONALS","INFORMATION TECHNOLOGY PROFESSIONALS","INSURANCE SALES AGENTS","JANITORS/CUSTODIANS","LANDSCAPERS/GROUNDSKEEPERS",
          "LAWYERS","LINGUISTICS PROFESSIONALS","LITERATURE PROFESSIONALS","MAIDS/HOUSECLEANERS","MAIL CARRIERS","MARKETING PROFESSIONALS","MATHEMATICIANS","MECHANICS","MID LEVEL MANAGERS",
          "MUSICAL COMPOSERS","MUSICIANS","NEUROSCIENTISTS","NURSES","OFFICE CLERKS","PHARMACEUTICAL SALESPERSONS","PHARMACISTS","PHILOSOPHERS","PHYSICS PROFESSIONALS","PLUMBERS",
          "POLICE OFFICERS","POLITICAL SCIENTISTS","POLITICIANS","PSYCHOLOGISTS","RECEPTIONISTS","RETAIL SALESPERSONS","SECURITY GUARDS","SINGERS","SOCIAL WORKERS","SOCIOLOGISTS",
          "STATISTICIANS","STOCK BROKERS","TEACHERS/EDUCATORS","TELEMARKETERS","TRUCK DRIVERS","WAITERS")
prof <- as.data.frame(prof)
names(prof) <- c("Profession")

#### storing professions in a table for our reference in future
profession_table = prof

prof[c("ARROGANT","SMART","STUPID","FASHIONABLE","ALOOF","NERDY","LIBERAL","CONSERVATIVE","ATHLETIC","MUSCULAR","GREEDY","SOCIABLE","INTROVERTED","EXTRAVERTED","NEUROTIC","TIMID","CONSIDERATE",
       "RUDE","FAT","THIN","WELL GROOMED","HYGIENIC","MASCULINE","FEMININE","HARD WORKING","LAZY","WHITE","BLACK","HISPANIC","INDIAN","ASIAN","CHRISTIAN","CATHOLIC","MUSLIM","HINDU","BUDDHIST",
       "JEWISH","ATHEIST","AGNOSTIC","ATTRACTIVE","RICH","OUTGOING","WELL DRESSED","ANXIOUS","MEN","WOMEN","GAY","POOR","MIDDLE CLASS","OLD","YOUNG","DISABLED","JERKS","MEAN","KIND","HELPFUL",
       "ENTITLED","FULL OF THEMSELVES","PERVERTS","ANGRY","BORING","BRILLIANT","GENIUSES","KNOWLEDGEABLE","INTERESTING","UGLY","SEXIST","RACIST","HOMOPHOBIC","SELFISH","SELFLESS","INTELLECTUALS",
       "PARTYERS","DUMB","GOOD AT MATH","AMBITIOUS","ASSERTIVE","PHYSICALLY FIT","STRONG","SEXUALLY ACTIVE","SEXUALLY ADEPT","ROMANTIC","COMBATIVE","VIOLENT","GEEKY","LUSTFUL","PRIDEFUL",
       "EASY TO TALK TO","DEPRESSED","HAPPY","BUSY","GOOD WITH COMPUTERS","SOCIALLY AWKWARD","GAMERS","TRUSTWORTHY","HONEST","LIARS")] <- NA

prof_array <- c("ACCOUNTANTS","ACTORS","ADMINISTRATIVE ASSISTANTS/SECRETARY","ADVERTISING PROFESSIONALS","ANTHROPOLOGISTS","ARCHAEOLOGISTS",
                "ART HISTORY PROFESSIONALS","ARTISTS","AUDITORS","BANK TELLERS","BANKERS","BARTENDERS","BIOCHEMISTRY PROFESSIONALS","BIOLOGISTS","CARPENTERS",
                "CASHIERS","CHEMISTS","CHIEF EXECUTIVE OFFICERS (CEO'S)","CHILDCARE WORKERS","COMMUNICATION STUDIES PROFESSIONALS","COMPUTER SCIENTISTS","CONSTRUCTION WORKERS","COOKS/CHEFS",
                "CUSTOMER SERVICE REPRESENTATIVES","DATA SCIENTISTS","DENTAL ASSISTANTS","DENTISTS","DISHWASHERS","DOCTORS","EARTH SCIENCE PROFESSIONALS","ECONOMISTS","ELECTRICIANS",
                "EMERGENCY MEDICAL TECHNICIANS","ENGINEERS","FACTORY WORKERS","FINANCE PROFESSIONALS","FIRE FIGHTERS","FLIGHT ATTENDANTS","FOREIGN LANGUAGE TEACHERS","GARBAGE COLLECTORS",
                "HAIRSTYLISTS/COSMETOLOGISTS","HISTORIANS","HUMAN RESOURCES PROFESSIONALS","INFORMATION TECHNOLOGY PROFESSIONALS","INSURANCE SALES AGENTS","JANITORS/CUSTODIANS","LANDSCAPERS/GROUNDSKEEPERS",
                "LAWYERS","LINGUISTICS PROFESSIONALS","LITERATURE PROFESSIONALS","MAIDS/HOUSECLEANERS","MAIL CARRIERS","MARKETING PROFESSIONALS","MATHEMATICIANS","MECHANICS","MID LEVEL MANAGERS",
                "MUSICAL COMPOSERS","MUSICIANS","NEUROSCIENTISTS","NURSES","OFFICE CLERKS","PHARMACEUTICAL SALESPERSONS","PHARMACISTS","PHILOSOPHERS","PHYSICS PROFESSIONALS","PLUMBERS",
                "POLICE OFFICERS","POLITICAL SCIENTISTS","POLITICIANS","PSYCHOLOGISTS","RECEPTIONISTS","RETAIL SALESPERSONS","SECURITY GUARDS","SINGERS","SOCIAL WORKERS","SOCIOLOGISTS",
                "STATISTICIANS","STOCK BROKERS","TEACHERS/EDUCATORS","TELEMARKETERS","TRUCK DRIVERS","WAITERS")




ster_array <- c("ARROGANT","SMART","STUPID","FASHIONABLE","ALOOF","NERDY","LIBERAL","CONSERVATIVE","ATHLETIC","MUSCULAR","GREEDY","SOCIABLE","INTROVERTED","EXTRAVERTED","NEUROTIC","TIMID","CONSIDERATE",
                "RUDE","FAT","THIN","WELL GROOMED","HYGIENIC","MASCULINE","FEMININE","HARD WORKING","LAZY","WHITE","BLACK","HISPANIC","INDIAN","ASIAN","CHRISTIAN","CATHOLIC","MUSLIM","HINDU","BUDDHIST",
                "JEWISH","ATHEIST","AGNOSTIC","ATTRACTIVE","RICH","OUTGOING","WELL DRESSED","ANXIOUS","MEN","WOMEN","GAY","POOR","MIDDLE CLASS","OLD","YOUNG","DISABLED","JERKS","MEAN","KIND","HELPFUL",
                "ENTITLED","FULL OF THEMSELVES","PERVERTS","ANGRY","BORING","BRILLIANT","GENIUSES","KNOWLEDGEABLE","INTERESTING","UGLY","SEXIST","RACIST","HOMOPHOBIC","SELFISH","SELFLESS","INTELLECTUALS",
                "PARTYERS","DUMB","GOOD AT MATH","AMBITIOUS","ASSERTIVE","PHYSICALLY FIT","STRONG","SEXUALLY ACTIVE","SEXUALLY ADEPT","ROMANTIC","COMBATIVE","VIOLENT","GEEKY","LUSTFUL","PRIDEFUL",
                "EASY TO TALK TO","DEPRESSED","HAPPY","BUSY","GOOD WITH COMPUTERS","SOCIALLY AWKWARD","GAMERS","TRUSTWORTHY","HONEST","LIARS")

prof_data <- as.data.frame(prof_array)
ster_data <- as.data.frame(ster_array)

df <- data.frame(matrix(ncol = 98, nrow = 0))
dfName <- c("Profession","ARROGANT","SMART","STUPID","FASHIONABLE","ALOOF","NERDY","LIBERAL","CONSERVATIVE","ATHLETIC","MUSCULAR","GREEDY","SOCIABLE","INTROVERTED","EXTRAVERTED","NEUROTIC","TIMID","CONSIDERATE",
            "RUDE","FAT","THIN","WELL GROOMED","HYGIENIC","MASCULINE","FEMININE","HARD WORKING","LAZY","WHITE","BLACK","HISPANIC","INDIAN","ASIAN","CHRISTIAN","CATHOLIC","MUSLIM","HINDU","BUDDHIST",
            "JEWISH","ATHEIST","AGNOSTIC","ATTRACTIVE","RICH","OUTGOING","WELL DRESSED","ANXIOUS","MEN","WOMEN","GAY","POOR","MIDDLE CLASS","OLD","YOUNG","DISABLED","JERKS","MEAN","KIND","HELPFUL",
            "ENTITLED","FULL OF THEMSELVES","PERVERTS","ANGRY","BORING","BRILLIANT","GENIUSES","KNOWLEDGEABLE","INTERESTING","UGLY","SEXIST","RACIST","HOMOPHOBIC","SELFISH","SELFLESS","INTELLECTUALS",
            "PARTYERS","DUMB","GOOD AT MATH","AMBITIOUS","ASSERTIVE","PHYSICALLY FIT","STRONG","SEXUALLY ACTIVE","SEXUALLY ADEPT","ROMANTIC","COMBATIVE","VIOLENT","GEEKY","LUSTFUL","PRIDEFUL",
            "EASY TO TALK TO","DEPRESSED","HAPPY","BUSY","GOOD WITH COMPUTERS","SOCIALLY AWKWARD","GAMERS","TRUSTWORTHY","HONEST","LIARS")

colnames(df) <- dfName

##### calculating avg of all responses for a profession and stereotype pair.
for(p in 1:nrow(prof_data)){
  x <- prof_array[p]
  trial <- prof[p,]
  for(q in 1:nrow(ster_data))
  {
    y <- ster_array[q]  
    trialdata <- big_data %>% filter(Profession == x, Stereotype == y)
    value = trialdata$converted_res
    Value_data <- as.data.frame(array(value))
    sumValue <- 0
    n <- 0
    avg <- 0
    for( r in 1:nrow(Value_data))
    {
      sumValue <- sumValue + value[r]
      n <- n+1
      avg <- sumValue/n
      
    }
    trial[,y]<- avg
    trial$Profession <- x
    df <- rbind(df, trial)
  }
  
}  

### selecting columns because each is repeated 97 times and 97th row has complete data of a profession.
OurData <- df[c(97,194,291,388,485,582,679,776,873,970,1067,1164,1261,1358,1455,
                1552,1649,1746,1843,1940,2037,2134,2231,2328,2425,2522,2619,2716,
                2813,2910,3007,3104,3201,3298,3395,3492,3589,3686,3783,3880,3977,
                4074,4171,4268,4365,4462,4559,4656,4753,4850,4947,5044,5141,5238,
                5335,5432,5529,5626,5723,5820,5917,6014,6111,6208,6305,6402,6499,
                6596,6693,6790,6887,6984,7081,7178,7275,7372,7469,7566,7663,7760,
                7857,7954),]
rownames(OurData) <- 1:nrow(OurData)

###### execute this if you want profession names on the plot
#rownames(OurData) <- c("ACCOUNTANTS","ACTORS","ADMINISTRATIVE ASSISTANTS/SECRETARY","ADVERTISING PROFESSIONALS","ANTHROPOLOGISTS","ARCHAEOLOGISTS",
#                       "ART HISTORY PROFESSIONALS","ARTISTS","AUDITORS","BANK TELLERS","BANKERS","BARTENDERS","BIOCHEMISTRY PROFESSIONALS","BIOLOGISTS","CARPENTERS",
#                       "CASHIERS","CHEMISTS","CHIEF EXECUTIVE OFFICERS (CEO'S)","CHILDCARE WORKERS","COMMUNICATION STUDIES PROFESSIONALS","COMPUTER SCIENTISTS","CONSTRUCTION WORKERS","COOKS/CHEFS",
#                       "CUSTOMER SERVICE REPRESENTATIVES","DATA SCIENTISTS","DENTAL ASSISTANTS","DENTISTS","DISHWASHERS","DOCTORS","EARTH SCIENCE PROFESSIONALS","ECONOMISTS","ELECTRICIANS",
#                      "EMERGENCY MEDICAL TECHNICIANS","ENGINEERS","FACTORY WORKERS","FINANCE PROFESSIONALS","FIRE FIGHTERS","FLIGHT ATTENDANTS","FOREIGN LANGUAGE TEACHERS","GARBAGE COLLECTORS",
#                      "HAIRSTYLISTS/COSMETOLOGISTS","HISTORIANS","HUMAN RESOURCES PROFESSIONALS","INFORMATION TECHNOLOGY PROFESSIONALS","INSURANCE SALES AGENTS","JANITORS/CUSTODIANS","LANDSCAPERS/GROUNDSKEEPERS",
#                      "LAWYERS","LINGUISTICS PROFESSIONALS","LITERATURE PROFESSIONALS","MAIDS/HOUSECLEANERS","MAIL CARRIERS","MARKETING PROFESSIONALS","MATHEMATICIANS","MECHANICS","MID LEVEL MANAGERS",
#                      "MUSICAL COMPOSERS","MUSICIANS","NEUROSCIENTISTS","NURSES","OFFICE CLERKS","PHARMACEUTICAL SALESPERSONS","PHARMACISTS","PHILOSOPHERS","PHYSICS PROFESSIONALS","PLUMBERS",
#                      "POLICE OFFICERS","POLITICAL SCIENTISTS","POLITICIANS","PSYCHOLOGISTS","RECEPTIONISTS","RETAIL SALESPERSONS","SECURITY GUARDS","SINGERS","SOCIAL WORKERS","SOCIOLOGISTS",
#                      "STATISTICIANS","STOCK BROKERS","TEACHERS/EDUCATORS","TELEMARKETERS","TRUCK DRIVERS","WAITERS")


#create new dataset

dataNew = OurData

dataNew$Profession <- NULL
#dataNew[is.na(dataNew)] <- 0
dataNew <- as.data.frame(t(na.omit(t(dataNew))))
dataNew <- scale(dataNew)

# k- means clustering
k2 <- kmeans(dataNew,3)

k2
#size

k2$size
#cluster vector
k2$cluster
#check matching
table(OurData$Profession,k2$cluster)
windows()
plot(dataNew, col = k2$cluster)
points(k2$centers, col = 1:2, pch = 8, cex = 2)

#### cluster plot
fviz_cluster(k2, data = dataNew)







