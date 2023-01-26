library(haven)
library(dplyr)

# Read data.
path = file.path(Long format met NIHSS sens en neglect.sav")
data = read_sav(path)
data <- data[order(data$Number, data$Days),]

#factor FE
data$FE <- factor(data$FE, levels = c(0,1,2), labels = c("none","partial","full"))
#data$PREFERRED_HAND <- factor(data$PREFERRED_HAND, levels = c(1,2,0), labels = c("right", "left", "no clear preference"))

data <- data[complete.cases(data), ]

######################## exclude patients with 1 row############################

#check how many patients with 1 row 
table(table(data$Number)) #15 patients

dat1mes <- lapply(split(data, data$Number), function(x) as.data.frame(x))
iDs <- lapply(dat1mes, function(x) if (dim(x)[1] == 1) print(x$Number))
vecIDS <- do.call(c, iDs)

#patients with more than 1 row
data2 <-  data[!data$Number %in% vecIDS, ]
data2.id <- data2[!duplicated(data2$Number), ]

################################################################################

#select the first and last row of eaah patient
data3 <- data2 %>%                                            
  group_by(Number) %>%                    # Next operations are done per subject.
  arrange(Days) %>%                       # Sort data per subject on day (ascending).
  filter(row_number() %in% c(1, n())) %>%     # Select the first and last row (observation) per subject.
  arrange(Number)                               # Sort data per subject.

# Add column indicating measurement session
data3 <- data3 %>%
  group_by(Number) %>%
  mutate(measurement = c(1,2))                         

# Select subjects with first measurement session < 10 days & last measurement session between 180-220 days.
data3_Days <- data3 %>%
  group_by(Number) %>%                            # Next operations are done per subject.
  filter(Days[1]<4 & Days[2]>160 & Days[2]<230)  # Select subjects with first session <10 days & last session 180-220 days.

# Select data from first measurement.
data_first_measurement <- data3_Days %>%
  filter(measurement==1) %>%
  select(Number, ARAT, FMARM, Days, GENDER, NIHSS, SA, FE)

# Select relevant data from last measurement.
data_last_measurement <- data3_Days %>%
  filter(measurement==2) %>%
  select(Number, ARAT, FMARM, Days, GENDER, NIHSS, SA, FE)%>%
  rename(last_Number = Number,last_ARAT = ARAT, last_FMARM = FMARM, last_Days = Days, last_GENDER = GENDER, last_NIHSS = NIHSS, last_SA = SA, last_FE = FE)

# Add data from last measurement.
data_final <- cbind(data_first_measurement, data_last_measurement)
data_final <- as_tibble(data_final)

####plot to check
plot(data_first_measurement$ARAT ~ data_first_measurement$Days)

plot(data_last_measurement$last_ARAT ~ data_last_measurement$last_Days)

hist(data_first_measurement$Days)
hist(data_last_measurement$last_Days)

#save final data
save(data_final, file = "finaldata.RData")
