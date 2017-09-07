library(dplyr)
library(datapkg)


##################################################################
#
# Processing Script for Substance Treatment Admissions by Drug Type
# Created by Jenna Daly
# On 09/06/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
SA_drug_data <- dir(path_to_raw, recursive=T, pattern = "Drug.csv") 
SA_total_data <- dir(path_to_raw, recursive=T, pattern = "Month.csv") 

drug_df <- read.csv(paste0(path_to_raw, "/", SA_drug_data), stringsAsFactors=F, header=T)
total_df <- read.csv(paste0(path_to_raw, "/", SA_total_data), stringsAsFactors=F, header=T)

total_df$PrimaryDrug <- "Total"
names(total_df)[names(total_df) == "MonthTotal"] <- "AdmCount"

#Merge together indiv drugs and month totals
SA_df <- merge(drug_df, total_df, by = c("FiscalYear", "MonthYear", "Town", "AdmMonth", "FYMonthOrder", "AdmYear", "PrimaryDrug", "AdmCount"), all = T)

#subset data
SA_df <- SA_df[,c('Town', 'AdmMonth', 'AdmYear', 'PrimaryDrug', 'AdmCount')]

#recode month column
month_digits <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
months <- factor(c("January", "February", "March", "April", "May", "June", "July", 
                   "August", "September", "October", "November", "December"))
SA_df$Month <- months[match(SA_df$AdmMonth, month_digits)]

SA_df$AdmMonth <- NULL

SA_df$Month <- factor(SA_df$Month, levels = months)
SA_df <- arrange(SA_df, Month, AdmYear)

#Set NAs to 0 (to distinguish them from the backfilled NAs)
SA_df$AdmCount[is.na(SA_df$AdmCount)] <- 0

#bring in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

#backfill years and months
years <- c("2013",
           "2014",
           "2015",
           "2016")

drugs <- c("Alcohol",
           "Amphetamines",
           "Barbiturates",
           "Benzodiazepines",
           "Cocaine",
           "Cocaine/Crack",
           "Crack",
           "Hallucinogens: LSD, DMS, STP, etc",
           "Heroin",
           "Inhalants",
           "Marijuana, Hashish, THC",
           "Methamphetamines",
           "Non-Prescriptive Methadone",
           "Other",
           "Other Opiates and Synthetics",
           "Other Sedatives or Hypnotics",
           "Other Stimulants",
           "Over-the-Counter",
           "PCP",
           "Tobacco",
           "Tranquilizers", 
           "Total")

backfill <- expand.grid(
  `Town` = unique(fips$`Town`),
  `AdmYear` = years, 
  `Month` = months,
  `PrimaryDrug` = drugs
)

#Set all columns to character
cols.char <- c("Town", "Month", "AdmYear", "PrimaryDrug")
backfill[cols.char] <- sapply(backfill[cols.char],as.character)

backfill <- as.data.frame(backfill)

backfill <- backfill[backfill$Town != "Connecticut",]

SA_df_backfill <- merge(SA_df, backfill, by = c("Town", "AdmYear", "Month", "PrimaryDrug"), all.y=T)

SA_df_backfill_FIPS <- merge(SA_df_backfill, fips, by = "Town", all=T)

#Remove CT
SA_df_backfill_FIPS <- SA_df_backfill_FIPS[SA_df_backfill_FIPS$Town != "Connecticut",]

#Create totals for CT
totals <- SA_df_backfill_FIPS[SA_df_backfill_FIPS$PrimaryDrug == "Total",]

totals <- totals %>% 
  group_by(AdmYear, Month) %>% 
  summarise(AdmCount = sum(AdmCount, na.rm=T))

totals$Town <- "Connecticut"
totals$PrimaryDrug <- "Total"
totals$FIPS <- "09"

totals <- as.data.frame(totals)

#NAs are missing
#0s are suppressed

#setting 0's back to -9999 to account for suppressed values
SA_df_backfill_FIPS$AdmCount[SA_df_backfill_FIPS$AdmCount == 0] <- -9999

#setting NAs back to 0 to account for 0 admissions
SA_df_backfill_FIPS$AdmCount[is.na(SA_df_backfill_FIPS$AdmCount)] <- 0

complete_SA_df <- rbind(SA_df_backfill_FIPS, totals)

#Create filtering columns
complete_SA_df$Variable <- "Treatment Admissions"

complete_SA_df$`Measure Type` <- "Number"

#Select and reorder columns
complete_SA_df <- complete_SA_df %>% 
  select(Town, FIPS, AdmYear, Month, PrimaryDrug, Variable, `Measure Type`, AdmCount) %>%
  rename(Year = AdmYear, Value = AdmCount, Substance = PrimaryDrug) %>%
  arrange(Year, desc(Value))

#Write CSV
write.table(
  complete_SA_df,
  file.path(getwd(), "data", "substance-treatment_admissions_by_drug_type_2013-2016.csv"),
  sep = ",",
  row.names = F
)
