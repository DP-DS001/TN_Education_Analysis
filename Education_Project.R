install.packages(c("maps", "mapdata"))
install.packages("devtools")
devtools::install_github("dkahle/ggmap")
install.packages("tigris")
install.packages("fastDummies")

#Reading in the US Census Data

library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(censusapi)
library("readxl")
readRenviron(".Renviron")
census_key <- Sys.getenv("CENSUS_KEY")
apis <- listCensusApis()
saipe <- getCensus(name = "timeseries/poverty/saipe",
                   vars = c("STABREV",
                            "NAME",
                            "SAEMHI_PT",
                            "SAEPOVRTALL_PT"), 
                   region = "county", 
                   regionin = "state:47",
                   time = 2015)
names(saipe) <- c("year", "state_code", "county_code", "state", "county", 
                  "median_household_income_estimate", 
                  "all_ages_poverty_rate_estimate")
saipe$county_code <- as.numeric(saipe$county_code)

acs_marital_group <- getCensus(name = "acs/acs5", 
                               vintage = 2015, 
                               vars = c("NAME",
                                        "B06008_001E",
                                        "B06008_002E",
                                        "B06008_003E",
                                        "B06008_004E",
                                        "B06008_005E",
                                        "B06008_006E",
                                        "B06008_007E",
                                        "B06008_008E"), 
                               region = "county", 
                               regionin = "state:47")
names(acs_marital_group) <- c("state_code", "county_code", "county_name",
                              "est_count_total",
                              "est_count_total_never_married",
                              "est_count_total_now_married_except_separated",
                              "est_count_total_divorced",
                              "est_count_total_separated",
                              "est_count_total_widowed",
                              "est_count_total_born_in_state_of_residence",
                              "est_count_total_born_in_state_of_residence_never_married")
acs_marital_group$county_code <- as.numeric(acs_marital_group$county_code)

#Reading in TN of Edu datasets, combining datasets

tvaas <- read_csv('data/tvaas.csv')
names(tvaas) <- c("district_number", "district_name","composite", "literacy", "numeracy")
districts <- read_csv('data/districts.csv')
crosswalk <- read_excel('data/data_district_to_county_crosswalk.xls')
names(crosswalk) <- c("county_number", "county_name", "district_number")
graduation <- read_excel('data/data_2015_District-Attendance-and-Graduation.xlsx')
names(graduation) <- c("school_year",
                       "district",
                       "district_name",
                       "k_8_attendance_rate_pct",
                       "k_8_promotion_rate_pct",
                       "state_goal_attendance_rate",
                       "state_goal_promotion_rate",
                       "attendance_rate_pct",
                       "cohort_dropout_pct",
                       "graduation_rate_nclb_pct",
                       "event_dropout_pct",
                       "all_grad_rate",
                       "white_grad_rate",
                       "african_american_grad_rate",
                       "hispanic_grad_rate",
                       "asian_grad_rate",
                       "native_american_grad_rate",
                       "hawaiian_pacisld_grad_rate",
                       "male_grad_rate",
                       "female_grad_rate",
                       "economically_disadvantaged_grad_rate",
                       "students_with_disabilities_grad_rate",
                       "limited_english_proficient_grad_rate")
tn_socioeconomics <- crosswalk %>%
  inner_join(tvaas, by = "district_number") %>%
  inner_join(districts, by = c("district_number" = "system")) %>%
  inner_join(saipe, by = c("county_number" = "county_code")) %>%
  inner_join(acs_marital_group, by = c("county_number" = "county_code")) %>%
  inner_join(graduation, by = c("district_number" = "district"))


saveRDS(tn_socioeconomics, "tn.rds")

#Save as csv file
write.csv(tn_socioeconomics, file = "data/tn_socioeconomics.csv",row.names=FALSE)

#Exploration of data
class(expend_df$meanDrop)
class(bar_plot$median_household_income_estimate)
bar_plot$expenditures <- as.numeric(as.character(bar_plot$expenditures))

#Practicing creating a barplot 
bar_plot <- subset(tn_socioeconomics, tn_socioeconomics$district_name.x %in% c("Oak Ridge City","Manchester City","Davidson County","Shelby County Unified", "Franklin Special School District","Achievement School District"))



ggplot(fish.species, aes(x = Year, y = Rainbow)) +
  geom_line()

expend_df <- tn_socioeconomics %>% 
  group_by(district_name.x) %>% 
  summarize(meanExp = mean(expenditures), 
            meanGrad= mean(grad), meanDrop = mean(dropout))

ggplot(expend_df1, aes(x=district_name.x, y=meanExp)) +
geom_point() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
expand_limits(y = 0) 

expend_df1 <- tn_socioeconomics %>%
  filter(grepl("^[A-H]", district_name.x)) %>% 
  group_by(district_name.x) %>% 
  summarize(meanExp = mean(expenditures), 
          meanGrad= mean(grad), meanDrop = mean(dropout))

expend_df2 <- tn_socioeconomics %>%
  filter(grepl("^[I-Q]", district_name.x)) %>% 
  group_by(district_name.x) %>% 
  summarize(meanExp = mean(expenditures), 
            meanGrad= mean(grad), meanDrop = mean(dropout))

expend_df3 <- tn_socioeconomics %>%
  filter(grepl("^[R-Z]", district_name.x)) %>% 
  group_by(district_name.x) %>% 
  summarize(meanExp = mean(expenditures), 
            meanGrad= mean(grad), meanDrop = mean(dropout))

cor(x, method = c("pearson", "kendall", "spearman"))
cor(x, method = "pearson", use = "complete.obs")


cor(expend_df[2:4], method = "pearson", use = "complete.obs")
res <- cor(expend_df[2:4])
res
round(res, 2)







View(apis)
tvaas <- read_csv('data/tvaas.csv')
crosswalk <- read_excel('data/data_district_to_county_crosswalk.xls')
districts <- read_csv('data/districts.csv')
dis_count_df  <- crosswalk%>%
  inner_join(district, by = "District Number")
dis_count_df %>%
  group_by("District Name") %>% 
  summarise('Numeracy' = n_distinct("District Name")) %>% 
  ggplot(aes(x = Numeracy, y = "District Name", fill = Numeracy)) + geom_col()



