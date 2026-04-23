# ===============================
# 📊 TELCO CUSTOMER CHURN ANALYSIS
# 🧹 DATA CLEANING SCRIPT
# ===============================

library(dplyr)
library(readr)

file_path <- "C:/Users/sulh9/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv"

telco <- read_csv(file_path)

head(telco)
str(telco)

colSums(is.na(telco))

telco$TotalCharges <- as.numeric(telco$TotalCharges)

median_value <- median(telco$TotalCharges, na.rm = TRUE)
telco$TotalCharges[is.na(telco$TotalCharges)] <- median_value

telco <- telco %>%
  mutate(
    gender = as.factor(gender),
    SeniorCitizen = as.factor(SeniorCitizen),
    Partner = as.factor(Partner),
    Dependents = as.factor(Dependents),
    PhoneService = as.factor(PhoneService),
    MultipleLines = as.factor(MultipleLines),
    InternetService = as.factor(InternetService),
    OnlineSecurity = as.factor(OnlineSecurity),
    OnlineBackup = as.factor(OnlineBackup),
    DeviceProtection = as.factor(DeviceProtection),
    TechSupport = as.factor(TechSupport),
    StreamingTV = as.factor(StreamingTV),
    StreamingMovies = as.factor(StreamingMovies),
    Contract = as.factor(Contract),
    PaperlessBilling = as.factor(PaperlessBilling),
    PaymentMethod = as.factor(PaymentMethod),
    Churn = as.factor(Churn)
  )

telco <- telco %>%
  distinct()

summary(telco)
table(telco$Churn)

telco <- telco %>%
  mutate(
    ChargeGroup = case_when(
      MonthlyCharges < 35 ~ "Low",
      MonthlyCharges >= 35 & MonthlyCharges < 70 ~ "Medium",
      MonthlyCharges >= 70 ~ "High"
    )
  )

telco <- telco %>%
  mutate(
    TenureGroup = case_when(
      tenure <= 12 ~ "0-1 Year",
      tenure > 12 & tenure <= 24 ~ "1-2 Years",
      tenure > 24 & tenure <= 48 ~ "2-4 Years",
      tenure > 48 ~ "4+ Years"
    )
  )

write_csv(telco, "cleaned_telco_churn.csv")

cat("Data cleaning complete. Cleaned dataset saved as 'cleaned_telco_churn.csv'")
