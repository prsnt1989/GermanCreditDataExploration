
library(dplyr)
library(dummies)

url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data'

df <- read.table(url, sep=' ', header = 0)

german_credit <- data.frame(Class = df$V21)
german_credit$Class <- 'Good'
german_credit$Class[df$V21 == 2] <- 'Bad'

german_credit$CheckingAccountStatus <- df$V1
levels(german_credit$CheckingAccountStatus) <- c('lt.0'
                                                 ,'0.to.200'
                                                 ,'gt.200'
                                                 ,'none'
)

german_credit$Duration <- df$V2


german_credit$CreditHistory <- df$V3
levels(german_credit$CreditHistory) <- c(
  'NoCredit.AllPaid'
  ,'ThisBank.AllPaid'
  ,'PaidDuly'
  ,'Delay'
  ,'Critical'
)


german_credit$Purpose <- df$V4

levels(german_credit$Purpose) <- c(
  'NewCar'
  ,'UsedCar'
  ,'Others'
  ,'Furniture.Equipment'
  ,'Radio.Television' 
  ,'DomesticAppliance'
  ,'Repairs' 
  ,'Education'
  ,'Retraining' 
  ,'Business'
)

german_credit$Amount <- df$V5

german_credit$SavingsAccountBonds <- df$V6
levels(german_credit$SavingsAccountBonds) <- c(
  'lt.100'
  ,'100.to.500'
  ,'500.to.1000'
  ,'gt.1000'
  ,'Unknown'
)

german_credit$EmploymentDuration <- df$V7
levels(german_credit$EmploymentDuration) <- c(
  'Unemployed'
  ,'0.to.1'
  ,'1.to.4'
  ,'4.to.7'
  ,'gt.7'
)

german_credit$InstallmentRatePercentage <- df$V8

german_credit$Personal <- df$V9
levels(german_credit$Personal) <- c(
  'Male.Divorced.Seperated'
  ,'Female.NotSingle'
  ,'Male.Single'
  ,'Male.Married.Widowed'
)

german_credit$OtherDebtorsGuarantors <- df$V10
levels(german_credit$OtherDebtorsGuarantors) <- c(
  'None'
  ,'CoApplicant'
  ,'Guarantor'
)

german_credit$ResidenceDuration <- df$V11

german_credit$Property <- df$V12
levels(german_credit$Property) <- c(
  'RealEstate'
  ,'Insurance'
  ,'CarOther'
  ,'Unknown'
)

german_credit$Age <- df$V13

german_credit$OtherInstallmentPlans <- df$V14
levels(german_credit$OtherInstallmentPlans) <- c(
  'Bank'
  ,'Stores'
  ,'None'
)

german_credit$Housing <- df$V15
levels(german_credit$Housing) <- c('Rent', 'Own', 'ForFree')

german_credit$NumberExistingCredits <- df$V16

german_credit$Job <- df$V17
levels(german_credit$Job) <- c(
  'UnemployedUnskilled'
  ,'UnskilledResident'
  ,'SkilledEmployee'
  ,'Management.SelfEmp.HighlyQualified'
)

german_credit$NumberPeopleMaintenance <- df$V18

german_credit$Telephone <- df$V19
levels(german_credit$Telephone) <- c(
  0
  ,1
)

german_credit$ForeignWorker <- df$V20
levels(german_credit$ForeignWorker) <- c(1, 0)

credit_dataset <- german_credit

#save(german_credit, file = 'german_credit')
#write.csv(german_credit, 'german_credit_full.csv',
#          row.names = FALSE)

final_german_credit <- german_credit

final_german_credit$Duration <- cut(final_german_credit$Duration, c(0,12,24,36,48,60,72,84),labels = c('lt.1','1.to.2','2.to.3','3.to.4','4.to.5','5.to.6','6.to.7'))
final_german_credit$Amount <- cut(final_german_credit$Amount, c(0,500,1000,1500,2500,5000,7500,10000,15000,20000,30000),labels = c('0.to.500','500.to.1000','1000.to.1500','1500.to.2500','2500.to.5000','5000.to.7500','7500.to.10000','10000.to.15000','15000.to.20000','20000.to.30000'))

final_german_credit$Age <- cut(final_german_credit$Age, c(0,25,40,60,65,100),labels = c('Student','Young','Adult','MidSenior','Senior'))

final_german_credit$InstallmentRatePercentage <- as.factor(final_german_credit$InstallmentRatePercentage)
final_german_credit$ResidenceDuration <- as.factor(final_german_credit$ResidenceDuration)
final_german_credit$NumberExistingCredits <- as.factor(final_german_credit$NumberExistingCredits)
final_german_credit$InstallmentRatePercentage <- as.factor(final_german_credit$InstallmentRatePercentage)



credit_dataset_withoutclass <- final_german_credit %>% select(c(-Class))
ml_credit_dataset <- dummy.data.frame(credit_dataset_withoutclass, sep = ".")
ml_credit_dataset$Class <- final_german_credit$Class


#save(ml_credit_dataset, file = 'ml_credit_dataset')
#write.csv(ml_credit_dataset, 'ml_credit_dataset_full.csv',
#          row.names = FALSE)




























































                                                 
                                              