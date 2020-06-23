library(readxl)
mydata <- read_excel("mydata.xlsx")
View(mydata)
#import your data

library (lavaan)

CC.Model <- '
# Latent True Scores
lpay1 =~ 1*pay_sat1 	
lpay2 =~ 1*pay_sat2
lpay3 =~ 1*pay_sat3
lpay4 =~ 1*pay_sat4

# Autoregressions 
lpay2 ~ 1*lpay1		
lpay3 ~ 1*lpay2
lpay4 ~ 1*lpay3

# Latent Change Factors
dpay2 =~ 1*lpay2	
dpay3 =~ 1*lpay3
dpay4 =~ 1*lpay4

# Proportional Change Effects - Constrained to Zero
dpay2 ~0*lpay1		
dpay3 ~0*lpay2
dpay4 ~0*lpay3

# Constant Change and Intercept 
B0 =~ 1*lpay1		
B1 =~ 1*dpay2 + 1*dpay3 + 1*dpay4 	
B0 ~ 1			 
B1 ~ 1			
B0 ~~ B0		
B1 ~~ B1		
B0 ~~ B1

# Additional Constraints
pay_sat1 ~~resvar*pay_sat1	 
pay_sat2 ~~resvar*pay_sat2
pay_sat3 ~~resvar*pay_sat3
pay_sat4 ~~resvar*pay_sat4
lpay2 ~~ 0*lpay2
lpay3 ~~ 0*lpay3
lpay4 ~~ 0*lpay4
dpay2 ~~ 0*dpay2
dpay2 ~~ 0*dpay3
dpay2 ~~ 0*dpay4
dpay3 ~~ 0*dpay3
dpay3 ~~ 0*dpay4
dpay4 ~~ 0*dpay4
pay_sat1 ~ 0
pay_sat2 ~ 0
pay_sat3 ~ 0 
pay_sat4 ~ 0'


CC.fit<-sem(CC.Model, data= mydata, missing = "fiml", estimator ="ml")
summary (CC.fit, fit.measures = T)
