library(readxl)
mydata <- read_excel("mydata.xlsx")
View(mydata)
#import your data

library (lavaan)

BVDC.Model <-
'#Dual Change Score Model for Pay Satisfaction
lpay1 =~ 1*pay_sat1
lpay2 =~ 1*pay_sat2
lpay3 =~ 1*pay_sat3
lpay4 =~ 1*pay_sat4
lpay2 ~ 1*lpay1
lpay3 ~ 1*lpay2
lpay4 ~ 1*lpay3
dpay2 =~ 1*lpay2
dpay3 =~ 1*lpay3
dpay4 =~ 1*lpay4
dpay2 ~pc_x*lpay1
dpay3 ~pc_x*lpay2
dpay4 ~pc_x*lpay3
B0 =~ 1*lpay1
B1 =~ 1*dpay2 + 1*dpay3 + 1*dpay4
B0 ~ 1
B1 ~ 1
B0 ~~ B0
B1 ~~ B1
B0 ~~ B1
pay_sat1 ~~resvar_x*pay_sat1
pay_sat2 ~~resvar_x*pay_sat2
pay_sat3 ~~resvar_x*pay_sat3
pay_sat4 ~~resvar_x*pay_sat4
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
pay_sat4 ~ 0

#Dual Change Score Model for Life Satisfaction
llife1 =~ 1*lif_sat1
llife2 =~ 1*lif_sat2
llife3 =~ 1*lif_sat3
llife4 =~ 1*lif_sat4
llife2 ~ 1*llife1
llife3 ~ 1*llife2
llife4 ~ 1*llife3
dlife2 =~ 1*llife2
dlife3 =~ 1*llife3
dlife4 =~ 1*llife4
dlife2 ~pc_y*llife1
dlife3 ~pc_y*llife2
dlife4 ~pc_y*llife3
C0 =~ 1*llife1
C1 =~ 1*dlife2 + 1*dlife3 + 1*dlife4
C0 ~ 1
C1 ~ 1
C0 ~~ C0
C1 ~~ C1
C0 ~~ C1
lif_sat1 ~~resvar_y*lif_sat1
lif_sat2 ~~resvar_y*lif_sat2
lif_sat3 ~~resvar_y*lif_sat3
lif_sat4 ~~resvar_y*lif_sat4
llife2 ~~ 0*llife2
llife3 ~~ 0*llife3
llife4 ~~ 0*llife4
dlife2 ~~ 0*dlife2
dlife2 ~~ 0*dlife3
dlife2 ~~ 0*dlife4
dlife3 ~~ 0*dlife3
dlife3 ~~ 0*dlife4
dlife4 ~~ 0*dlife4
lif_sat1 ~ 0
lif_sat2 ~ 0
lif_sat3 ~ 0 
lif_sat4 ~ 0
# Intercept and Constant Change Parameter Covariances
B0 ~~ C0
B0 ~~ C1
C0 ~~ B1
C1 ~~ B1

# Residual Covariances
pay_sat1 ~~r_cov*lif_sat1 
pay_sat2 ~~r_cov*lif_sat2
pay_sat3 ~~r_cov*lif_sat3 
pay_sat4 ~~r_cov*lif_sat4 

# Coupling Effects of Pay Satisfaction on Life Satisfaction - constrained to zero to test nested models
dpay2 ~cpl_x*llife1
dpay3 ~cpl_x*llife2
dpay4 ~cpl_x*llife3

# Coupling Effects of Life Satisfaction on Pay Satisfaction - constrained to zero to test nested models
dlife2 ~cpl_y*lpay1
dlife3 ~cpl_y*lpay2
dlife4 ~cpl_y*lpay3'


BVDC.fit <- sem(BVDC.Model, data= mydata, missing = "fiml", estimator ="ml")
summary (BVDC.fit, fit.measures = T)


