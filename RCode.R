# R Codes for:
# López Steinmetz, L. C., Fong, S. B., & Godoy, J. C.
# "Mental health changes of college students with and without mental disorder background during the Argentina's lengthy mandatory 
# quarantine  due to the COVID-19 pandemic: A longitudinal study"

####################################################################################
####################################################################################
############################ DATASET CODES #########################################
####################################################################################
####################################################################################

# MENTAL DISORDER HISTORY: No (absence) = 0, Yes (presence) = 1
# SEX: Man = 0, Woman = 1
# AGE: Young (18-24 years old) = 0, Adult (25 years old and more) = 1
# REGION: Most populated = 0, Inside the country = 1
# SUICIDAL BEAHAVIOR HISTORY: No (absence) = 0, Ideation (suicidal ideation) = 1, Yes (suicide attempt = 2)
# LONELINESS: Living alone = 0, Living with somebody (accompanied) = 1
# QUARANTINE SUB-PERIODS (DURATION): Pre quar (0-days duration) = 0, 1 pre ext and 2 pre ext (10-days duration) = 1,
# first ext, second ext, and third ext (50-days duration = 2), fourth ext, fifth ext, and sixth ext (100-days duration = 3)

####################################################################################
####################################################################################
############################### METHOD #############################################
####################################################################################
####################################################################################

# Load the dataset
dataset<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t")

######  Section: METHOD. Sub-tile > Sample and procedure

# Distribution by mental disorder history 
table(dataset$text_mentdishist)
#   no  yes 
# 1187  428 
prop.table(table(dataset$text_mentdishist))*100
#       no      yes 
# 73.49845 26.50155

# Distribution by sex
table(dataset$text_sex)
# man woman 
# 252  1363
prop.table(table(dataset$text_sex))*100
#      man   woman 
# 15.60372 84.39628 

# Age: mean and standard deviation
mean(dataset$age_number)
# mean age = 23.57895
sd(dataset$age_number)
# sd age = 5.852042

# Distribution by categories of age
vector0<-dataset$age_number # To create a vector with the values of the column age_number
dataset<-cbind(dataset, age_categ=vector0) # To add a column with the values of age_number
dataset$age_categ[dataset$age_categ >= "25"] <- "adult"
dataset$age_categ[dataset$age_categ < "25"] <- "young"
table(dataset$age_categ)
# adult young 
#   419   1196
prop.table(table(dataset$age_categ))*100
#    adult    young 
# 25.94427 74.05573

# Distribution by broad quarantine sub-periods
table(dataset$text_prequar.and.quarsubperiod)
# 0. before quarantine              1.first             2.second              3.third 
#                  152                  783                  328                  352 
prop.table(table(dataset$text_prequar.and.quarsubperiod))*100
# 0. before quarantine              1.first             2.second              3.third 
#             9.411765            48.482972            20.309598            21.795666 

# Distribution by suicidal behavior history
table(dataset$text_suic)
#   ideation       no      yes 
#        664      811      140 
prop.table(table(dataset$text_suic))*100
#   ideation        no       yes 
#  41.114551 50.216718  8.668731

# Distribution by loneliness (living alone or accompanied)
table(dataset$text_aloneoraccompanied)
# accompanied       alone 
#        1457         158 
prop.table(table(dataset$text_aloneoraccompanied))*100
# accompanied       alone 
#   90.216718    9.783282 

# Distribution by region of residence
table(dataset$text_two_region)
# INSIDE THE COUNTRY     MOST POPULATED 
#                829                786  
prop.table(table(dataset$text_two_region))*100
# INSIDE THE COUNTRY     MOST POPULATED 
#           51.33127           48.66873 



######  Section: METHOD. Sub-tile > Data analysis

# Testing Skewness and Kurtosis. Criteria: range of acceptable values -1 to +1 for Skewness and -3 to +3 for Kurtosis (Brown, 2006) or near to.
# Reference: Brown, T. A. (2006). Confirmatory factor analysis for applied research. New York: Guilford Press.

library(moments)

# SELF-PERCEIVED HEALTH
# Time 1:
skewness(dataset$AGHQ)
# skewness AGHQ = 0.100017
kurtosis(dataset$AGHQ) 
# kurtosis AGHQ = 2.018091
# Time 2:
skewness(dataset$BGHQ)
# skewness BGHQ = 0.09073812
kurtosis(dataset$BGHQ) 
# kurtosis BGHQ = 1.854873

# PSYCHOLOGICAL DISTRESS
# Time 1:
skewness(dataset$AK10)
# skewness AK10 = 0.133999
kurtosis(dataset$AK10) 
# kurtosis AK10 = 2.174469
# Time 2:
skewness(dataset$BK10)
# skewness BK10 = 0.2277034
kurtosis(dataset$BK10) 
# kurtosis BK10 = 2.300789


################################################
######## To Assess for multicollinearity #######
# To check for multicollinearity, we used the VIF (Variance Inflation Factor) values. We adopted the following criteria (Field et al., 2012):
# If VIF values are less than 10 then that indicates there probably isn't cause for concern.
# If the average of VIF values is not substantially greater than 1, then that also indicates that there's no cause for concern.
# Tolerance below 0.1 and below 0.2 indicates a serious and a potential problem
# Reference: Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. London: SAGE.
# For assessing the assumption of no multicollinearity we used the vif() function, for the initial model:

library(car)

######### WITH MENTAL DISORDER HISTORY #########

with<-subset(dataset, dataset$mentdishist==1)

######### GHQ: Self-perceived health
# First measurement:
fitwithA<-lm(AGHQ ~ sex + age_dichotomus + prequar.and.quarsubperiod + aloneoraccompanied + two_region + suic, data = with)

# The VIF
vif(fitwithA)
#      sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 1.017458                  1.070635                  1.060670                  1.017311                  1.064983                  1.046068 

# The mean VIF
mean(vif(fitwithA))
# [1] 1.046187

# The tolerance
1/vif(fitwithA)
#       sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 0.9828419                 0.9340253                 0.9428004                 0.9829838                 0.9389826                 0.9559605


######### K-10: Psychological distress
# First measurement:
fitwithAK<-lm(AK10 ~ sex + age_dichotomus + prequar.and.quarsubperiod + aloneoraccompanied + two_region + suic, data = with)

# The VIF
vif(fitwithAK)
#      sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 1.017458                  1.070635                  1.060670                  1.017311                  1.064983                  1.046068 

# The mean VIF
mean(vif(fitwithAK))
# [1] 1.046187

# The tolerance
1/vif(fitwithAK)
#       sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 0.9828419                 0.9340253                 0.9428004                 0.9829838                 0.9389826                 0.9559605



####### WITHOUT MENTAL DISORDER HISTORY ########

without<-subset(dataset, dataset$mentdishist==0)

######### GHQ: Self-perceived health
# First measurement:
fitwithoutA<-lm(AGHQ ~ sex + age_dichotomus + prequar.and.quarsubperiod + aloneoraccompanied + two_region + suic, data = without)

# The VIF
vif(fitwithoutA)
#      sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 1.023542                  1.023687                  1.123743                  1.011307                  1.123650                  1.007466  

# The mean VIF
mean(vif(fitwithoutA))
# [1] 1.052232

# The tolerance
1/vif(fitwithoutA)
#       sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 0.9769993                 0.9768612                 0.8898833                 0.9888195                 0.8899567                 0.9925898 


######### K-10: Psychological distress
# First measurement:
fitwithoutAK<-lm(AK10 ~ sex + age_dichotomus + prequar.and.quarsubperiod + aloneoraccompanied + two_region + suic, data = without)

# The VIF
vif(fitwithoutAK)
#      sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 1.023542                  1.023687                  1.123743                  1.011307                  1.123650                  1.007466  

# The mean VIF
mean(vif(fitwithoutAK))
# [1] 1.052232

# The tolerance
1/vif(fitwithoutAK)
#       sex            age_dichotomus prequar.and.quarsubperiod        aloneoraccompanied                two_region                      suic 
# 0.9769993                 0.9768612                 0.8898833                 0.9888195                 0.8899567                 0.9925898 


#################################################################################################
#################################################################################################
###################################### RESULTS ##################################################
#################################################################################################
#################################################################################################

##################################################################################################
################################ DESCRIPTIVE MEASURES ############################################

########### SELF-PERCEIVED HEALTH
# TIME 1: SELF-PERCEIVED HEALTH
vector1<-dataset$AGHQ # To create a vector with the values of the column AGHQ
dataset<-cbind(dataset, dxAGHQ=vector1) # To add a column with the values of AGHQ
dataset$dxAGHQ[dataset$dxAGHQ >= 5] <- "presence common mental disorders" # presence of common mental disorders: cutoff score indicating common mental disorders >= 5
dataset$dxAGHQ[dataset$dxAGHQ < 5] <- "absence common mental disorder" # absence of common mental disorders

prop.table(table(dataset$dxAGHQ[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder               46.71053  0.00000  0.00000  0.00000
# presence common mental disorders             53.28947  0.00000  0.00000  0.00000

prop.table(table(dataset$dxAGHQ[dataset$text_prequar.and.quarsubperiod=="1.first"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="1.first"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder                0.00000 43.42273  0.00000  0.00000
# presence common mental disorders              0.00000 56.57727  0.00000  0.00000

prop.table(table(dataset$dxAGHQ[dataset$text_prequar.and.quarsubperiod=="2.second"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="2.second"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder                0.00000  0.00000 33.84146  0.00000
# presence common mental disorders              0.00000  0.00000 66.15854  0.00000

prop.table(table(dataset$dxAGHQ[dataset$text_prequar.and.quarsubperiod=="3.third"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="3.third"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder                0.00000  0.00000  0.00000 28.40909
# presence common mental disorders              0.00000  0.00000  0.00000 71.59091


# TIME 2: SELF-PERCEIVED HEALTH
vector2<-dataset$BGHQ # To create a vector with the values of the column BGHQ
dataset<-cbind(dataset, dxBGHQ=vector2) # To add a column with the values of BGHQ
dataset$dxBGHQ[dataset$dxBGHQ >= 5] <- "presence common mental disorders" # presence of common mental disorders: cutoff score indicating common mental disorders >= 5
dataset$dxBGHQ[dataset$dxBGHQ < 5] <- "absence common mental disorder" # absence of common mental disorders

prop.table(table(dataset$dxBGHQ[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder               39.47368  0.00000  0.00000  0.00000
# presence common mental disorders             60.52632  0.00000  0.00000  0.00000

prop.table(table(dataset$dxBGHQ[dataset$text_prequar.and.quarsubperiod=="1.first"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="1.first"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder                0.00000 40.48531  0.00000  0.00000
# presence common mental disorders              0.00000 59.51469  0.00000  0.00000

prop.table(table(dataset$dxBGHQ[dataset$text_prequar.and.quarsubperiod=="2.second"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="2.second"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder                0.00000  0.00000 41.76829  0.00000
# presence common mental disorders              0.00000  0.00000 58.23171  0.00000

prop.table(table(dataset$dxBGHQ[dataset$text_prequar.and.quarsubperiod=="3.third"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="3.third"]))*100
#                                  0. before quarantine  1.first 2.second  3.third
# absence common mental disorder                0.00000  0.00000  0.00000 42.32955
# presence common mental disorders              0.00000  0.00000  0.00000 57.67045


########### PSYCHOLOGICAL DISTRESS
# TIME 1: PSYCHOLOGICAL DISTRESS
vector3<-dataset$AK10 # To create a vector with the values of the column AK10
dataset<-cbind(dataset, dxAK10=vector3) # To add a column with the values of AK10
dataset$dxAK10[dataset$dxAK10 > 20] <- "presence depressive or anxiety disorder" # presence depressive or anxiety disorder: cutoff score indicating presence depressive or anxiety disorder > 20
dataset$dxAK10[dataset$dxAK10 <= 20] <- "absence depressive or anxiety disorder" # absence depressive or anxiety disorder

prop.table(table(dataset$dxAK10[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder              28.28947  0.00000  0.00000  0.00000
# presence depressive or anxiety disorder             71.71053  0.00000  0.00000  0.00000 

prop.table(table(dataset$dxAK10[dataset$text_prequar.and.quarsubperiod=="1.first"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="1.first"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder               0.00000 31.28991  0.00000  0.00000
# presence depressive or anxiety disorder              0.00000 68.71009  0.00000  0.00000

prop.table(table(dataset$dxAK10[dataset$text_prequar.and.quarsubperiod=="2.second"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="2.second"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder               0.00000  0.00000 25.60976  0.00000
# presence depressive or anxiety disorder              0.00000  0.00000 74.39024  0.00000

prop.table(table(dataset$dxAK10[dataset$text_prequar.and.quarsubperiod=="3.third"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="3.third"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder               0.00000  0.00000  0.00000 22.72727
# presence depressive or anxiety disorder              0.00000  0.00000  0.00000 77.27273


# TIME 2: PSYCHOLOGICAL DISTRESS
vector4<-dataset$BK10 # To create a vector with the values of the column BK10
dataset<-cbind(dataset, dxBK10=vector4) # To add a column with the values of BK10
dataset$dxBK10[dataset$dxBK10 > 20] <- "presence depressive or anxiety disorder" # presence depressive or anxiety disorder: cutoff score indicating presence depressive or anxiety disorder > 20
dataset$dxBK10[dataset$dxBK10 <= 20] <- "absence depressive or anxiety disorder" # absence depressive or anxiety disorder

prop.table(table(dataset$dxBK10[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="0. before quarantine"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder              25.65789  0.00000  0.00000  0.00000
# presence depressive or anxiety disorder             74.34211  0.00000  0.00000  0.00000

prop.table(table(dataset$dxBK10[dataset$text_prequar.and.quarsubperiod=="1.first"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="1.first"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder               0.00000 25.15964  0.00000  0.00000
# presence depressive or anxiety disorder              0.00000 74.84036  0.00000  0.00000

prop.table(table(dataset$dxBK10[dataset$text_prequar.and.quarsubperiod=="2.second"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="2.second"]))*100
#                                         0. before quarantine  1.first 2.second  3.third
# absence depressive or anxiety disorder               0.00000  0.00000 30.4878  0.00000
# presence depressive or anxiety disorder              0.00000  0.00000 69.5122  0.00000

prop.table(table(dataset$dxBK10[dataset$text_prequar.and.quarsubperiod=="3.third"],dataset$text_prequar.and.quarsubperiod[dataset$text_prequar.and.quarsubperiod=="3.third"]))*100
#                                         0. before quarantine 1.first 2.second 3.third
# absence depressive or anxiety disorder             0.00000  0.00000  0.00000 29.54545
# presence depressive or anxiety disorder            0.00000  0.00000  0.00000 70.45455 




#### Presence or absence of common mental disorders in the gropus with and without mental disorder history:

# During the first measurement
prop.table(table(dataset$dxAGHQ[dataset$text_mentdishist=="yes"],dataset$text_mentdishist[dataset$text_mentdishist=="yes"]))*100
#                                        no      yes
# absence common mental disorder    0.00000 32.94393
# presence common mental disorders  0.00000 67.05607

prop.table(table(dataset$dxAGHQ[dataset$text_mentdishist=="no"],dataset$text_mentdishist[dataset$text_mentdishist=="no"]))*100
#                                        no      yes
# absence common mental disorder   40.52233  0.00000
# presence common mental disorders 59.47767  0.00000


# During the follow-up
prop.table(table(dataset$dxBGHQ[dataset$text_mentdishist=="yes"],dataset$text_mentdishist[dataset$text_mentdishist=="yes"]))*100
#                                        no      yes
# absence common mental disorder    0.00000 32.24299
# presence common mental disorders  0.00000 67.75701

prop.table(table(dataset$dxBGHQ[dataset$text_mentdishist=="no"],dataset$text_mentdishist[dataset$text_mentdishist=="no"]))*100
#                                        no      yes
# absence common mental disorder   44.22915  0.00000
# presence common mental disorders 55.77085  0.00000



#### Presence or absence of any depressive or anxiety disorder in the gropus with and without mental disorder history:

# During the first measurement
prop.table(table(dataset$dxAK10[dataset$text_mentdishist=="yes"],dataset$text_mentdishist[dataset$text_mentdishist=="yes"]))*100
#                                               no      yes
# absence depressive or anxiety disorder   0.00000 16.58879
# presence depressive or anxiety disorder  0.00000 83.41121

prop.table(table(dataset$dxAK10[dataset$text_mentdishist=="no"],dataset$text_mentdishist[dataset$text_mentdishist=="no"]))*100
#                                               no      yes
# absence depressive or anxiety disorder  32.09773  0.00000
# presence depressive or anxiety disorder 67.90227  0.00000


# During the follow-up
prop.table(table(dataset$dxBK10[dataset$text_mentdishist=="yes"],dataset$text_mentdishist[dataset$text_mentdishist=="yes"]))*100
#                                               no      yes
# absence depressive or anxiety disorder   0.00000 16.35514
# presence depressive or anxiety disorder  0.00000 83.64486

prop.table(table(dataset$dxBK10[dataset$text_mentdishist=="no"],dataset$text_mentdishist[dataset$text_mentdishist=="no"]))*100
#                                               no      yes
# absence depressive or anxiety disorder  31.17102  0.00000
# presence depressive or anxiety disorder 68.82898  0.00000






# Descriptive statistics:
library(pastecs)
# SELF-PERCEIVED HEALTH:

by(dataset$AGHQ, list(dataset$text_mentdishist), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$text_mentdishist), stat.desc, basic = FALSE)

by(dataset$AGHQ, list(dataset$text_sex), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$text_sex), stat.desc, basic = FALSE)

by(dataset$AGHQ, list(dataset$age_text), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$age_text), stat.desc, basic = FALSE)

by(dataset$AGHQ, list(dataset$text_prequar.and.quarsubperiod), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$text_prequar.and.quarsubperiod), stat.desc, basic = FALSE)

by(dataset$AGHQ, list(dataset$text_suic), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$text_suic), stat.desc, basic = FALSE)

by(dataset$AGHQ, list(dataset$text_aloneoraccompanied), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$text_aloneoraccompanied), stat.desc, basic = FALSE)

by(dataset$AGHQ, list(dataset$text_two_region), stat.desc, basic = FALSE)
by(dataset$BGHQ, list(dataset$text_two_region), stat.desc, basic = FALSE)


# PSYCHOLOGICAL DISTRESS:
by(dataset$AK10, list(dataset$text_mentdishist), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$text_mentdishist), stat.desc, basic = FALSE)

by(dataset$AK10, list(dataset$text_sex), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$text_sex), stat.desc, basic = FALSE)

by(dataset$AK10, list(dataset$age_text), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$age_text), stat.desc, basic = FALSE)

by(dataset$AK10, list(dataset$text_prequar.and.quarsubperiod), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$text_prequar.and.quarsubperiod), stat.desc, basic = FALSE)

by(dataset$AK10, list(dataset$text_suic), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$text_suic), stat.desc, basic = FALSE)

by(dataset$AK10, list(dataset$text_aloneoraccompanied), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$text_aloneoraccompanied), stat.desc, basic = FALSE)

by(dataset$AK10, list(dataset$text_two_region), stat.desc, basic = FALSE)
by(dataset$BK10, list(dataset$text_two_region), stat.desc, basic = FALSE)

### FIGURE 1
# Figure 1.a.b. SELF-PERCEIVED HEALTH
vector0<-dataset$text_mentdishist # To create a vector with the values of the column text_mentdishist
dataset<-cbind(dataset, MDH=vector0) # To add a column with the values of text_mentdishist
library(ggpubr)
# Color version
p1 <- ggplot(data = dataset, aes(x = AGHQ, fill = MDH)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = AGHQ, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   a)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

p1 <- ggplot(data = dataset, aes(x = BGHQ, fill = MDH)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = BGHQ, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   b)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# Black & white version
p1 <- ggplot(data = dataset, aes(x = AGHQ, fill = MDH)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("black", "gray48")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = AGHQ, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.6, width = 0.15) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   a)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

p1 <- ggplot(data = dataset, aes(x = BGHQ, fill = MDH)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("black", "gray48")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = BGHQ, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.6, width = 0.15) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   b)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

library(dplyr)
# Statistics of the AGHQ in participants with and without mental disorder history
dataset %>% filter(!is.na(AGHQ)) %>% group_by(text_mentdishist) %>%
  summarise(media = mean(AGHQ),
            mediana = median(AGHQ),
            sd = sd (AGHQ),
            min = min(AGHQ),
            max = max(AGHQ))

# Statistics of the BGHQ in participants with and without mental disorder history
dataset %>% filter(!is.na(BGHQ)) %>% group_by(text_mentdishist) %>%
  summarise(media = mean(BGHQ),
            mediana = median(BGHQ),
            sd = sd (BGHQ),
            min = min(BGHQ),
            max = max(BGHQ))


# Figure 1.c.d.PSYCHOLOGICAL DISTRESS
# Color version
p1 <- ggplot(data = dataset, aes(x = AK10, fill = MDH)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = AK10, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   c)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

p1 <- ggplot(data = dataset, aes(x = BK10, fill = MDH)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = BK10, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   d)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# Black & white version
p1 <- ggplot(data = dataset, aes(x = AK10, fill = MDH)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("black", "gray48")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = AK10, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.6, width = 0.15) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   c)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

p1 <- ggplot(data = dataset, aes(x = BK10, fill = MDH)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("black", "gray48")) +
  geom_rug(aes(color = MDH), alpha = 0.5) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = MDH, y = BK10, color = MDH)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.6, width = 0.15) +
  scale_color_manual(values = c("black", "gray48")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   d)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# Statistics of the AK10 in participants with and without mental disorder history
dataset %>% filter(!is.na(AK10)) %>% group_by(text_mentdishist) %>%
  summarise(media = mean(AK10),
            mediana = median(AK10),
            sd = sd (AK10),
            min = min(AK10),
            max = max(AK10))

# Statistics of the BK10 in participants with and without mental disorder history
dataset %>% filter(!is.na(BK10)) %>% group_by(text_mentdishist) %>%
  summarise(media = mean(BK10),
            mediana = median(BK10),
            sd = sd (BK10),
            min = min(BK10),
            max = max(BK10))


t.test(dataset$AGHQ[dataset$text_mentdishist=="yes"], dataset$AGHQ[dataset$text_mentdishist=="no"], alternative = "two.sided", paired=FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)
# Welch Two Sample t-test
# t = 4.3452, df = 720.6, p-value = 1.592e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.4599073 1.2180565
# sample estimates:
#   mean of x mean of y 
# 6.362150  5.523168 

t.test(dataset$AK10[dataset$text_mentdishist=="yes"], dataset$AK10[dataset$text_mentdishist=="no"], alternative = "two.sided", paired=FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)
# Welch Two Sample t-test
# t = 10.123, df = 709.48, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   3.792316 5.617336
# sample estimates:
#   mean of x mean of y 
# 30.13785  25.43302

t.test(dataset$BGHQ[dataset$text_mentdishist=="yes"], dataset$BGHQ[dataset$text_mentdishist=="no"], alternative = "two.sided", paired=FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)
# Welch Two Sample t-test
# t = 4.9125, df = 728.74, p-value = 1.111e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.621662 1.449294
# sample estimates:
#   mean of x mean of y 
# 6.411215  5.375737

t.test(dataset$BK10[dataset$text_mentdishist=="yes"], dataset$BK10[dataset$text_mentdishist=="no"], alternative = "two.sided", paired=FALSE, mu = 0, var.equal = FALSE, conf.level = 0.95)
# Welch Two Sample t-test
# t = 9.2331, df = 690.73, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   3.602552 5.548520
# sample estimates:
#   mean of x mean of y 
# 30.09112  25.51559


###################################################################################################

##################################### MULTILEVEL MODELLING ########################################

# We have followed the steps indicated in Field et al. (2012) for carrying out these analyses. 
# Reference: Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. SAGE: London.


##################################### SELF-PERCEIVED HEALTH #######################################

####### TO ASSESS THE NEED FOR A MULTILEVEL MODEL WITH RANDOM EFFECTS BY MENTAL DISORDER HISTORY

####### Preparing the data

# To select the variables from the dataset:
library(dplyr)
selfperhealth<-select(dataset, CODE.ANALYSIS, sex, age_dichotomus, mentdishist, prequar.and.quarsubperiod, aloneoraccompanied, two_region, suic, AGHQ, BGHQ)

# To convert the format of the dataframe into the long format:
library(reshape)
myDataGHQ<-melt(selfperhealth, id = c("CODE.ANALYSIS","sex", "age_dichotomus","mentdishist", "prequar.and.quarsubperiod", "aloneoraccompanied", "two_region", "suic"), measured = c("AGHQ","BGHQ"))

# To rename variables:
names(myDataGHQ)<-c("participant","sex","age","mentdishist","quar", "aloneoraccompanied","region","suic","variable", "scores") # "variable" refers to the repeated-measures variable

# This creates a variable "period" in the dataframe myDataGHQ:
myDataGHQ$period<-gl(2, 1615, labels = c("first", "second")) # We created 2 sets of 1615 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (first measurement and follow-up).

# To make it clearer that there are two observations for each participant we have sorted the data by participant:
myDataGHQ<-myDataGHQ[order(myDataGHQ$participant),]

####### MIXED DESIGNS AS A GLM

# Setting contrasts for quarantine sub-period:
myDataGHQ$quar<-as.factor(myDataGHQ$quar)
is.factor(myDataGHQ$quar)

basvsfirst<-c(0,1,0,0) # this compares the baseline (prior to quarantine) to the first sub-period of quarantine
basvssecond<-c(0,0,1,0) # this compares the baseline (prior to quarantine) to the second sub-period of quarantine
basvsthird<-c(0,0,0,1) # this compares the baseline (prior to quarantine) to the third sub-period of quarantine
contrasts(myDataGHQ$quar)<-cbind(basvsfirst, basvssecond, basvsthird)
myDataGHQ$quar # To check we setted the contrasts correctly 

# Setting contrasts for suicide attempt history:
myDataGHQ$suic<-as.factor(myDataGHQ$suic)
is.factor(myDataGHQ$suic)

basevsideation<-c(0,1,0) # this compares the baseline (no suicidal attempt nor ideation history) to the ideation history
basevsattempt<-c(0,0,1) # this compares the baseline (no suicidal attempt nor ideation history) to the suicide attempt history
contrasts(myDataGHQ$suic)<-cbind(basevsideation, basevsattempt)
myDataGHQ$suic # To check we setted the contrasts correctly 


# Building the model
library(nlme)

interceptOnly <-gls(scores ~ 1, data = myDataGHQ, method = "ML") # only the intercept

randomInterceptOnly <-lme(scores ~ 1, random = ~1|mentdishist, data = myDataGHQ, method = "ML") # Mental disorder history as a random effect

within<-lme(scores ~ 1, random = ~1|mentdishist/participant, data = myDataGHQ, method = "ML") # the nested structure indicates repeated measures (within variable)

model1<-lme(scores ~ 1, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim")) # repeated measures nested by participants and participants nested by mental disorder history
anova(interceptOnly, randomInterceptOnly, within, model1)  # to compare models

# To see the overall effect of each main effect (additive and interaction) we added them to the model one at a time:

model2<-lme(scores ~ period, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model2) # period is not significantly related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2) 

model3<-lme(scores ~ period + sex, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model3) # sex is positively related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3) #### Adding sex significantly improved the fit of the model

model4<-lme(scores ~ period + sex + age, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model4) # age is not significantly related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4)

model5<-lme(scores ~ period + sex + age + quar, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model5) # quar is positively related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5) #### Adding quar significantly improved the fit of the model

model6<-lme(scores ~ period + sex + age + quar + aloneoraccompanied, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model6) # aloneoraccompanied is not significantly related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6)

model7<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model7) # region is not significantly related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6, model7)

model8<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model8) # suic is positively related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6, model7, model8) #### Adding suic significantly improved the fit of the model

model9<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic + period:quar, random = ~1|mentdishist/participant/period, data = myDataGHQ, method = "ML", control = lmeControl(opt = "optim"))
summary(model9)  # the interaction period:quar is negatively related to self-perceived health
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6, model7, model8, model9) #### Adding the interaction period:quar significantly improved the fit of the model
#                     Model df      AIC      BIC    logLik    Test  L.Ratio p-value
# interceptOnly           1  2 17316.43 17328.59 -8656.215                          
# randomInterceptOnly     2  3 17282.22 17300.46 -8638.111   1 vs 2  36.2085  <.0001
# within                  3  4 16931.58 16955.90 -8461.792   2 vs 3 352.6392  <.0001
# model1                  4  5 16933.58 16963.98 -8461.792   3 vs 4   0.0000  0.9959
# model2                  5  6 16934.51 16970.99 -8461.255   4 vs 5   1.0726  0.3004
# model3                  6  7 16914.53 16957.09 -8450.265   5 vs 6  21.9809  <.0001
# model4                  7  8 16913.12 16961.76 -8448.559   6 vs 7   3.4114  0.0647
# model5                  8 11 16899.26 16966.14 -8438.628   7 vs 8  19.8631  0.0002
# model6                  9 12 16901.25 16974.21 -8438.623   8 vs 9   0.0086  0.9260
# model7                 10 13 16903.16 16982.20 -8438.578  9 vs 10   0.0909  0.7631
# model8                 11 15 16816.80 16908.00 -8393.399 10 vs 11  90.3574  <.0001
# model9                 12 18 16761.34 16870.78 -8362.670 11 vs 12  61.4582  <.0001 #### Final model

summary(model9) # Final model

# Linear mixed-effects model fit by maximum likelihood
#  Data: myDataGHQ 
#        AIC      BIC   logLik
#   16761.34 16870.78 -8362.67

# Random effects:
#   Formula: ~1 | mentdishist
#         (Intercept)
# StdDev:    0.223313

# Formula: ~1 | participant %in% mentdishist
#         (Intercept)
# StdDev:    2.207815

# Formula: ~1 | period %in% participant %in% mentdishist
#         (Intercept) Residual
# StdDev:    2.376047 0.970087

# Fixed effects: scores ~ period + sex + age + quar + aloneoraccompanied + region +      suic + period:quar 
#                                  Value Std.Error   DF   t-value p-value
# (Intercept)                   3.820531 0.4556884 1611  8.384085  0.0000
# periodsecond                  0.717105 0.2950321 1611  2.430601  0.0152 Do not interpret this because the interaction is meaningful
# sex                           0.806532 0.2003323 1604  4.025971  0.0001 #
# age                          -0.299094 0.1655180 1604 -1.807018  0.0709
# quarbasvsfirst                0.145285 0.3110695 1604  0.467049  0.6405
# quarbasvssecond               1.266201 0.3422792 1604  3.699323  0.0002 Do not interpret this because the interaction is meaningful
# quarbasvsthird                1.736142 0.3469071 1604  5.004631  0.0000 Do not interpret this because the interaction is meaningful
# aloneoraccompanied            0.055621 0.2423260 1604  0.229529  0.8185
# region                       -0.089489 0.1537200 1604 -0.582153  0.5605
# suicbasevsideation            1.347046 0.1523417 1604  8.842268  0.0000 #
# suicbasevsattempt             1.750353 0.2728597 1604  6.414847  0.0000 #
# periodsecond:quarbasvsfirst  -0.270107 0.3223994 1611 -0.837801  0.4023
# periodsecond:quarbasvssecond -1.329910 0.3569053 1611 -3.726227  0.0002 #
# periodsecond:quarbasvsthird  -1.887560 0.3530313 1611 -5.346721  0.0000 #
#  Correlation: 
#                              (Intr) prdscn sex    age    qrbsvsf qrbsvss qrbsvst alnrcc region scbsvsd scbsvst prdscnd:qrbsvsf prdscnd:qrbsvss
# periodsecond                 -0.324                                                                                                           
# sex                          -0.315  0.000                                                                                                    
# age                          -0.168  0.000  0.074                                                                                             
# quarbasvsfirst               -0.610  0.474 -0.073  0.006                                                                                      
# quarbasvssecond              -0.525  0.431 -0.085 -0.047  0.768                                                                               
# quarbasvsthird               -0.577  0.425 -0.033 -0.040  0.779   0.707                                                                       
# aloneoraccompanied           -0.481  0.000 -0.057  0.069  0.027  -0.005   0.006                                                               
# region                       -0.383  0.000  0.074  0.063  0.236   0.199   0.300   0.050                                                       
# suicbasevsideation           -0.164  0.000 -0.055  0.060  0.016   0.028   0.037   0.008 -0.024                                                
# suicbasevsattempt            -0.126  0.000 -0.043  0.036  0.027  -0.003   0.026   0.036 -0.019  0.286                                         
# periodsecond:quarbasvsfirst   0.296 -0.915  0.000  0.000 -0.518  -0.394  -0.389   0.000  0.000  0.000   0.000                                 
# periodsecond:quarbasvssecond  0.268 -0.827  0.000  0.000 -0.392  -0.521  -0.352   0.000  0.000  0.000   0.000   0.756                         
# periodsecond:quarbasvsthird   0.271 -0.836  0.000  0.000 -0.396  -0.360  -0.509   0.000  0.000  0.000   0.000   0.765           0.691         

# Standardized Within-Group Residuals:
#          Min           Q1          Med           Q3          Max 
# -0.922369569 -0.232566121 -0.007162531  0.234791249  0.886777012 

# Number of Observations: 3230
# Number of Groups: 
#                    mentdishist             participant %in% mentdishist period %in% participant %in% mentdishist 
#                              2                                     1615                                     3230 



intervals(model9) # 95% CI of the final model
# Approximate 95% confidence intervals

#  Fixed effects:
#                                   lower        est.       upper
# (Intercept)                   2.9286652  3.82053057  4.71239589
# periodsecond                  0.1396737  0.71710526  1.29453687
# sex                           0.4144439  0.80653199  1.19862007
# age                          -0.6230440 -0.29909399  0.02485601
# quarbasvsfirst               -0.4635369  0.14528474  0.75410636
# quarbasvssecond               0.5962965  1.26620150  1.93610650
# quarbasvsthird                1.0571794  1.73614197  2.41510457
# aloneoraccompanied           -0.4186568  0.05562091  0.52989863
# region                       -0.3903477 -0.08948863  0.21137043
# suicbasevsideation            1.0488849  1.34704633  1.64520780
# suicbasevsattempt             1.2163152  1.75035304  2.28439089
# periodsecond:quarbasvsfirst  -0.9011009 -0.27010654  0.36088781
# periodsecond:quarbasvssecond -2.0284388 -1.32991014 -0.63138145
# periodsecond:quarbasvsthird  -2.5785064 -1.88755981 -1.19661321
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: mentdishist 
#                      lower     est.     upper
# sd((Intercept)) 0.06413957 0.223313 0.7775025
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 2.074391 2.207815 2.349821
#   Level: period 
#                    lower     est.    upper
# sd((Intercept)) 2.277804 2.376047 2.478527

#   Within-group standard error:
#     lower      est.     upper 
# 0.8923001 0.9700870 1.0546551 


# Calculating effect sizes
library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# sex
rcontrast(4.025971, 1604)
# [1] "r =  0.100019619217211"

# suicbasevsideation
rcontrast(8.842268, 1604)
# [1] "r =  0.215589053060969"

# suicbasevsattempt
rcontrast(6.414847, 1604)
# [1] "r =  0.158155215050355"

# periodsecond:quarbasvssecond
rcontrast(-3.726227, 1611)
# [1] "r =  0.0924395929989422"

# periodsecond:quarbasvsthird 
rcontrast(-5.346721, 1611)
# [1] "r =  0.13204447466703"


# Descriptive statistics by group
library(pastecs)
by(myDataGHQ$scores, list(myDataGHQ$period, myDataGHQ$quar), stat.desc, basic = FALSE) # variables included in the interaction


##### TO RUN THE FINAL MODEL IN EACH GROUP, SEPARATELY:

# Group WITH mental disorder history
with<-subset(myDataGHQ, myDataGHQ$mentdishist==1)
with9<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic + period:quar, random = ~1|participant/period, data = with, method = "ML", control = lmeControl(opt = "optim"))
summary(with9) 
# Linear mixed-effects model fit by maximum likelihood
# Data: with 
#        AIC      BIC    logLik
#   4494.065 4574.854 -2230.032

# Random effects:
#  Formula: ~1 | participant
#         (Intercept)
# StdDev:    2.500349

#  Formula: ~1 | period %in% participant
#         (Intercept)  Residual
# StdDev:    2.280304 0.9806433

# Fixed effects: scores ~ period + sex + age + quar + aloneoraccompanied + region +      suic + period:quar 
#                                  Value Std.Error  DF   t-value p-value
# (Intercept)                   4.271062 0.9312189 424  4.586528  0.0000
# periodsecond                  0.285714 0.5982783 424  0.477561  0.6332
# sex                           0.709800 0.4766106 418  1.489265  0.1372
# age                          -0.151465 0.3405124 418 -0.444814  0.6567
# quarbasvsfirst               -0.359263 0.6699528 418 -0.536252  0.5921
# quarbasvssecond               0.418473 0.7208755 418  0.580507  0.5619
# quarbasvsthird                0.795056 0.7456718 418  1.066228  0.2869
# aloneoraccompanied            0.413773 0.4363405 418  0.948279  0.3435
# region                        0.431457 0.3179743 418  1.356894  0.1755
# suicbasevsideation            1.037116 0.3482828 418  2.977797  0.0031 #
# suicbasevsattempt             1.782629 0.4294523 418  4.150937  0.0000 #
# periodsecond:quarbasvsfirst   0.308756 0.6447237 424  0.478896  0.6323
# periodsecond:quarbasvssecond -0.600932 0.7029285 424 -0.854897  0.3931 the interaction is NOT meaningful for the group with mental disorder history
# periodsecond:quarbasvsthird  -1.345238 0.7120936 424 -1.889131  0.0596 the interaction is NOT meaningful for the group with mental disorder history
# Correlation: 
#                              (Intr) prdscn sex    age    qrbsvsf qrbsvss qrbsvst alnrcc region scbsvsd scbsvst prdscnd:qrbsvsf prdscnd:qrbsvss
# periodsecond                 -0.321                                                                                                           
# sex                          -0.430  0.000                                                                                                    
# age                          -0.257  0.000  0.089                                                                                             
# quarbasvsfirst               -0.670  0.447 -0.031  0.047                                                                                      
# quarbasvssecond              -0.584  0.415 -0.040 -0.022  0.797                                                                               
# quarbasvsthird               -0.617  0.401 -0.023 -0.014  0.793   0.728                                                                       
# aloneoraccompanied           -0.427  0.000 -0.042  0.096  0.023  -0.002   0.019                                                               
# region                       -0.387  0.000  0.055  0.080  0.252   0.194   0.276   0.068                                                       
# suicbasevsideation           -0.253  0.000 -0.002  0.161  0.014   0.007   0.026   0.014 -0.043                                                
# suicbasevsattempt            -0.215  0.000 -0.045  0.175  0.020  -0.013   0.020   0.069 -0.046  0.509                                         
# periodsecond:quarbasvsfirst   0.298 -0.928  0.000  0.000 -0.481  -0.385  -0.372   0.000  0.000  0.000   0.000                                 
# periodsecond:quarbasvssecond  0.273 -0.851  0.000  0.000 -0.380  -0.488  -0.341   0.000  0.000  0.000   0.000   0.790                         
# periodsecond:quarbasvsthird   0.270 -0.840  0.000  0.000 -0.375  -0.349  -0.477   0.000  0.000  0.000   0.000   0.780           0.715         

# Standardized Within-Group Residuals:
#          Min           Q1          Med           Q3          Max 
# -0.966252344 -0.235480464  0.007167624  0.239579661  0.893473390 

# Number of Observations: 856
# Number of Groups: 
#            participant period %in% participant 
#                    428                     856 


intervals(with9)
# Approximate 95% confidence intervals
# Fixed effects:
#                                   lower       est.      upper
# (Intercept)                   2.4557111  4.2710617 6.08641234
# periodsecond                 -0.8805902  0.2857143 1.45201874
# sex                          -0.2193600  0.7097996 1.63895914
# age                          -0.8152988 -0.1514648 0.51236919
# quarbasvsfirst               -1.6653462 -0.3592633 0.94681958
# quarbasvssecond              -0.9868846  0.4184730 1.82383061
# quarbasvsthird               -0.6586419  0.7950564 2.24875471
# aloneoraccompanied           -0.4368796  0.4137727 1.26442497
# region                       -0.1884383  0.4314573 1.05135293
# suicbasevsideation            0.3581330  1.0371156 1.71609814
# suicbasevsattempt             0.9454056  1.7826292 2.61985279
# periodsecond:quarbasvsfirst  -0.9480910  0.3087558 1.56560254
# periodsecond:quarbasvssecond -1.9712450 -0.6009317 0.76938165
# periodsecond:quarbasvsthird  -2.7334181 -1.3452381 0.04294187
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 2.250462 2.500349 2.777982
#   Level: period 
#                    lower     est.    upper
# sd((Intercept)) 2.068796 2.280304 2.513436

#   Within-group standard error:
#     lower      est.     upper 
# 0.7229075 0.9806433 1.3302689 



# Calculating effect sizes
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# suicbasevsideation
rcontrast(2.977797, 418)
# [1] "r =  0.144128098582957"

# suicbasevsattempt
rcontrast(4.150937, 418)
# [1] "r =  0.198969538079797"


# Descriptive statistics:
by(with$scores, list(with$period, with$quar), stat.desc, basic = FALSE)
# : first
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 6.0000000    6.5428571    0.5959850    1.2111872   12.4319328    3.5258946    0.5388922 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 7.0000000    6.8285714    0.6616579    1.3446505   15.3226891    3.9144207    0.5732415 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 6.0000000    5.9308756    0.2251454    0.4437633   10.9998293    3.3165991    0.5592090 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 6.0000000    6.5253456    0.2466757    0.4861997   13.2042157    3.6337606    0.5568687 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 7.0000000    6.7934783    0.3746437    0.7441836   12.9129240    3.5934557    0.5289567 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 7.0000000    6.4782609    0.4281870    0.8505408   16.8676541    4.1070250    0.6339703 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 7.0000000    6.9285714    0.3953203    0.7862763   13.1273666    3.6231708    0.5229319 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 6.0000000    5.8690476    0.4067629    0.8090350   13.8983075    3.7280434    0.6352041 


# Group WITHOUT mental disorder history
without<-subset(myDataGHQ, myDataGHQ$mentdishist==0)
without9<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic + period:quar, random = ~1|participant/period, data = without, method = "ML", control = lmeControl(opt = "optim"))
summary(without9) # the interaction is meaningful for the group without mental disorder history
# Linear mixed-effects model fit by maximum likelihood
# Data: without 
#      AIC      BIC    logLik
# 12271.86 12369.99 -6118.929

# Random effects:
#   Formula: ~1 | participant
#         (Intercept)
# StdDev:    2.069668

# Formula: ~1 | period %in% participant
#         (Intercept)  Residual
# StdDev:    2.410171 0.9595218

# Fixed effects: scores ~ period + sex + age + quar + aloneoraccompanied + region +      suic + period:quar 
#                                  Value Std.Error   DF   t-value p-value
# (Intercept)                   3.678646 0.4806454 1183  7.653555  0.0000
# periodsecond                  0.846154 0.3401741 1183  2.487414  0.0130 Do not interpret this because the interaction is meaningful
# sex                           0.805354 0.2181344 1177  3.692009  0.0002 #
# age                          -0.355021 0.1894518 1177 -1.873940  0.0612
# quarbasvsfirst                0.278678 0.3495717 1177  0.797198  0.4255
# quarbasvssecond               1.534189 0.3877490 1177  3.956655  0.0001 Do not interpret this because the interaction is meaningful
# quarbasvsthird                2.001746 0.3899107 1177  5.133857  0.0000 Do not interpret this because the interaction is meaningful
# aloneoraccompanied           -0.108009 0.2932923 1177 -0.368262  0.7127
# region                       -0.267040 0.1745567 1177 -1.529821  0.1263
# suicbasevsideation            1.432716 0.1685888 1177  8.498289  0.0000 #
# suicbasevsattempt             1.364617 0.4079599 1177  3.344978  0.0008 #
# periodsecond:quarbasvsfirst  -0.455694 0.3736830 1183 -1.219468  0.2229
# periodsecond:quarbasvssecond -1.574967 0.4160376 1183 -3.785637  0.0002 #
# periodsecond:quarbasvsthird  -2.051378 0.4077220 1183 -5.031314  0.0000 #
# Correlation: 
#                              (Intr) prdscn sex    age    qrbsvsf qrbsvss qrbsvst alnrcc region scbsvsd scbsvst prdscnd:qrbsvsf prdscnd:qrbsvss
# periodsecond                 -0.354                                                                                                           
# sex                          -0.300  0.000                                                                                                    
# age                          -0.149  0.000  0.073                                                                                             
# quarbasvsfirst               -0.640  0.487 -0.083 -0.010                                                                                      
# quarbasvssecond              -0.548  0.439 -0.097 -0.055  0.757                                                                               
# quarbasvsthird               -0.610  0.436 -0.034 -0.050  0.774   0.700                                                                       
# aloneoraccompanied           -0.557  0.000 -0.066  0.058  0.028  -0.007  -0.001                                                               
# region                       -0.412  0.000  0.081  0.055  0.229   0.200   0.308   0.041                                                       
# suicbasevsideation           -0.139  0.000 -0.068  0.034  0.018   0.037   0.041   0.006 -0.018                                                
# suicbasevsattempt            -0.065  0.000 -0.021 -0.037  0.038   0.001   0.033   0.000 -0.004  0.165                                         
# periodsecond:quarbasvsfirst   0.322 -0.910  0.000  0.000 -0.534  -0.399  -0.397   0.000  0.000  0.000   0.000                                 
# periodsecond:quarbasvssecond  0.289 -0.818  0.000  0.000 -0.398  -0.536  -0.357   0.000  0.000  0.000   0.000   0.744                         
# periodsecond:quarbasvsthird   0.295 -0.834  0.000  0.000 -0.406  -0.366  -0.523   0.000  0.000  0.000   0.000   0.760           0.682         

# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -0.86940579 -0.23116185 -0.01384139  0.23152613  0.86956828 

# Number of Observations: 2374
# Number of Groups: 
#             participant period %in% participant 
#                    1187                    2374 


intervals(without9)
# Approximate 95% confidence intervals
# Fixed effects:
#                                   lower       est.       upper
# (Intercept)                   2.7384181  3.6786459  4.61887375
# periodsecond                  0.1807129  0.8461538  1.51159478
# sex                           0.3786423  0.8053542  1.23206607
# age                          -0.7256247 -0.3550213  0.01558207
# quarbasvsfirst               -0.4051499  0.2786781  0.96250604
# quarbasvssecond               0.7756793  1.5341893  2.29269923
# quarbasvsthird                1.2390072  2.0017459  2.76448454
# aloneoraccompanied           -0.6817434 -0.1080085  0.46572637
# region                       -0.6085062 -0.2670405  0.07442523
# suicbasevsideation            1.1029247  1.4327160  1.76250728
# suicbasevsattempt             0.5665707  1.3646169  2.16266315
# periodsecond:quarbasvsfirst  -1.1866848 -0.4556945  0.27529588
# periodsecond:quarbasvssecond -2.3888108 -1.5749674 -0.76112397
# periodsecond:quarbasvsthird  -2.8489545 -2.0513777 -1.25380098
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 1.913662 2.069668 2.238393
#   Level: period 
#                    lower     est.    upper
# sd((Intercept)) 2.286423 2.410171 2.540616

#   Within-group standard error:
#     lower      est.     upper 
# 0.8206896 0.9595218 1.1218396 



# Calculating effect sizes
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# sex
rcontrast(3.692009, 1177)
# [1] "r =  0.106997633103728"

# suicbasevsideation
rcontrast(8.498289, 1177)
# [1] "r =  0.240442852973165"

# suicbasevsattempt
rcontrast(3.344978, 1177)
# [1] "r =  0.0970399411209478"

# periodsecond:quarbasvssecond
rcontrast(-3.785637, 1183)
# [1] "r =  0.109403658594736"

# periodsecond:quarbasvsthird 
rcontrast(-5.031314, 1183)
# [1] "r =  0.144740975986287"


# Descriptive statistics: 
by(without$scores, list(without$period, without$quar), stat.desc, basic = FALSE)
# : first
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 4.0000000    4.5128205    0.2861411    0.5667385    9.5795756    3.0950889    0.6858436 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 5.0000000    5.3589744    0.3373895    0.6682423   13.3183024    3.6494249    0.6809932 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 5.0000000    4.9681979    0.1329137    0.2610653    9.9989868    3.1621175    0.6364717 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 5.0000000    5.3586572    0.1482482    0.2911849   12.4392789    3.5269362    0.6581754 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 6.0000000    6.1737288    0.2225916    0.4385299   11.6930941    3.4195167    0.5538819 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 5.0000000    5.4449153    0.2517262    0.4959284   14.9543996    3.8670919    0.7102208 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 7.0000000    6.5634328    0.1917116    0.3774588    9.8498938    3.1384540    0.4781726 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 5.0000000    5.3582090    0.2212763    0.4356685   13.1221421    3.6224497    0.6760561


##################################### PSYCHOLOGICAL DISTRESS ######################################

####### TO ASSESS THE NEED FOR A MULTILEVEL MODEL WITH RANDOM EFFECTS BY MENTAL DISORDER HISTORY

####### Preparing the data

# To select the variables from the dataset:
library(dplyr)
psychdistress<-select(dataset, CODE.ANALYSIS, sex, age_dichotomus, mentdishist, prequar.and.quarsubperiod, aloneoraccompanied, two_region, suic, AK10, BK10)

# To convert the format of the dataframe into the long format:
library(reshape)
myDataK10<-melt(psychdistress, id = c("CODE.ANALYSIS","sex", "age_dichotomus","mentdishist", "prequar.and.quarsubperiod", "aloneoraccompanied", "two_region", "suic"), measured = c("AK10","BK10"))

# To rename variables:
names(myDataK10)<-c("participant","sex","age","mentdishist","quar", "aloneoraccompanied","region","suic","variable", "scores") # "variable" refers to the repeated-measures variable

# This creates a variable "period" in the dataframe myDataK10:
myDataK10$period<-gl(2, 1615, labels = c("first", "second")) # We created 2 sets of 1615 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (first measurement and follow-up).

# To make it clearer that there are two observations for each participant we have sorted the data by participant:
myDataK10<-myDataK10[order(myDataK10$participant),]

####### MIXED DESIGNS AS A GLM

# Setting contrasts for quarantine sub-periods:
myDataK10$quar<-as.factor(myDataK10$quar)
is.factor(myDataK10$quar)

basvsfirst<-c(0,1,0,0) # this compares the baseline (prior to quarantine) to the first sub-period of quarantine
basvssecond<-c(0,0,1,0) # this compares the baseline (prior to quarantine) to the second sub-period of quarantine
basvsthird<-c(0,0,0,1) # this compares the baseline (prior to quarantine) to the third sub-period of quarantine
contrasts(myDataK10$quar)<-cbind(basvsfirst, basvssecond, basvsthird)
myDataK10$quar # To check we setted the contrasts correctly 


# Setting contrasts for suicide attempt history:
myDataK10$suic<-as.factor(myDataK10$suic)
is.factor(myDataK10$suic)

basevsideation<-c(0,1,0) # this compares the baseline (no suicidal attempt nor ideation history) to the ideation history
basevsattempt<-c(0,0,1) # this compares the baseline (no suicidal attempt nor ideation history) to the suicide attempt history
contrasts(myDataK10$suic)<-cbind(basevsideation, basevsattempt)
myDataK10$suic # To check we setted the contrasts correctly 


# Building the model
library(nlme)

interceptOnly <-gls(scores ~ 1, data = myDataK10, method = "ML") # only the intercept

randomInterceptOnly <-lme(scores ~ 1, random = ~1|mentdishist, data = myDataK10, method = "ML") # Mental disorder history as a random effect

within<-lme(scores ~ 1, random = ~1|mentdishist/participant, data = myDataK10, method = "ML") # the nested structure indicates repeated measures (within variable)

model1<-lme(scores ~ 1, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim")) # repeated measures nested by participants and participants nested by mental disorder history
anova(interceptOnly, randomInterceptOnly, within, model1)  # to compare models

# To see the overall effect of each main effect (additive and interaction) we added them to the model one at a time:

model2<-lme(scores ~ period, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model2) # period is not significantly related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2) 

model3<-lme(scores ~ period + sex, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model3) # sex is positively related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3) #### Adding sex significantly improved the fit of the model

model4<-lme(scores ~ period + sex + age, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model4) # age is negatively related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4) #### Adding age significantly improved the fit of the model

model5<-lme(scores ~ period + sex + age + quar, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model5) # quar is not significantly related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5)

model6<-lme(scores ~ period + sex + age + quar + aloneoraccompanied, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model6) # aloneoraccompanied is not significantly related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6)

model7<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model7) # region is not significantly related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6, model7)

model8<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model8) # suic is positively related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6, model7, model8) #### Adding suic significantly improved the fit of the model

model9<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic + period:quar, random = ~1|mentdishist/participant/period, data = myDataK10, method = "ML", control = lmeControl(opt = "optim"))
summary(model9)  # the interaction period:quar is negatively related to psychological distress
anova(interceptOnly, randomInterceptOnly, within, model1, model2, model3, model4, model5, model6, model7, model8, model9) #### Adding the interaction period:quar significantly improved the fit of the model
#                     Model df      AIC      BIC    logLik   Test   L.Ratio p-value
# interceptOnly           1  2 22928.03 22940.20 -11462.02                          
# randomInterceptOnly     2  3 22744.19 22762.43 -11369.10   1 vs 2 185.8437  <.0001
# within                  3  4 21839.81 21864.13 -10915.90   2 vs 3 906.3830  <.0001
# model1                  4  5 21841.81 21872.21 -10915.90   3 vs 4   0.0000  1.0000
# model2                  5  6 21843.73 21880.21 -10915.86   4 vs 5   0.0820  0.7746
# model3                  6  7 21807.01 21849.57 -10896.50   5 vs 6  38.7205  <.0001
# model4                  7  8 21791.80 21840.44 -10887.90   6 vs 7  17.2072  <.0001
# model5                  8 11 21790.86 21857.75 -10884.43   7 vs 8   6.9349  0.0740
# model6                  9 12 21792.84 21865.81 -10884.42   8 vs 9   0.0197  0.8884
# model7                 10 13 21793.97 21873.01 -10883.99  9 vs 10   0.8739  0.3499
# model8                 11 15 21625.01 21716.22 -10797.51 10 vs 11 172.9584  <.0001
# model9                 12 18 21601.10 21710.54 -10782.55 11 vs 12  29.9124  <.0001 #### Final model


summary(model9) # Final model
# Linear mixed-effects model fit by maximum likelihood
#  Data: myDataK10 
#        AIC      BIC    logLik
#    21601.1 21710.54 -10782.55

# Random effects:
#  Formula: ~1 | mentdishist
#         (Intercept)
# StdDev:    1.569874

# Formula: ~1 | participant %in% mentdishist
#         (Intercept)
# StdDev:    6.033566

# Formula: ~1 | period %in% participant %in% mentdishist
#         (Intercept) Residual
# StdDev:    4.300127 2.011835

# Fixed effects: scores ~ period + sex + age + quar + aloneoraccompanied + region +      suic + period:quar 
#                                  Value Std.Error   DF   t-value p-value
# (Intercept)                  23.172031 1.5015367 1611 15.432211  0.0000
# periodsecond                  0.335526 0.5457574 1611  0.614790  0.5388
# sex                           2.656104 0.4841425 1604  5.486203  0.0000 #
# age                          -1.499848 0.4000284 1604 -3.749353  0.0002 #
# quarbasvsfirst               -0.751294 0.7085821 1604 -1.060278  0.2892
# quarbasvssecond               0.363493 0.7790541 1604  0.466582  0.6409
# quarbasvsthird                1.724538 0.7919469 1604  2.177593  0.0296 Do not interpret this because interaction is meaningful
# aloneoraccompanied            0.197859 0.5857125 1604  0.337809  0.7356
# region                       -0.505652 0.3713891 1604 -1.361516  0.1735
# suicbasevsideation            4.522845 0.3689337 1604 12.259237  0.0000 #
# suicbasevsattempt             5.977262 0.6646821 1604  8.992663  0.0000 #
# periodsecond:quarbasvsfirst   0.538037 0.5963821 1611  0.902168  0.3671
# periodsecond:quarbasvssecond -0.875160 0.6602119 1611 -1.325575  0.1852
# periodsecond:quarbasvsthird  -1.699163 0.6530457 1611 -2.601905  0.0094 #
# Correlation: 
#                              (Intr) prdscn sex    age    qrbsvsf qrbsvss qrbsvst alnrcc region scbsvsd scbsvst prdscnd:qrbsvsf prdscnd:qrbsvss
# periodsecond                 -0.182                                                                                                           
# sex                          -0.232  0.000                                                                                                    
# age                          -0.124  0.000  0.075                                                                                             
# quarbasvsfirst               -0.425  0.385 -0.077  0.006                                                                                      
# quarbasvssecond              -0.364  0.350 -0.090 -0.050  0.769                                                                               
# quarbasvsthird               -0.404  0.345 -0.035 -0.042  0.781   0.709                                                                       
# aloneoraccompanied           -0.351  0.000 -0.058  0.069  0.028  -0.006   0.006                                                               
# region                       -0.281  0.000  0.074  0.063  0.250   0.211   0.318   0.050                                                       
# suicbasevsideation           -0.122  0.000 -0.054  0.061  0.018   0.030   0.040   0.006 -0.025                                                
# suicbasevsattempt            -0.097  0.000 -0.039  0.039  0.030  -0.003   0.027   0.032 -0.020  0.292                                         
# periodsecond:quarbasvsfirst   0.166 -0.915  0.000  0.000 -0.421  -0.321  -0.315   0.000  0.000  0.000   0.000                                 
# periodsecond:quarbasvssecond  0.150 -0.827  0.000  0.000 -0.318  -0.424  -0.285   0.000  0.000  0.000   0.000   0.756                         
# periodsecond:quarbasvsthird   0.152 -0.836  0.000  0.000 -0.322  -0.293  -0.412   0.000  0.000  0.000   0.000   0.765           0.691         

# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -1.29626002 -0.21907968 -0.01152405  0.21501987  1.38013675 

# Number of Observations: 3230
# Number of Groups: 
#                      mentdishist             participant %in% mentdishist period %in% participant %in% mentdishist 
#                                2                                     1615                                     3230 


intervals(model9)
# Approximate 95% confidence intervals
# Fixed effects:
#                                   lower       est.      upper
# (Intercept)                  20.2332502 23.1720311 26.1108120
# periodsecond                 -0.7326204  0.3355263  1.4036730
# sex                           1.7085458  2.6561039  3.6036620
# age                          -2.2827786 -1.4998476 -0.7169166
# quarbasvsfirst               -2.1381230 -0.7512943  0.6355345
# quarbasvssecond              -1.1612632  0.3634926  1.8882485
# quarbasvsthird                0.1745488  1.7245383  3.2745278
# aloneoraccompanied           -0.9484906  0.1978592  1.3442091
# region                       -1.2325308 -0.5056523  0.2212261
# suicbasevsideation            3.8007726  4.5228454  5.2449182
# suicbasevsattempt             4.6763535  5.9772617  7.2781698
# periodsecond:quarbasvsfirst  -0.6291914  0.5380369  1.7052652
# periodsecond:quarbasvssecond -2.1673154 -0.8751605  0.4169945
# periodsecond:quarbasvsthird  -2.9772921 -1.6991627 -0.4210332
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: mentdishist 
#                     lower     est.    upper
# sd((Intercept)) 0.6363159 1.569874 3.873082
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 5.759851 6.033566 6.320287
#   Level: period 
#                    lower     est.   upper
# sd((Intercept)) 3.347616 4.300127 5.52366

#   Within-group standard error:
#     lower      est.     upper 
# 0.6491493 2.0118346 6.2350503



# Calculating effect sizes
library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# sex
rcontrast(5.486203, 1604)
# [1] "r =  0.135716536479114"

# age
rcontrast(-3.749353, 1604)
# [1] "r =  0.0932093191612123"

# suicbasevsideation
rcontrast(12.259237, 1604)
# [1] "r =  0.292693369534297"

# suicbasevsattempt
rcontrast(8.992663, 1604)
# [1] "r =  0.219081335208577"

# periodsecond:quarbasvsthird 
rcontrast(-2.601905, 1611)
# [1] "r =  0.0646893904645056"


# Descriptive statistics by group
library(pastecs)
by(myDataK10$scores, list(myDataK10$period, myDataK10$quar), stat.desc, basic = FALSE) # variables included in the interaction


##### TO RUN THE FINAL MODEL IN EACH GROUP, SEPARATELY:

# Group WITH mental disorder history
with<-subset(myDataK10, myDataK10$mentdishist==1)
with9<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic + period:quar, random = ~1|participant/period, data = with, method = "ML", control = lmeControl(opt = "optim"))
summary(with9) 
# Linear mixed-effects model fit by maximum likelihood
#  Data: with 
#        AIC      BIC    logLik
#   5844.212 5925.001 -2905.106

# Random effects:
#   Formula: ~1 | participant
#         (Intercept)
# StdDev:    6.604564

# Formula: ~1 | period %in% participant
#         (Intercept) Residual
# StdDev:    4.441562 2.113258

# Fixed effects: scores ~ period + sex + age + quar + aloneoraccompanied + region +      suic + period:quar 
#                                  Value Std.Error  DF   t-value p-value
# (Intercept)                  26.101925 2.2347971 424 11.679774  0.0000
# periodsecond                 -0.314286 1.1855221 424 -0.265103  0.7911
# sex                           1.588117 1.1645526 418  1.363714  0.1734
# age                          -1.553189 0.8320096 418 -1.866792  0.0626
# quarbasvsfirst               -1.649576 1.5707594 418 -1.050177  0.2942
# quarbasvssecond              -1.292967 1.6882089 418 -0.765881  0.4442
# quarbasvsthird               -0.165626 1.7494371 418 -0.094674  0.9246
# aloneoraccompanied            1.020781 1.0661563 418  0.957440  0.3389
# region                       -0.099252 0.7769398 418 -0.127748  0.8984
# suicbasevsideation            4.296094 0.8509959 418  5.048314  0.0000 #
# suicbasevsattempt             6.428728 1.0493257 418  6.126533  0.0000 #
# periodsecond:quarbasvsfirst   1.102304 1.2775563 424  0.862822  0.3887
# periodsecond:quarbasvssecond -0.055280 1.3928925 424 -0.039687  0.9684 
# periodsecond:quarbasvsthird  -1.423810 1.4110536 424 -1.009040  0.3135 the interaction is NOT meaningful for the group with mental disorder history
# Correlation: 
#                              (Intr) prdscn sex    age    qrbsvsf qrbsvss qrbsvst alnrcc region scbsvsd scbsvst prdscnd:qrbsvsf prdscnd:qrbsvss
# periodsecond                 -0.265                                                                                                           
# sex                          -0.438  0.000                                                                                                    
# age                          -0.262  0.000  0.089                                                                                             
# quarbasvsfirst               -0.659  0.377 -0.033  0.049                                                                                      
# quarbasvssecond              -0.571  0.351 -0.041 -0.023  0.798                                                                               
# quarbasvsthird               -0.608  0.339 -0.024 -0.015  0.794   0.729                                                                       
# aloneoraccompanied           -0.435  0.000 -0.042  0.096  0.024  -0.002   0.019                                                               
# region                       -0.394  0.000  0.055  0.080  0.262   0.203   0.288   0.068                                                       
# suicbasevsideation           -0.257  0.000 -0.002  0.161  0.015   0.007   0.027   0.014 -0.043                                                
# suicbasevsattempt            -0.219  0.000 -0.045  0.175  0.021  -0.013   0.021   0.069 -0.046  0.509                                         
# periodsecond:quarbasvsfirst   0.246 -0.928  0.000  0.000 -0.407  -0.326  -0.314   0.000  0.000  0.000   0.000                                 
# periodsecond:quarbasvssecond  0.226 -0.851  0.000  0.000 -0.321  -0.413  -0.288   0.000  0.000  0.000   0.000   0.790                         
# periodsecond:quarbasvsthird   0.223 -0.840  0.000  0.000 -0.317  -0.295  -0.403   0.000  0.000  0.000   0.000   0.780           0.715         

# Standardized Within-Group Residuals:
#          Min           Q1          Med           Q3          Max 
# -1.233291628 -0.209385494  0.008568727  0.209335006  1.187309460 

# Number of Observations: 856
# Number of Groups: 
#             participant period %in% participant 
#                     428                     856 


intervals(with9)
# Approximate 95% confidence intervals
# Fixed effects:
#                                   lower       est.        upper
# (Intercept)                  21.7453343 26.10192544 30.45851662
# periodsecond                 -2.6253838 -0.31428571  1.99681236
# sex                          -0.6821957  1.58811702  3.85842972
# age                          -3.1752041 -1.55318905  0.06882601
# quarbasvsfirst               -4.7117948 -1.64957597  1.41264290
# quarbasvssecond              -4.5841551 -1.29296667  1.99822175
# quarbasvsthird               -3.5761801 -0.16562650  3.24492715
# aloneoraccompanied           -1.0577068  1.02078081  3.09926846
# region                       -1.6139082 -0.09925243  1.41540337
# suicbasevsideation            2.6370649  4.29609405  5.95512320
# suicbasevsattempt             4.3830520  6.42872802  8.47440403
# periodsecond:quarbasvsfirst  -1.3882087  1.10230415  3.59281695
# periodsecond:quarbasvssecond -2.7706327 -0.05527950  2.66007366
# periodsecond:quarbasvsthird  -4.1745664 -1.42380952  1.32694739
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 6.050853 6.604564 7.208946
#   Level: period 
#                     lower     est.    upper
# sd((Intercept)) 0.4862105 4.441562 40.57394

#   Within-group standard error:
#        lower         est.        upper 
# 1.210985e-04 2.113258e+00 3.687788e+04


# Calculating effect sizes
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# suicbasevsideation
rcontrast(5.048314, 418)
# [1] "r =  0.23972131971478"

# suicbasevsattempt
rcontrast(6.126533, 418)
# [1] "r =  0.287047774890676"


# Descriptive statistics:
by(with$scores, list(with$period, with$quar), stat.desc, basic = FALSE)
# : first
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 31.0000000   31.4285714    1.2861811    2.6138344   57.8991597    7.6091497    0.2421093 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 31.0000000   31.1142857    1.3693963    2.7829480   65.6336134    8.1014575    0.2603774 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 31.0000000   29.7281106    0.5570951    1.0980387   67.3470302    8.2065236    0.2760526 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 31.0000000   30.5161290    0.6031258    1.1887655   78.9360812    8.8845980    0.2911443 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 31.0000000   30.0978261    0.9438437    1.8748295   81.9573579    9.0530303    0.3007869 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 30.0000000   29.7282609    0.9995344    1.9854524   91.9143574    9.5871976    0.3224944 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 32.0000000   30.7023810    0.9285705    1.8468894   72.4284280    8.5104893    0.2771931 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 30.0000000   28.9642857    0.9963281    1.9816565   83.3842513    9.1314978    0.3152675 



# Group WITHOUT mental disorder history
without<-subset(myDataK10, myDataK10$mentdishist==0)
without9<-lme(scores ~ period + sex + age + quar + aloneoraccompanied + region + suic + period:quar, random = ~1|participant/period, data = without, method = "ML", control = lmeControl(opt = "optim"))
summary(without9) # the interaction is meaningful for the group without mental disorder history
# Linear mixed-effects model fit by maximum likelihood
#  Data: without 
#        AIC      BIC    logLik
#   15760.85 15858.98 -7863.423

# Random effects:
#   Formula: ~1 | participant
#         (Intercept)
# StdDev:    5.780584

# Formula: ~1 | period %in% participant
#         (Intercept) Residual
# StdDev:    4.251307 1.962379

# Fixed effects: scores ~ period + sex + age + quar + aloneoraccompanied + region +      suic + period:quar 
#                                  Value Std.Error   DF   t-value p-value
# (Intercept)                  21.502808 1.1255341 1183 19.104538  0.0000
# periodsecond                  0.529915 0.6140047 1183  0.863046  0.3883
# sex                           2.914079 0.5254385 1177  5.545993  0.0000 #
# age                          -1.450144 0.4563484 1177 -3.177713  0.0015 #
# quarbasvsfirst               -0.558419 0.7875369 1177 -0.709070  0.4784
# quarbasvssecond               0.865748 0.8730781 1177  0.991604  0.3216
# quarbasvsthird                2.277937 0.8811223 1177  2.585267  0.0098 Do not interpret this because interaction is meaningful
# aloneoraccompanied           -0.235060 0.7064777 1177 -0.332722  0.7394
# region                       -0.624502 0.4204693 1177 -1.485250  0.1377
# suicbasevsideation            4.591128 0.4060938 1177 11.305586  0.0000 #
# suicbasevsattempt             5.093538 0.9826870 1177  5.183276  0.0000 #
# periodsecond:quarbasvsfirst   0.376446 0.6744874 1183  0.558121  0.5769
# periodsecond:quarbasvssecond -1.135847 0.7509362 1183 -1.512574  0.1307
# periodsecond:quarbasvsthird  -1.776183 0.7359269 1183 -2.413532  0.0159 #
# Correlation: 
#                              (Intr) prdscn sex    age    qrbsvsf qrbsvss qrbsvst alnrcc region scbsvsd scbsvst prdscnd:qrbsvsf prdscnd:qrbsvss
# periodsecond                 -0.273                                                                                                           
# sex                          -0.309  0.000                                                                                                    
# age                          -0.154  0.000  0.073                                                                                             
# quarbasvsfirst               -0.621  0.390 -0.089 -0.011                                                                                      
# quarbasvssecond              -0.528  0.352 -0.103 -0.059  0.759                                                                               
# quarbasvsthird               -0.595  0.348 -0.037 -0.053  0.776   0.703                                                                       
# aloneoraccompanied           -0.573  0.000 -0.066  0.058  0.030  -0.008  -0.001                                                               
# region                       -0.424  0.000  0.081  0.055  0.245   0.214   0.328   0.041                                                       
# suicbasevsideation           -0.143  0.000 -0.068  0.034  0.019   0.039   0.044   0.006 -0.018                                                
# suicbasevsattempt            -0.067  0.000 -0.021 -0.037  0.041   0.001   0.035   0.000 -0.004  0.165                                         
# periodsecond:quarbasvsfirst   0.248 -0.910  0.000  0.000 -0.428  -0.320  -0.317   0.000  0.000  0.000   0.000                                 
# periodsecond:quarbasvssecond  0.223 -0.818  0.000  0.000 -0.319  -0.430  -0.285   0.000  0.000  0.000   0.000   0.744                         
# periodsecond:quarbasvsthird   0.228 -0.834  0.000  0.000 -0.325  -0.293  -0.418   0.000  0.000  0.000   0.000   0.760           0.682         

# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -1.26403273 -0.22526299 -0.02173415  0.21630904  1.38587673 

# Number of Observations: 2374
# Number of Groups: 
#             participant period %in% participant 
#                    1187                    2374


intervals(without9)
# Approximate 95% confidence intervals
# Fixed effects:
#                                   lower       est.      upper
# (Intercept)                  19.3010635 21.5028081 23.7045526
# periodsecond                 -0.6711878  0.5299145  1.7310168
# sex                           1.8862221  2.9140786  3.9419351
# age                          -2.3428473 -1.4501441 -0.5574409
# quarbasvsfirst               -2.0989892 -0.5584191  0.9821510
# quarbasvssecond              -0.8421569  0.8657479  2.5736526
# quarbasvsthird                0.5542959  2.2779366  4.0015773
# aloneoraccompanied           -1.6170634 -0.2350603  1.1469428
# region                       -1.4470189 -0.6245020  0.1980149
# suicbasevsideation            3.7967325  4.5911282  5.3855240
# suicbasevsattempt             3.1712177  5.0935381  7.0158585
# periodsecond:quarbasvsfirst  -0.9429713  0.3764459  1.6958631
# periodsecond:quarbasvssecond -2.6048114 -1.1358467  0.3331179
# periodsecond:quarbasvsthird  -3.2157869 -1.7761832 -0.3365794
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: participant 
#                   lower     est.    upper
# sd((Intercept)) 5.47093 5.780584 6.107765
#   Level: period 
#                    lower     est.    upper
# sd((Intercept)) 1.985217 4.251307 9.104098

#   Within-group standard error:
#      lower        est.       upper 
# 0.05496099  1.96237888 70.06662387 


# Calculating effect sizes
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# sex
rcontrast(5.545993, 1177)
# [1] "r =  0.159584000815676"

# age
rcontrast(-3.177713, 1177)
# [1] "r =  0.0922298303214902"

# suicbasevsideation
rcontrast(11.305586, 1177)
# [1] "r =  0.312981303870357"

# suicbasevsattempt
rcontrast(5.183276, 1177)
# [1] "r =  0.149387822556588"

# periodsecond:quarbasvsthird 
rcontrast(-2.413532, 1183)
# [1] "r =  0.0699993603807256"

# Descriptive statistics: 
by(without$scores, list(without$period, without$quar), stat.desc, basic = FALSE)
# : first
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 24.0000000   24.7264957    0.7372678    1.4602518   63.5969643    7.9747705    0.3225192 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 0
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 25.0000000   25.2564103    0.7271577    1.4402274   61.8647215    7.8654130    0.3114224 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 24.0000000   24.6448763    0.3210007    0.6305004   58.3214516    7.6368483    0.3098757 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 1
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 25.0000000   25.5512367    0.3256930    0.6397169   60.0389631    7.7484813    0.3032527 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 25.0000000   25.9237288    0.5148468    1.0143048   62.5558601    7.9092263    0.3050960 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 2
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 24.0000000   25.3177966    0.5689585    1.1209108   76.3964479    8.7405062    0.3452317 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : first
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 27.0000000   26.9738806    0.4737921    0.9328439   60.1603639    7.7563112    0.2875490 
# ------------------------------------------------------------------------------------------------------------------------------ 
# : second
# : 3
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 25.0000000   25.7276119    0.5135418    1.0111065   70.6783358    8.4070408    0.3267711 


####################################################################################
####################################################################################
############################### THE END ############################################
####################################################################################
####################################################################################