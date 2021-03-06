library(Matching)
library(rgenoud)
library(foreign)
library(parallel)
library(rbounds)

load("/Users/juan/Downloads/maintmp.RData")
original_data <- x

#replication of table 2

#we want to know the treatment effect
original_model <- lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                       d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                       d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                       d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                       d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                       d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                       d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                       d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=original_data)
summary(original_model)

insured <- subset(original_data, d_hinsEMP==1)
obese_i <- subset(insured, d_obese==1)
n_obese_i <- subset(insured, d_obese==0)
not_insured <- subset(original_data, d_hinsEMP==0)
obese_n_i <-subset(not_insured, d_obese==1)
n_obese_n_i <-subset(not_insured, d_obese==0)

te_insured <- mean(predict(original_model, newdata=obese_i))-mean(predict(original_model, newdata=n_obese_i))
te_uninsured <- mean(predict(original_model, newdata=obese_n_i))-mean(predict(original_model, newdata=n_obese_n_i))


#Only working with women
#we are interested in the difference it makes for other ethnicities than b/w

### OTHER RACE WOMEN
other <- original_data[which(original_data$d_race_o==1 & original_data$d_sexf==1), ]

model_other<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                  d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                  d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                  d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                  d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                  d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                  d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                  d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=other)

summary(model_other)

insured_o <- subset(other, d_hinsEMP==1)
obese_i_o <- subset(insured, d_obese==1)
n_obese_i_o <- subset(insured, d_obese==0)
not_insured_o <- subset(other, d_hinsEMP==0)
obese_n_i_o <-subset(not_insured, d_obese==1)
n_obese_n_i_o <-subset(not_insured, d_obese==0)

te_insured_o <- mean(predict(model_other, newdata=obese_i_o))-mean(predict(model_other, newdata=n_obese_i_o))
te_uninsured_o <- mean(predict(model_other, newdata=obese_n_i_o))-mean(predict(model_other, newdata=n_obese_n_i_o))

#subset other to insured
other <- subset(other, d_hinsEMP == 1)

#specify variables to match on
attach(other)

X <-cbind(d_sexf,childany,childf,age,d_urban_res
          ,srvy_yr,AFQTrevised,educ,tenure,NumberEmp,d_race_b,d_race_o
          ,d_marrnever,d_marroth,d_ind_ag,d_ind_for
          ,d_ind_mining,d_ind_const,d_ind_mfrg,d_ind_transp,d_ind_wtrade
          ,d_ind_rtrade,d_ind_finance,d_ind_bus_svc,d_ind_pers_svc
          ,d_ind_entert,d_ind_prof_svc,d_occ_mgmt,d_occ_tech
          ,d_occ_admin,d_occ_svc,d_occ_farming,d_occ_prodxn
          ,d_occ_operators)
detach(other)

#Mahalanobis Matching
Mahal_other <- Match(Tr=other$d_obese,X=X, Weight = 2)
summary(Mahal_other)
MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
               srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
               d_race_b+d_race_o+d_marrnever+d_marroth+
               d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
               d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
               d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
               d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
               d_occ_farming+d_occ_prodxn+d_occ_operators,data=other, match.out = Mahal_other)

#parallelise matching
no_cores <-detectCores()-1

cl <- makeCluster(no_cores)

# Genetic Matching
genout <- GenMatch(Tr=other$d_obese,X=X,pop.size = 200, max.generations = 20, wait.generations = 10,M=1, cluster=cl)
mout <- Match(Tr=other$d_obese,X=X, Weight.matrix = genout)

mb<- MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
                    srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                    d_race_b+d_race_o+d_marrnever+d_marroth+
                    d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                    d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                    d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                    d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                    d_occ_farming+d_occ_prodxn+d_occ_operators,data=other, match.out = mout)

stopCluster(cl)

#Sensitivity Analysis
sens1 <- Match(Y=other$CPS_hourly_rec,Tr=other$d_obese,X=X, Weight.matrix = genout)
summary(sens1)

psens(sens1, Gamma=4, GammaInc = 0.2)










### WHITE WOMEN
white <- original_data[which(original_data$d_race_w==1 & original_data$d_sexf==1), ]

model_white<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                  d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                  d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                  d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                  d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                  d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                  d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                  d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=white)

summary(model_white)

insured_w <- subset(white, d_hinsEMP==1)
obese_i_w <- subset(insured, d_obese==1)
n_obese_i_w <- subset(insured, d_obese==0)
not_insured_w <- subset(white, d_hinsEMP==0)
obese_n_i_w <-subset(not_insured, d_obese==1)
n_obese_n_i_w <-subset(not_insured, d_obese==0)

te_insured_w <- mean(predict(model_white, newdata=obese_i_w))-mean(predict(model_white, newdata=n_obese_i_w))
te_uninsured_w <- mean(predict(model_white, newdata=obese_n_i_w))-mean(predict(model_white, newdata=n_obese_n_i_w))

#subset other to insured
white <- subset(white, d_hinsEMP == 1)

#sample of less participants 
white <- subset(white, white$dish2 < 8)

#specify variables to match on
attach(white)

Xw <-cbind(d_sexf,childany,childf,age,d_urban_res
           ,srvy_yr,AFQTrevised,educ,tenure,NumberEmp,d_race_b,d_race_o
           ,d_marrnever,d_marroth,d_ind_ag,d_ind_for
           ,d_ind_mining,d_ind_const,d_ind_mfrg,d_ind_transp,d_ind_wtrade
           ,d_ind_rtrade,d_ind_finance,d_ind_bus_svc,d_ind_pers_svc
           ,d_ind_entert,d_ind_prof_svc,d_occ_mgmt,d_occ_tech
           ,d_occ_admin,d_occ_svc,d_occ_farming,d_occ_prodxn
           ,d_occ_operators)
detach(white)

#Mahalanobis Matching
Mahal_white <- Match(Tr=white$d_obese,X=Xw, Weight = 2)
summary(Mahal_white)
MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
               srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
               d_race_b+d_race_o+d_marrnever+d_marroth+
               d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
               d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
               d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
               d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
               d_occ_farming+d_occ_prodxn+d_occ_operators,data=white, match.out = Mahal_white)

#parallelise matching
no_cores <-detectCores()-1

cl <- makeCluster(no_cores)

# Genetic Matching
genout_w <- GenMatch(Tr=white$d_obese,X=Xw,pop.size = 200, max.generations = 20, wait.generations = 10,M=1, cluster=cl)
mout_w <- Match(Tr=white$d_obese,X=Xw, Weight.matrix = genout_w)

mb_w<- MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
                      srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                      d_race_b+d_race_o+d_marrnever+d_marroth+
                      d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                      d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                      d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                      d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                      d_occ_farming+d_occ_prodxn+d_occ_operators,data=white, match.out = mout_w)

stopCluster(cl)

#Sensitivity Analysis
sens1_w <- Match(Y=white$CPS_hourly_rec,Tr=white$d_obese,X=Xw, Weight.matrix = genout_w)
summary(sens1_w)

psens(sens1_w, Gamma=4, GammaInc = 0.2)










### BLACK WOMEN
black <- original_data[which(original_data$d_race_b==1 & original_data$d_sexf==1), ]

model_black<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                  d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                  d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                  d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                  d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                  d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                  d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                  d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=black)

summary(model_white)

insured_b <- subset(black, d_hinsEMP==1)
obese_i_b <- subset(insured, d_obese==1)
n_obese_i_b <- subset(insured, d_obese==0)
not_insured_b <- subset(black, d_hinsEMP==0)
obese_n_i_b <-subset(not_insured, d_obese==1)
n_obese_n_i_b <-subset(not_insured, d_obese==0)

te_insured_b <- mean(predict(model_black, newdata=obese_i_b))-mean(predict(model_black, newdata=n_obese_i_b))
te_uninsured_b <- mean(predict(model_black, newdata=obese_n_i_b))-mean(predict(model_black, newdata=n_obese_n_i_b))

#subset other to insured
black <- subset(black, d_hinsEMP == 1)


#specify variables to match on
attach(black)

Xb <-cbind(d_sexf,childany,childf,age,d_urban_res
           ,srvy_yr,AFQTrevised,educ,tenure,NumberEmp,d_race_b,d_race_o
           ,d_marrnever,d_marroth,d_ind_ag,d_ind_for
           ,d_ind_mining,d_ind_const,d_ind_mfrg,d_ind_transp,d_ind_wtrade
           ,d_ind_rtrade,d_ind_finance,d_ind_bus_svc,d_ind_pers_svc
           ,d_ind_entert,d_ind_prof_svc,d_occ_mgmt,d_occ_tech
           ,d_occ_admin,d_occ_svc,d_occ_farming,d_occ_prodxn
           ,d_occ_operators)
detach(black)

#Mahalanobis Matching
Mahal_black <- Match(Tr=black$d_obese,X=Xb, Weight = 2)
summary(Mahal_black)
MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
               srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
               d_race_b+d_race_o+d_marrnever+d_marroth+
               d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
               d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
               d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
               d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
               d_occ_farming+d_occ_prodxn+d_occ_operators,data=black, match.out = Mahal_black)

#parallelise matching
no_cores <-detectCores()-1

cl <- makeCluster(no_cores)

# Genetic Matching
genout_b <- GenMatch(Tr=black$d_obese,X=Xb,pop.size = 200, max.generations = 20, wait.generations = 10,M=1, cluster=cl)
mout_b <- Match(Tr=white$d_obese,X=Xb, Weight.matrix = genout_b)

mb_b<- MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
                      srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                      d_race_b+d_race_o+d_marrnever+d_marroth+
                      d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                      d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                      d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                      d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                      d_occ_farming+d_occ_prodxn+d_occ_operators,data=black, match.out = mout_b)

stopCluster(cl)

#Sensitivity Analysis
sens1_b <- Match(Y=black$CPS_hourly_rec,Tr=black$d_obese,X=Xb, Weight.matrix = genout_b)
summary(sens1_b)

psens(sens1_b, Gamma=4, GammaInc = 0.2)