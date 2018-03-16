# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Developing a digital marketplace for family planning: results of a pilot randomized encouragement trial
# Eric Green, eric.green@duke.edu
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# setup
# =============================================================================
# load packages 
# -----------------------------------------------------------------------------
  library(tidyverse)
  library(arm)
  library(AER)
  library(ivpack)
  library(stargazer)
  library(magrittr)
  library(xtable)
  
# load data
# -----------------------------------------------------------------------------
  dat <- read.csv("data and replication files/input/dat.csv", 
                  stringsAsFactors = FALSE)
  
# helper functions
# -----------------------------------------------------------------------------
# rounding functions to keep trailing zeros
  rd0 <- function(y) sprintf("%.0f", round(y, 0))
  rd1 <- function(y) sprintf("%.1f", round(y, 1))
  rd2 <- function(y) sprintf("%.2f", round(y, 2))
  rd3 <- function(y) sprintf("%.3f", round(y, 3))

# ELSE / TRUE
  ELSE <- TRUE

# function to convert Yes to 1  
  yes1 <- function(x) {
    if_else(x=="Yes", 1, 0)
  }

# mean and sd format
  msd = function(df_col, rd=1) {
    paste0(sprintf(paste0("%.", rd, "f"), 
                   round(mean(df_col, na.rm=TRUE), rd)), 
           " (", 
           sprintf(paste0("%.", rd, "f"), 
                   round(sd(df_col, na.rm=TRUE), rd)), 
           ")")
  }

# percentage format
  per = function(df_col, rd=1) {
    sprintf(paste0("%.", rd, "f"),
            round(mean(df_col, na.rm=TRUE)*100, rd))
  }

  
# codebook (see resources folder for full survey codebook)
# =============================================================================  
# sid: participant ID
# trt: assignment to treatment (encouragement)
# tried: participant tried investigational intervention (1 yes, 0 no)
# found: participant completed follow-up survey (1 yes, 0 no)
# textit: participant completed follow-up survey via SMS (1 yes, 0 no)
# textit_locf: NA replaced with 0 
# anyFP: participant reported trying modern method since recruitment (1 yes, 0 no)
# anyFP_locf: NA replace with 0
  
  
# summarize baseline controls and participant characteristics
# =============================================================================
# construct variables
# -----------------------------------------------------------------------------
  dat <-
  dat %>%
    # baseline variables used to block randomization
      mutate(age = New8,
             postSec = case_when(DHSRB105=="University" ~ 1,
                                 DHSRB105=="College_(Middle_Level)" ~ 1,
                                 ELSE ~ 0),
             discontinued = case_when(DHSC313=="Yes" ~ 1,
                                      ELSE ~ 0),
             marriedUnion = case_when(DHSMSA601=="No,_Not_In_Union" ~ 0,
                                      ELSE ~ 1)
             ) %>%
    # other participant characteristics measured at baseline
      mutate(nulligravida = case_when(DHSR201=="No" ~ 1,
                                      ELSE ~ 0),
             childrenEverBorn = if_else(DHSR209A=="Yes", DHSR209, DHSR209B),
             desiredFertility = DHSFP712A,
             spacing = case_when(DHSFP704=="No_More/None" ~ 0,
                                 ELSE ~ 1),
             highestEdu_none = case_when(DHSRB104=="No" ~ 1,
                                         ELSE ~ 0),
             christian = case_when(DHSRB113=="Roman_Catholic" ~ 1,
                                   DHSRB113=="Protestant/Other_Christian" ~ 1,
                                   ELSE ~ 0),
             luhya = case_when(DHSRB114=="Luhya" ~ 1,
                               ELSE ~ 0)
             ) %>%
    # convert variables from Yes/No to 1/0 where Yes==1
      mutate_at(.vars = vars(DHSFP714:DHSFP715E, DHSC30101:DHSC30112), 
                .funs = funs(yes1)
                ) %>%
    # exposed to family planning messages
      mutate(expFP = if_else(rowSums(select_(., 
                                             "DHSFP714", "DHSFP714A",
                                             "DHSFP714B", "DHSFP715A",
                                             "DHSFP715B", "DHSFP715C",
                                             "DHSFP715D", "DHSFP715E"))>0, 
                            1, 0),
             notExpFPRTN = if_else(rowSums(select_(., 
                                                   "DHSFP714", "DHSFP714A",
                                                   "DHSFP714B"))==0, 
                                1, 0),
             knowsMethods = rowSums(select_(., 
                                            "DHSC30101",
                                            "DHSC30102",
                                            "DHSC30103",
                                            "DHSC30104",
                                            "DHSC30105",
                                            "DHSC30106",
                                            "DHSC30107",
                                            "DHSC30108",
                                            "DHSC30109",
                                            "DHSC30110",
                                            "DHSC30111",
                                            "DHSC30112"))
             )
  
# create table of baseline characteristics
# -----------------------------------------------------------------------------
# summarize by assignment to treatment
  part <- 
  dat %>%
    group_by(trt) %>%
    summarise(
      `Mean age (SD)` = msd(age, 1),
      `Married or in union, \\%` = per(marriedUnion, 1),
      `Christian, \\%` = per(christian, 1),
      `Luhya tribe, \\%` = per(luhya, 1),
      `Attended post-secondary schooling, \\%` = per(postSec, 1),
      `No schooling, \\%` = per(highestEdu_none, 1),
      `Nulligravida, \\%` = per(nulligravida, 1),
      `Mean number of children born (SD)` = msd(childrenEverBorn, 1),
      `Mean number of desired children (SD)` = msd(desiredFertility, 1),
      `Unmet need for spacing, \\%` = per(spacing, 1),
      `Past use of family planning, \\%` = per(discontinued, 1),
      `Mean number methods known (SD)$^c$` = msd(knowsMethods, 1),
      `Not exposed to family planning messages, \\%$^d$` = per(notExpFPRTN, 1)
    ) 
  
# format table and add DHS reference values
  tbl_part <-
    part %>%
    gather(var, value, -trt) %>% 
    spread(trt, value) %>%
    arrange(match(var, names(part)[-1])) %>%
    set_colnames(c("Characteristic", "Control", "Treatment")) %>%
    # add DHS reference values from 2014 report
    # order must match part
    mutate(refValue = c("",
                        "59.7",
                        "91.4",
                        "15.0",
                        "7.2",
                        "0.9",
                        "35.3",
                        "1.1",
                        "3.6",
                        "90.5",
                        "30.5",
                        "8.7",
                        "18.9"),
           refSource = c("",
                         "all women, national, 20-24",
                         "all women, national, 15-49",
                         "all women, national, 15-49",
                         "all women, Bungoma, 15-49",
                         "all women, Bungoma, 15-49",
                         "all women, national, 20-24",
                         "all women, national, 20-24",
                         "all women, national, 15-49",
                         "currently married women$^a$, national, 15-49",
                         "all women$^b$, national, 15-49",
                         "all women, national, 15-49",
                         "all women, Western, 15-49")
           )
    
# print table to tex
  t <- xtable(tbl_part)
  f <- "data and replication files/output/tables/part.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# summarize overall, not by group
# -----------------------------------------------------------------------------
  partAll <- 
    dat %>%
    summarise(
      `Mean age (SD)` = msd(age, 1),
      `Married or in union, \\%` = per(marriedUnion, 1),
      `Christian, \\%` = per(christian, 1),
      `Luhya tribe, \\%` = per(luhya, 1),
      `Attended post-secondary schooling, \\%` = per(postSec, 1),
      `No schooling, \\%` = per(highestEdu_none, 1),
      `Nulligravida, \\%` = per(nulligravida, 1),
      `Mean number of children born (SD)` = msd(childrenEverBorn, 1),
      `Mean number of desired children (SD)` = msd(desiredFertility, 1),
      `Unmet need for spacing, \\%` = per(spacing, 1),
      `Past use of family planning, \\%` = per(discontinued, 1),
      `Mean number methods known (SD)$^c$` = msd(knowsMethods, 1),
      `Not exposed to family planning messages, \\%$^d$` = per(notExpFPRTN, 1)
    ) 
  
  
# estimate effect of encouragement on intervention take up
# =============================================================================
# calculate take up rates
# -----------------------------------------------------------------------------
  takeup <-
  dat %>%
    group_by(trt) %>%
    summarise(triedN = sum(tried),
              triedP = mean(tried)
    )
  
  triedC <- takeup$triedN[takeup$trt==0]
  triedT <- takeup$triedN[takeup$trt==1]
  triedCP <- takeup$triedP[takeup$trt==0]
  triedTP <- takeup$triedP[takeup$trt==1]
  
# determine correlates of take up among the treatment group
# -----------------------------------------------------------------------------
  takeup <- 
  dat %>%
    filter(trt==1) %>%
    do(fit = lm(tried ~ 
                  age + 
                  marriedUnion + 
                  christian + 
                  luhya + 
                  postSec + 
                  nulligravida +
                  childrenEverBorn +
                  desiredFertility +
                  spacing +
                  discontinued +
                  knowsMethods +
                  notExpFPRTN,
                data = .)) 


# examine study attrition
# =============================================================================
  found <- sum(dat$found)
  foundP <- (found/nrow(dat))*100
  notFound <- nrow(dat)-sum(dat$found)
  textitP <- dat %>% filter(found==1) %>% summarise(textit=mean(textit))
  
# summarize by follow-up status (found vs unfound)
  att <- 
    dat %>%
    group_by(found) %>%
    summarise(
      `Assigned to treatment, \\%` = per(trt, 1),
      `Mean age (SD)` = msd(age, 1),
      `Married or in union, \\%` = per(marriedUnion, 1),
      `Christian, \\%` = per(christian, 1),
      `Luhya tribe, \\%` = per(luhya, 1),
      `Attended post-secondary schooling, \\%` = per(postSec, 1),
      `No schooling, \\%` = per(highestEdu_none, 1),
      `Nulligravida, \\%` = per(nulligravida, 1),
      `Mean number of children born (SD)` = msd(childrenEverBorn, 1),
      `Mean number of desired children (SD)` = msd(desiredFertility, 1),
      `Unmet need for spacing, \\%` = per(spacing, 1),
      `Past use of family planning, \\%` = per(discontinued, 1),
      `Mean number methods known (SD)$^a$` = msd(knowsMethods, 1),
      `Not exposed to family planning messages, \\%$^b$` = per(notExpFPRTN, 1)
    )
  
# format table
  tbl_att <-
    att %>%
    gather(var, value, -found) %>% 
    spread(found, value) %>%
    arrange(match(var, names(att)[-1])) %>%
    set_colnames(c("Characteristic", "Not Found", "Found")) %>%
    # add pvalues
    mutate(`p-value` = c(rd3(prop.test(c(sum(dat$trt[dat$found==1]), 
                                    sum(dat$trt[dat$found==0])), 
                                  c(length(dat$trt[dat$found==1]), 
                                    length(dat$trt[dat$found==0]))
                                  )$p.value), 
                    rd3(t.test(age ~ found, data = dat)$p.value),
                    rd3(prop.test(c(sum(dat$marriedUnion[dat$found==1]), 
                                    sum(dat$marriedUnion[dat$found==0])), 
                                  c(length(dat$marriedUnion[dat$found==1]), 
                                    length(dat$marriedUnion[dat$found==0]))
                                  )$p.value),
                    rd3(prop.test(c(sum(dat$christian[dat$found==1]), 
                                    sum(dat$christian[dat$found==0])), 
                                  c(length(dat$christian[dat$found==1]), 
                                    length(dat$christian[dat$found==0]))
                                  )$p.value),
                    rd3(prop.test(c(sum(dat$luhya[dat$found==1]), 
                                    sum(dat$luhya[dat$found==0])), 
                                  c(length(dat$luhya[dat$found==1]), 
                                    length(dat$luhya[dat$found==0]))
                                  )$p.value),
                    rd3(prop.test(c(sum(dat$postSec[dat$found==1]), 
                                    sum(dat$postSec[dat$found==0])), 
                                  c(length(dat$postSec[dat$found==1]), 
                                    length(dat$postSec[dat$found==0]))
                                  )$p.value),
                    rd3(prop.test(c(sum(dat$highestEdu_none[dat$found==1]), 
                                    sum(dat$highestEdu_none[dat$found==0])), 
                                  c(length(dat$highestEdu_none[dat$found==1]), 
                                    length(dat$highestEdu_none[dat$found==0]))
                                  )$p.value),
                    rd3(prop.test(c(sum(dat$nulligravida[dat$found==1]), 
                                    sum(dat$nulligravida[dat$found==0])), 
                                  c(length(dat$nulligravida[dat$found==1]), 
                                    length(dat$nulligravida[dat$found==0]))
                                  )$p.value),
                    rd3(t.test(childrenEverBorn ~ found, data = dat)$p.value),
                    rd3(t.test(desiredFertility ~ found, data = dat)$p.value),
                    rd3(prop.test(c(sum(dat$spacing[dat$found==1]), 
                                    sum(dat$spacing[dat$found==0])), 
                                  c(length(dat$spacing[dat$found==1]), 
                                    length(dat$spacing[dat$found==0]))
                                  )$p.value),
                    rd3(prop.test(c(sum(dat$discontinued[dat$found==1]), 
                                    sum(dat$discontinued[dat$found==0])), 
                                  c(length(dat$discontinued[dat$found==1]), 
                                    length(dat$discontinued[dat$found==0]))
                                  )$p.value),
                    rd3(t.test(knowsMethods ~ found, data = dat)$p.value),
                    rd3(prop.test(c(sum(dat$notExpFPRTN[dat$found==1]), 
                                    sum(dat$notExpFPRTN[dat$found==0])), 
                                  c(length(dat$notExpFPRTN[dat$found==1]), 
                                    length(dat$notExpFPRTN[dat$found==0]))
                                    )$p.value)
                    )
           ) %>%
    mutate(star = ifelse(as.numeric(`p-value`)<0.01, "***",
                  ifelse(as.numeric(`p-value`)<0.05, "**",
                  ifelse(as.numeric(`p-value`)<0.10, "*", "")))
           ) %>%
    mutate(`p-value` = paste0(`p-value`, star)) %>%
    dplyr::select(-one_of("star"))
  
# print to tex
  t <- xtable(tbl_att)
  f <- "data and replication files/output/tables/att.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
      
# conduct impact analysis
# =============================================================================
# https://rpubs.com/wsundstrom/t_ivreg

# create additional helper functions
# -----------------------------------------------------------------------------
# function to calculate corrected SEs for OLS regression 
  cse = function(reg) {
    rob = sqrt(diag(vcovHC(reg, type = "HC1")))
    return(rob)
  }
# corrected SEs for IV regressions... slight difference from S&W method
  ivse = function(reg) {
    rob = robust.se(reg)[,2]
    return(rob)
  }
# asterisk
  asterisk2 <- function(y) symnum(y, c(0, .01, .05, .1, 1),
                                     c("***", "**", "*", " "))
  
# first stage
# -----------------------------------------------------------------------------
  tried_c <- # control mean
  dat %>% 
    filter(trt==0) %>% 
    summarise(tried=mean(tried)) 
  
# ols
  firstStage_fit1 <- 
    lm(tried ~ 
         trt + 
         age + postSec + discontinued + marriedUnion +
         nulligravida + childrenEverBorn,
       data = dat)
  
# probit  
  firstStage_fit3 <- 
    glm(tried ~ 
         trt + 
         age + postSec + discontinued + marriedUnion +
         nulligravida + childrenEverBorn,
        data = dat,
        family = binomial(link = "probit"))
  
# ITT
# -----------------------------------------------------------------------------
# ols, locf  
  anyFP_locf_c <- # control mean
  dat %>% 
    filter(trt==0) %>% 
    summarise(anyFP=mean(anyFP_locf))
  
  anyFP_itt_fit1 <- 
    lm(anyFP_locf ~ 
        trt + 
         textit_locf + age + postSec + discontinued + marriedUnion +
        nulligravida + childrenEverBorn,
       data = dat)
  
  anyFP_itt_fit1_est <- summary(anyFP_itt_fit1)$coefficients[2,1]
  
# ols, no replacement
  anyFP_c <- # control mean
    dat %>% 
    filter(trt==0) %>% 
    summarise(anyFP=mean(anyFP, na.rm=TRUE))
  
  anyFP_itt_fit2 <- 
    lm(anyFP ~ 
         trt + 
         textit + age + postSec + discontinued + marriedUnion +
         nulligravida + childrenEverBorn,
       data = dat)
  
  anyFP_itt_fit2_est <- summary(anyFP_itt_fit2)$coefficients[2,1]
  
# probit, locf
  anyFP_itt_fit3 <- 
    glm(anyFP_locf ~ 
          trt + 
          textit_locf + age + postSec + discontinued + marriedUnion +
          nulligravida + childrenEverBorn,
        data = dat,
        family = binomial(link = "probit"))
  
  anyFP_itt_fit3_est <- summary(anyFP_itt_fit3)$coefficients[2,1]
  
# IV
# -----------------------------------------------------------------------------
# ols, locf    
  anyFP_iv_fit1 <- 
    ivreg(anyFP_locf ~ 
            tried + 
            textit_locf + age + postSec + discontinued + marriedUnion + 
            nulligravida + childrenEverBorn | 
            textit_locf + age + postSec + discontinued + marriedUnion +
            nulligravida + childrenEverBorn +
            trt, 
          data = dat)
  
  anyFP_iv_fit1_est <- summary(anyFP_iv_fit1)$coefficients[2,1]
  
# ols, no replacement
  anyFP_iv_fit2 <- 
    ivreg(anyFP ~ 
            tried + 
            textit + age + postSec + discontinued + marriedUnion + 
            nulligravida + childrenEverBorn | 
            textit + age + postSec + discontinued + marriedUnion +
            nulligravida + childrenEverBorn +
            trt, 
          data = dat)
  
  anyFP_iv_fit2_est <- summary(anyFP_iv_fit2)$coefficients[2,1]
  
# logit, locf    
# export to run in stata
# path to stata executable for quantile regression
#   instructions for mac: http://www.stata.com/support/faqs/mac/advanced-topics/
#   change path and suffix as needed (StataSE, StataMP, smStata)
  runInStata = 0
  if (runInStata==1) {
    # change for your system
    stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP"
    library(foreign)
    datStata <- 
    dat %>%
      dplyr::select(anyFP_locf, tried, textit_locf, age, postSec, discontinued, 
                    marriedUnion, nulligravida, childrenEverBorn, trt)
    write.dta(datStata, "output/datStata.dta")
    system(paste(stata, "-q -e scripts/ivprobit", sep=" "))
  }
  
  probit <- read.csv("data and replication files/output/stataout.csv", 
                     stringsAsFactors = FALSE)
  names(probit) <- probit[1, ]
  probit <- probit[-1,]
  probit <- probit[-nrow(probit),]
  
  anyFP_iv_fit3_est <- paste0(round(as.numeric(probit[1,2]), 2),
                              asterisk2(as.numeric(probit[1,5])))
  anyFP_iv_fit3_ci <- paste0("(",
                             paste(round(as.numeric(unlist(strsplit(probit[1,6],
                                                                    ","))), 2),
                                   collapse=", "),
                             ")")
  

  
  