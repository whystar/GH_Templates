
library(lmerTest)
library(pROC)
library(car)
library(MuMIn)
library(stargazer)
library(sjstats)
library(rms)
library(data.table)




#================================================================
# 5: submission
#================================================================


data <- read.csv("rdd_metrics_issue.csv")
data_submission = data[!is.na(data$n_issues),]
data_submission$intervention = as.logical(data_submission$intervention)



fm_submission <- lmer(formula = log(n_issues+0.5) ~ 
                        + log(prj_age + 0.5)
                      + log(stars + 0.5)
                      + log(commits + 0.5)
                      + log(n_open_tasks + 0.5)
                      
                      + (1|repo_id)
                      + (1|prj_lang)
                      + time
                      + intervention
                      + time_after_intervention
                      ,
                      data=data_submission)

summary(fm_submission)
r.squaredGLMM(fm_submission)






#================================================================
# 3: comments 
#================================================================

data <- read.csv("rdd_metrics_issue.csv")
data_comment = data[!is.na(data$comment),]
data_comment$intervention = as.logical(data_comment$intervention)

fm_comment <- lmer(formula = log(comment+0.5) ~ 
                     + log(prj_age + 0.5)
                   + log(stars + 0.5)
                   + log(commits + 0.5)
                   + log(n_rwrs + 0.5)
                   + log(n_open_tasks + 0.5)
                   + log(exp_issue + 0.5)
                   +log(desc_issue + 0.5)
                   + (1|repo_id)
                   + (1|prj_lang)
                   + time
                   + intervention
                   + time_after_intervention
                   ,
                   data=data_comment)

summary(fm_comment)
r.squaredGLMM(fm_comment)





#================================================================
# 1: resolution
#================================================================
data <- read.csv("rdd_metrics_issue.csv")
data_resolve = data[!is.na(data$resolve),]
data_resolve$intervention = as.logical(data_resolve$intervention)


fm1_resolve <- lmer(formula = log(resolve/60/60+0.5) ~ 

            + log(prj_age + 0.5)
            + log(stars + 0.5)
            + log(commits + 0.5)
            + log(n_rwrs + 0.5)
            + log(n_open_tasks + 0.5)
            + log(exp_issue + 0.5)
            + log(desc_issue + 0.5)
            + (1|repo_id)
           + (1|prj_lang)
           + time
           + intervention
           + time_after_intervention
              ,
            data=data_resolve)

summary(fm1_resolve)
r.squaredGLMM(fm1_resolve)

