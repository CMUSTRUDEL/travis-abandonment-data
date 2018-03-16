library(xtable)
library(interplot)
library(caret)
library(car)
library(pscl)
library(pROC)
library(dplyr)
library(stargazer)

# Read in merged csv file
df.match <- read.csv2(file="matched_data.csv", quote="\"", sep=";")

# Convert date strings into real dates
# df$created_at <- as.Date(df$created_at)
# df$first_build <- as.Date(df$first_build)
# df$last_build <- as.Date(df$last_build)
# df$last_commit <- as.Date(df$last_commit)
# 
# summary(df$created_at)
# summary(df$first_build)
# summary(df$last_build)
# 
# # Calculate project age, in days
# df$project_age = as.numeric(Sys.Date() - df$created_at)
# head(df[c("project_age", "created_at")])
# 
# # Compute usage period for builds, in days
# df$builds_period <- as.numeric(df$last_build - df$first_build)
# summary(df$builds_period)
# 
# head(df$active)
# df$abandoned = 1-df$active
# head(df$abandoned)
# 
# df.new = subset(df, created_at < "2015-04-01" & 
#                   first_build < "2015-04-01" &
#                   last_commit <= "2017-06-01" &
#                   last_commit >= "2011-01-01" &
#                   project_age <= 3000 &
#                   last_build > first_build &
#                   last_build_duration > 0 &
#                   last_build_duration <= 3000 &
#                   yml_commits > 0 &
#                   commits > 0 &
#                   contribs > 0)
# df.new = df.new[complete.cases(df.new), ]
# 
# nrow(df.new)
# hist(df.new$created_at, breaks=200)
# hist(df.new$first_build, breaks=200)
# 
# hist(df.new$last_build, breaks=200)
# hist(df.new$last_commit, breaks=200)
# 
# # table(df.new[df.new$abandoned==1 & (df.new$last_commit - df.new$last_build) > 0,])
# 
# df.new$commit_time_after_last_build = as.numeric(df.new$last_commit - df.new$last_build, units = "days")
# summary(df.new$commit_time_after_last_build)
# 
# x = subset(df.new, abandoned == 1 & commit_time_after_last_build >= 0)
# hist(x$commit_time_after_last_build, breaks=200)
# table(x$commit_time_after_last_build >= 30)
# 
# df.new$abandoned_and_alive = (df.new$abandoned == 1) & (df.new$commit_time_after_last_build >= 30)
# 
# # tail(sort(as.Date(df[df.new$abandoned_and_alive == TRUE,]$last_t_commit)), 10)
# # df.new[with(df.new[which(df.new$abandoned_and_alive == TRUE),], order(-as.numeric(last_t_commit))), ]
# 
# (nrow(df[df$abandoned == 1,])-nrow(df.new[df.new$abandoned_and_alive == 1, ]))/nrow(df[df$active == 0,])
# 
# hist(df.new[df.new$abandoned_and_alive,]$commit_time_after_last_build, breaks=200)
# hist(df.new[df.new$abandoned_and_alive,]$project_age, breaks=200)
# 
# #remove those projects with uncommon languages, only including those which have more than 20 occurances, 1% loss rate
# 
# before_count <- nrow(df.new)
# num_langs = sort(table(df.new[df.new$abandoned_and_alive == 1,]$lang))
# num_langs
# length(num_langs)
# df.new <- subset(df.new, lang %in% names(num_langs[num_langs >= 40]))
# df.new <- droplevels(df.new)
# (before_count-nrow(df.new))/nrow(df.new)
# 
# 
# # summary(df.new[df.new$abandoned==0,]$project_age)
# # summary(df.new[df.new$abandoned==1,]$project_age)
# # 
# # table(df.new$abandoned)
# 
# nrow(df.new)
# df.sub = subset(df.new, abandoned_and_alive | active)
# nrow(df.sub)
# 
# sort(table(df.sub$lang))
# 
# # # Remove outliers, top 1% unless very low variation in data (eg, yml_contribs has max value 16, job_count has max value 19), # 8% loss rate
# df.sub <- subset(df.sub,
#                   # build_count < quantile(df.sub$build_count, .99) &
#                   # builds_period < quantile(df.sub$builds_period, .99) &
#                   # last_build_duration < quantile(df.sub$last_build_duration, .99) &
#                   # commits < quantile(df.sub$commits, .99) &
#                   contribs < quantile(df.sub$contribs, .99) &
#                   # job_count < quantile(df.sub$job_count, .99) &
#                   # project_age < quantile(df.sub$project_age, .99) &
#                   PRs < quantile(df.sub$PRs, .99) 
#                   # pushes < quantile(df.sub$pushes, .99) &
#                   # yml_commits < quantile(df.sub$yml_commits, .99) &
#                   # yml_contribs < quantile(df.sub$yml_contribs, .99)
# )
# 
# 
# 
# library("MatchIt")
# # options("optmatch_max_problem_size" = Inf)
# # match.it = matchit(abandoned ~ project_age + first_build, data = df.new, 
# #                    method="nearest", ratio = 1, discard = "both", reestimate = TRUE) #
# match.it = matchit(abandoned_and_alive ~ first_build, data = df.sub, # + lang
#                    method="nearest", ratio = 3, discard = "both", reestimate = TRUE) 
# 
# # ?matchit
# a = summary(match.it)
# a
# library("knitr")
# kable(a$nn, digits = 2, align = 'c', 
#       caption = 'Table 2: Sample sizes')
# kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
#       caption = 'Table 3: Summary of balance for matched data')
# plot(match.it, type = 'jitter', interactive = FALSE)
# 
# df.match <- match.data(match.it)[1:ncol(df.sub)]
# # rm(df.patients, df.population)
# 
# boxplot(project_age ~ abandoned_and_alive, data=df.match)
# wilcox.test(project_age ~ abandoned_and_alive, data=df.match)
# 
# boxplot(first_build ~ abandoned_and_alive, data=df.match)
# wilcox.test(as.numeric(first_build) ~ abandoned_and_alive, data=df.match)
# 
# layout(1:1)
# boxplot(last_build ~ abandoned_and_alive, data=df.match)
# 
# hist(df.match$created_at, breaks=200)
# hist(df.match$first_build, breaks=200)
# hist(df.match$last_build, breaks=200)
# 
# # options(contrasts=c(unordered="contr.poly", ordered="contr.poly"))
# 
# sort(table(df.match[df.match$abandoned_and_alive == 1, ]$lang))
# t1 = sort(table(df.match[df.match$abandoned_and_alive == 0, ]$lang))
# t1
# t1[t1>30]
# t1[t1<=30]
# 
# # bmatch <- df.match



# vs Mean Contrasts
cs = contr.sum(14)
cs
colnames(cs) = sort(unique(df.match$lang))[1:length(unique(df.match$lang))-1]
colnames(cs) = paste(colnames(cs), "vs Mean")
cs
contrasts(df.match$lang) = cs


# vs JS Contrasts
# js = contr.treatment(14, base = 8)
# colnames(js) = sort(unique(df.match$lang))[2:length(unique(df.match$lang))]
# colnames(js) = paste(c("C","C#","C++","CSS","Go","HTML","Java","Objective-C","PHP","Puppet","Python","Ruby","Shell"), "vs Javascript")
# contrasts(df.match$lang) = js
# sort(table(df.match$lang))
# 
# # vs C Contrasts
# ds = contr.treatment(14, base = 1)
# colnames(ds) = sort(unique(df.match$lang))[2:length(unique(df.match$lang))]
# colnames(ds) = paste(c("C#","C++","CSS","Go","HTML","Java","JavaScript","Objective-C","PHP","Puppet","Python","Ruby","Shell"), "vs C")
# contrasts(df.match$lang) = ds
# sort(table(df.match$lang))

# df_fork <- read.csv(file=file.choose(), quote="\"", sep=",", na.string=c("NULL", "", "NA"))
# 
# nrow(df.match)
# merged = merge(df.match, df_fork, by="travis_id")
# merged = merged[!duplicated(merged$travis_id), ]

# count_abandoners_forks = nrow(subset(merged, 
#             abandoned_and_alive == 0
#             & is.na(forked_from)
#             ))

# email_list <- subset(merged, 
#                  abandoned_and_alive == 1
#                  & is.na(forked_from)
#                  & as.Date(last_t_commit) >= "2015-01-01"
#                  )

hist(log(df.match$project_age))
hist(log(df.match$last_build_duration))
hist(log(df.match$commits))
hist(log(df.match$contribs))
hist(log(df.match$job_count))
hist(log(df.match$PRs+1))
hist(log(df.match$yml_commits))
hist(log(df.match$yml_contribs))

summary(as.Date(merged$last_t_commit))

combined_logit <- glm(abandoned_and_alive  ~
                        # log(build_count) +
                        log(project_age) +
                        log(last_build_duration) +
                        log(commits) +
                        log(contribs) +
                        log(job_count) +
                        log(PRs+0.5) +
                        # log(pushes+0.5) +
                        log(yml_commits) +
                        log(yml_contribs) + 
                        lang,
                      data = df.match, 
                      family = "binomial"
                      )
summary(combined_logit)
Anova(combined_logit, type=2)


library("car")
vif(combined_logit)

plot(combined_logit)

summary(combined_logit)
Anova(combined_logit, type=2)
library(pscl)
pR2(combined_logit)

library(pROC)
auc(roc(df.match$abandoned_and_alive ~ predict(combined_logit, type=c("response"))))
plot(roc(df.match$abandoned_and_alive ~ predict(combined_logit, type=c("response"))))

source("helpers.R")
library("texreg")
library("xtable")

install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(df.match$abandoned_and_alive, fitted(combined_logit), g=10)
