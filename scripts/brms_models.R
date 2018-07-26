library(dplyr)
library(ggplot2)
library(here)
library(brms)
vignette(package = "brms")

scaled_df <- final_df %>% mutate_each_(funs(scale),vars=c("visit_time","drug_time","updrsT","age"))


row_to_remove <- which(final_df$PATNO == 3478)
final_df <- final_df[-row_to_remove,]
surg.m1 <- brm(updrsT ~ (1|PATNO) + (1|CNO)
                 + visit_time + visit_time:age + PDSURG,
                 data=scaled_df,
                 prior=c(set_prior("normal(0,10)", class="Intercept"),
                         set_prior("normal(0,10)", class='b'),
                         set_prior("cauchy(0,1)", class='sd', group="PATNO"),
                         set_prior("cauchy(0,1)", class='sd', group="CNO")))

bayes.mod <- brm(updrsT ~ (1|PATNO) + (1|CNO)
                 + visit_time + visit_time:age,
                 data=final_df,
                 prior=c(set_prior("normal(0,10)", class="Intercept"),
                         set_prior("normal(0,10)", class='b'),
                         set_prior("cauchy(0,1)", class='sd', group="PATNO"),
                         set_prior("cauchy(0,1)", class='sd', group="CNO")))

nd <- 
  tibble(population = seq(from = 1000, to = 300000, by = 5000),
         # To "simulate counterfactual societies, using the hyper-parameters" (p. 383), we'll plug a new island into the `culture` variable
         culture = "My_island") summ

p1 <- predict(bayes.mod,
              allow_new_levels = T,
              data = final_df,
              re_formula = ~ (1 | PATNO))


ggplot(final_df, aes(visit_time, Estimate,col= PDSURG )) + geom_line(alpha = 0.5,aes(group= factor(PATNO))) + ggtitle("UPDRS totals over time")

# Model where we assume that as time goes on that the slope of visit_time will be different depending on the 
pred1 <- brm(updrsT ~ (visit_time|PATNO) + (1|CNO) + 
                   + visit_time + visit_time:age,
                 data=final_df,
                 prior=c(set_prior("normal(0,10)", class="Intercept"),
                         set_prior("normal(0,10)", class='b'),
                         set_prior("cauchy(0,1)", class='sd', group="PATNO"),
                         set_prior("cauchy(0,1)", class='sd', group="CNO")))
