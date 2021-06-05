dat = structure(list(f1 = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                      1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("a", 
                                                                                                  "c"), class = "factor"), f2 = structure(c(1L, 2L, 1L, 2L, 1L, 
                                                                                                                                            2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), .Label = c("1", 
                                                                                                                                                                                                                    "c"), class = "factor"), resp = c(1.6, 0.3, 3, 0.1, 3.2, 0.2, 
                                                                                                                                                                                                                                                      0.4, 0.4, 2.8, 0.7, 3.8, 3, 0.3, 14.3, 1.2, 0.5, 1.1, 4.4, 0.4, 
                                                                                                                                                                                                                                                      8.4)), row.names = c(NA, -20L), class = "data.frame")

str(dat)
history(dat$resp)

fit1 = lm(log(resp) ~ f1 + f2 + f1:f2, data = dat)
fit1

   ### pairwise comparisons for all combinations of f1 and f2
   ### the factors with levels we want to compare among are on the
   ### right-hand side,  combination of f1 and f2 are put in the formula
   ### We are comparing levels within each factor with levels of another factor
   ### Results are given on the log (not the response) scale. 
   ### Confidence level used: 0.95
emm1 = emmeans(fit1, specs = pairwise ~ f1:f2)
emm1

   ### Using formula above returns an object with two parts. 
   ### The first part, called emmeans, is the estimated marginal means 
   ### along with the standard errors and confidence intervals
   ### results are all on the model scale
emm1$emmeans
   ### The second part output (contrasts) contains the comparisons of interest.

   ### Previous model was based on log transformed scale. 
   ### Now back-transform the variable by setting type = variable
   ### In this case, type = "response" will return results based on original scale
emmeans(fit1, specs = pairwise ~ f1:f2, type = "response")
