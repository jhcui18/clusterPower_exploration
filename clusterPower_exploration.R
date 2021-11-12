library(clusterPower)
library(shinythemes)
library(shiny)
runExample(appname = "analytic")#don't work


#########cpa.binary#########

# Compute the power, number of clusters needed, number of subjects per cluster needed, or other key 
# parameters for a parallel cluster randomized trial with a binary outcome.

# Find the number of clusters per condition needed for a trial with alpha = .05,
# power = 0.8, 10 observations per cluster, no variation in cluster size, probability
# in condition 1 of .1 and condition 2 of .2, and ICC = 0.1.
cpa.binary(power = 0.08, nsubjects = 10, p1 = 0.1, p2 = 0.2, ICC = 0.1)
# The result, showing nclusters of greater than 37, suggests 38 clusters per
# condition should be used.



# Find the minimum detectable p2 > p1, given 38 clusters per condition, 10
# observations per cluster no variation in cluster size, ICC of 0.1, and
# probability of .1 in condition 2, with power of .8.
## Not run:
cpa.binary(power = 0.08, nsubjects = 10, nclusters = 38,
           p1 = 0.1, p2 = NA, ICC = 0.1, p1inc = FALSE)
# The result shows that p2 greater than 0.198922 can be detected with at
# least 80% power



########cps.binary##########

# This function uses Monte Carlo methods (simulations) to estimate power for cluster-randomized trials.

# Estimate power for a trial with 10 clusters in each arm, 20 subjects in
# each cluster, with a probability of 0.8 in the first arm and 0.5 in the
# second arm, with a sigma_b_sq = 1 in the first arm sigma_b_sq = 1.2 in
# the second arm.
## Not run:
binary.sim = cps.binary(nsim = 100, nsubjects = 20,
                        nclusters = 10, p1 = 0.8,
                        p2 = 0.5, sigma_b_sq = 1,
                        sigma_b_sq2 = 1.2, alpha = 0.05,
                        method = 'glmm', allSimData = FALSE)

# Estimate power for a trial just as above, except that in the first arm,
# the clusters have 10 subjects in 9 of the 10 clusters and 100 in the tenth
# cluster, while in the second arm all clusters have 20 subjects.
library(geepack)
## Not run:
binary.sim2 = cps.binary(nsim = 100,
                         nsubjects = c(c(rep(10,9),100), rep(20,10)),
                         nclusters = 10, p1 = 0.8,
                         p2 = 0.5, sigma_b_sq = 1,
                         sigma_b_sq2 = 1.2, alpha = 0.05,
                         method = 'gee', allSimData = FALSE)


# compare analytic vs stimulated power
cpa.binary(nclusters = 10, nsubjects = 10, p1 = 0.1, p2 = 0.2, ICC = 0.1)

binary.sim = cps.binary(nsim = 100, nsubjects = 10,
                        nclusters = 10, p1 = 0.1,
                        p2 = 0.2, sigma_b_sq = 1,
                        sigma_b_sq2 = 1, alpha = 0.05,
                        method = 'glmm', allSimData = FALSE)


# compare analytic vs stimulated power
###########
# [1st parameter set]: assume equal between-cluster variance
# p1 = 0.8, p2 = 0.5, equal sigma of 1
binary.sim = cps.binary(nsim = 1000, nsubjects = 20,
                        nclusters = 10, p1 = 0.8,
                        p2 = 0.5, sigma_b_sq = 1,
                        sigma_b_sq2 = 1, alpha = 0.05,
                        method = 'glmm', allSimData = FALSE)
binary.sim #ICC P_h=0.233 P_c=0.001 lmer=0.472 power = 0.81[0.785, 0.835]

cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.233) #power=0.767
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.001) #power=0.999
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.472) #power=0.510

# [2nd parameter set]: assume unequal between-cluster variance
# p1 = 0.8, p2 = 0.5, sigma1 = 1, sigma2 = 1.2
binary.sim2 = cps.binary(nsim = 1000, nsubjects = 20,
                         nclusters = 10, p1 = 0.8,
                         p2 = 0.5, sigma_b_sq = 1,
                         sigma_b_sq2 = 1.2, alpha = 0.05,
                         method = 'glmm', allSimData = FALSE)
binary.sim2 #ICC P_h=0.25 P_c=0.001 lmer=0.484 power = 0.78[0.753, 0.805]

cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.25) #power=0.743
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.001) #power=0.999
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.484) #power=0.502

# [3nd parameter set]: assume a bigger difference between between-cluster variance
# p1 = 0.8, p2 = 0.5, sigma1 = 1, sigma2 =2
binary.sim3 = cps.binary(nsim = 1000, nsubjects = 20,
                         nclusters = 10, p1 = 0.8,
                         p2 = 0.5, sigma_b_sq = 1,
                         sigma_b_sq2 = 2, alpha = 0.05,
                         method = 'glmm', allSimData = FALSE) # fail to converge
binary.sim3 #ICC P_h=0.306 P_c=-0.002 lmer=0.521 power = 0.688[0.658, 0.717]
table(binary.sim3$convergence) #7/1000 not converging
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.306) #power=0.0.671
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = -0.002) #power=0.999
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.5, ICC = 0.521) #power=0.476

# [4th parameter set]: assume same p and different sigma
# p1 = 0.8, p2 = 0.8, sigma1 = 1, sigma2 = 2
binary.sim4 = cps.binary(nsim = 1000, nsubjects = 20,
                         nclusters = 10, p1 = 0.8,
                         p2 = 0.8, sigma_b_sq = 1,
                         sigma_b_sq2 = 2, alpha = 0.05,
                         method = 'glmm', allSimData = FALSE, lowPowerOverride = TRUE) # fail to converge
binary.sim4 #ICC P_h=0.306 P_c=0.016 lmer=0.514 power = 0.072[0.057, 0.0898]
table(binary.sim4$convergence) #9/1000 not converging
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.8, ICC = 0.306) #power=0.025
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.8, ICC = 0.016) #power=0.025
cpa.binary(nclusters = 10, nsubjects = 20, p1 = 0.8, p2 = 0.8, ICC = 0.514) #power=0.025

#try to generate a similar power with different sigma_b
binary.sim2 = cps.binary(nsim = 100, nsubjects = 20,
                         nclusters = 10, p1 = 0.8,
                         p2 = 0.5, sigma_b_sq = 1.2,
                         sigma_b_sq2 = 1.2, alpha = 0.05,
                         method = 'glmm', allSimData = FALSE)
binary.sim2 #power = 0.7 ICC P_h = 0.267
#reduce sigma_b increase power

#different Pr(y=1)?
binary.sim3 = cps.binary(nsim = 100, nsubjects = 20,
                         nclusters = 10, p1 = 0.8,
                         p2 = 0.55, sigma_b_sq = 1,
                         sigma_b_sq2 = 1, alpha = 0.05,
                         method = 'glmm', allSimData = FALSE,lowPowerOverride = TRUE)
binary.sim3 #power=0.62 ICC P_h =0.233



##############################################################
#################Create Output Tables#########################
##############################################################
#funtion to generate the table columns when running cps.binary
#input: number of stimulation, number of subjects, number of clusters, p1, p2, signma^b_1, sigma^b_2
#output: ICC, stimulated power, analytical power for each ICC and % not converge to the table
power_output_tbl <- function(nsim, nsubjects, nclusters, p1, p2, sigma_b_sq, sigma_b_sq2){
  #simulated power calculation
  binary.sim = cps.binary(nsim = nsim, nsubjects = nsubjects,
                          nclusters = nclusters, p1 = p1,
                          p2 = p2, sigma_b_sq = sigma_b_sq,
                          sigma_b_sq2 = sigma_b_sq2, alpha = 0.05,
                          method = 'glmm', allSimData = FALSE,lowPowerOverride = TRUE, poorFitOverride = TRUE,
  )
  #calculate % of Data set where the model won't converge among the first nsim simulations
  num_not_converge <- table(binary.sim$convergence[1:nsim])
  pct_not_converge <- unname(num_not_converge[1])/(unname(num_not_converge[1])+unname(num_not_converge[2]))
  pct_not_converge <- ifelse(is.na(pct_not_converge), 0, pct_not_converge)
  #three ICC from simulated power calculation
  ICC.P_h <- binary.sim$ICC[1] #ICC.P_h is the same for each simulation
  ICC.P_c <- mean(binary.sim$icc.list[1:nsim,1],na.rm=T) #only include ICC of the first nsim simulations
  ICC.lmer <- mean(binary.sim$icc.list[1:nsim,2],na.rm=T) #only include ICC of the first nsim simulations
  #simulated power calculation based on the first nsim simulations
  #the number of p.values <0.05 among all converged dataset of the first nsim simulations
  #p value is NA for unconverged datasets
  sim_pwr <- (length(which(binary.sim$model.estimates$p.value[1:nsim]<0.05)))/nsim
  #calculated as proportion confidence interval 
  sim_pwr_lower_CI <- sim_pwr-qnorm(0.975)*sqrt((sim_pwr*(1-sim_pwr))/nsim)
  sim_pwr_upper_CI <- sim_pwr+qnorm(0.975)*sqrt((sim_pwr*(1-sim_pwr))/nsim)
  
  #analyrical power calculation
  #with ICC P_h
  anal.pwr.P_h <- cpa.binary(nclusters = nclusters, nsubjects = nsubjects, p1 = p1, p2 = p2, ICC = ICC.P_h) 
  #with ICC P_c
  anal.pwr.P_c <- cpa.binary(nclusters = nclusters, nsubjects = nsubjects, p1 = p1, p2 = p2, ICC = ICC.P_c) 
  #with ICC lmer
  anal.pwr.lmer <- cpa.binary(nclusters = nclusters, nsubjects = nsubjects, p1 = p1, p2 = p2, ICC = ICC.lmer) 
  
  #output table
  tab <- matrix(c(nsim, nsubjects, nclusters, p1, p2, sigma_b_sq, sigma_b_sq2, ICC.P_h, ICC.P_c, ICC.lmer, pct_not_converge*100, sim_pwr, sim_pwr_lower_CI, sim_pwr_upper_CI,unname(anal.pwr.P_h), unname(anal.pwr.P_c), unname(anal.pwr.lmer)), ncol=1, byrow=TRUE)
  colnames(tab) <- c("parameter set 1")
  rownames(tab) <- c('nsim', 'nsubjects', 'nclusters', 'p1', 'p2', 'sigma_b_sq', 'sigma_b_sq2', 'ICC P_h', 'ICC P_c', 'ICC.lmer', '% of Dataset not converge', 'stimulated power','stimulated power lower 95% CI','stimulated power upper 95% CI','Analytic power using ICC P_h','Analytic power using ICC P_c','Analytic power using ICC lmer')
  tab <- as.table(tab)
  tab
}

#output from function
test <- power_output_tbl(nsim = 100, nsubjects=20, nclusters=15, p1=0.8, p2=0.5, sigma_b_sq=1, sigma_b_sq2=1)
test

test.sim = cps.binary(nsim = 100, nsubjects = 20,
                      nclusters = 15, p1 = 0.8,
                      p2 = 0.5, sigma_b_sq = 1,
                      sigma_b_sq2 = 1, alpha = 0.05,
                      method = 'glmm', poorFitOverride = TRUE,lowPowerOverride = TRUE)

#ICC.P_c
mean(test.sim$icc.list[1:100,1],na.rm=T)
#ICC.lmer
mean(test.sim$icc.list[1:100,2],na.rm=T)
#power
sim_pwr <- (length(which(test.sim$model.estimates$p.value[1:100]<0.05)))/100
sim_pwr
#confidence interval
sim_pwr_lower_CI <- sim_pwr-qnorm(0.975)*sqrt((sim_pwr*(1-sim_pwr))/100)
sim_pwr_lower_CI
sim_pwr_upper_CI <- sim_pwr+qnorm(0.975)*sqrt((sim_pwr*(1-sim_pwr))/100)
sim_pwr_upper_CI
#convergence
num_not_converge <- table(test.sim$convergence[1:100])
pct_not_converge <- unname(num_not_converge[1])/(unname(num_not_converge[1])+unname(num_not_converge[2]))
pct_not_converge <- ifelse(is.na(pct_not_converge), 0, pct_not_converge)
pct_not_converge


table(test.sim$convergence)
#confidence interval of all, check if is the same with the cps.binary output
sim_pwr <- (length(which(test.sim$model.estimates$p.value<0.05)))/101
sim_pwr
sim_pwr_lower_CI <- sim_pwr-qnorm(0.975)*sqrt((sim_pwr*(1-sim_pwr))/101)
sim_pwr_lower_CI
sim_pwr_upper_CI <- sim_pwr+qnorm(0.975)*sqrt((sim_pwr*(1-sim_pwr))/101)
sim_pwr_upper_CI


test.sim$power


# To start: sigma^b_1 = sigma^b_2 = .01, 1
# p_1 = c(.05, .25, .5) and p_2 = p_1 + c(0,.1,.3)
# Nsubjects = 20, 100
# Nclusters (per treatment group) = 15, 50  

v_power_output_tbl <- Vectorize(power_output_tbl)
# vector has to be the same length? YES
# output_table <- v_power_output_tbl(nsim=rep(10,3), nsubject=c(10,20,50), nclusters = c(8,15,25,50), p1=seq(from = 0.05, to = 0.85, by = 0.2), p2 = seq(from = 0.15, to = 0.95, by = 0.4), sigma_b_sq = c(0.01,0.1,1), sigma_b_sq2 = c(0.01,0.1,1))


# create a table of all combinations of parameters
# p2 needs to be always equal or greater than p1

#first table, p1=0.05, p2=p1+c(0, 0.1, 0.3)
parameter_set1 = expand.grid(nsim = 1000, 
                             nsubjects = c(20,100), 
                             ncluster = c(15,50), 
                             p1 = c(0.05), 
                             p2 = c(0.05,0.15,0.35),#greater or equal to p1: three separate runnings 
                             sigma_b_sq = c(0.01,1))
parameter_set1$sigma_b_sq2 <- parameter_set2$sigma_b_sq

head(parameter_set1)

v_power_output_tbl <- Vectorize(power_output_tbl)

output_table1 <- v_power_output_tbl(nsim = parameter_set1$nsim, 
                                    nsubject = parameter_set1$nsubjects, 
                                    nclusters = parameter_set1$ncluster, 
                                    p1 = parameter_set1$p1, 
                                    p2 = parameter_set1$p2, 
                                    sigma_b_sq = parameter_set1$sigma_b_sq, 
                                    sigma_b_sq2 = parameter_set1$sigma_b_sq2)

df.output_table1 <- as.data.frame(output_table1)
saveRDS(df.output_table1, file = "output_table1.rds")
#second table, p1=0.25, p2=p1+c(0, 0.1, 0.3)
parameter_set2 = expand.grid(nsim = 1000, 
                             nsubjects = c(20,100), 
                             ncluster = c(15,50), 
                             p1 = c(0.25), 
                             p2 = c(0.25,0.35,0.55),#greater or equal to p1: three separate runnings 
                             sigma_b_sq = c(0.01,1))
parameter_set2$sigma_b_sq2 <- parameter_set1$sigma_b_sq
output_table2 <- v_power_output_tbl(nsim = parameter_set2$nsim, 
                                    nsubject = parameter_set2$nsubjects, 
                                    nclusters = parameter_set2$ncluster, 
                                    p1 = parameter_set2$p1, 
                                    p2 = parameter_set2$p2, 
                                    sigma_b_sq = parameter_set2$sigma_b_sq, 
                                    sigma_b_sq2 = parameter_set2$sigma_b_sq2)

df.output_table2 <- as.data.frame(output_table2)
saveRDS(df.output_table2, file = "output_table2.rds")


#third table, p1=0.5, p2=p1+c(0, 0.1, 0.3)
parameter_set3 = expand.grid(nsim = 1000, 
                             nsubjects = c(20,100), 
                             ncluster = c(15,50), 
                             p1 = c(0.5), 
                             p2 = c(0.5,0.6,0.8),#greater or equal to p1: three separate runnings 
                             sigma_b_sq = c(0.01,1))
parameter_set3$sigma_b_sq2 <- parameter_set3$sigma_b_sq

output_table3 <- v_power_output_tbl(nsim = parameter_set3$nsim, 
                                    nsubject = parameter_set3$nsubjects, 
                                    nclusters = parameter_set3$ncluster, 
                                    p1 = parameter_set3$p1, 
                                    p2 = parameter_set3$p2, 
                                    sigma_b_sq = parameter_set3$sigma_b_sq, 
                                    sigma_b_sq2 = parameter_set3$sigma_b_sq2)

df.output_table3 <- as.data.frame(output_table3)
saveRDS(df.output_table3, file = "output_table3.rds")

# When sigma is 0.01, more than 25% of simulations are singular fit (not converge) [solved by adding lowPowerOverride = TRUE]



v_power_output_tbl <- Vectorize(power_output_tbl)
#vector has to be the same length?
output_table <- v_power_output_tbl(nsim = parameter_set$nsim, 
                                   nsubject = parameter_set$nsubjects, 
                                   nclusters = parameter_set$ncluster, 
                                   p1 = parameter_set$p1, 
                                   p2 = parameter_set$p2, 
                                   sigma_b_sq = parameter_set$sigma_b_sq, 
                                   sigma_b_sq2 = parameter_set$sigma_b_sq2)

df.output_table <- as.data.frame(output_table)


mapply(FUN = sum, args$x, args$y, args$z)


binary.sim = cps.binary(nsim = 100, nsubjects = 20,
                        nclusters = 10, p1 = 0.8,
                        p2 = 0.5, sigma_b_sq = 0.01,
                        sigma_b_sq2 = 0.01, alpha = 0.05,
                        method = 'glmm', poorFitOverride = TRUE,lowPowerOverride = TRUE)
num_not_converge <- table(binary.sim$convergence[1:100])
num_not_converge
binary.sim$ICC
mean(binary.sim$icc.list[1:100,2],na.rm=T)

length(which(binary.sim$model.estimates$p.value<0.05))
binary.sim$model.estimates

length(which(binary.sim$model.estimates$p.value[1:100]<0.05))
binary.sim$power

#the number of p.values <0.05 among all converged dataset of the first nsim simulations
f <- function(x,y) x+y
vf <- Vectorize(f)

