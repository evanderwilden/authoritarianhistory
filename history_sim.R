#########################
# Ethan vanderWilden
# Simulating Data
# Authoritarian Praise and the Far Right (with Laia Balcells and Sergi Martinez)

# March 2023
#########################

############## 1. Load in packages ####
# List of packages
pkg = c("tidyverse", "gridExtra", "haven", "modelsummary")

# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))
}
# Load
lapply(pkg, library, character.only = TRUE)

############ 2. Load and clean the data ####
setwd('~/Projects/memorialization')
toy <- read.csv("history_toy.csv")
attributes(toy)$subheaders <- toy[1,]
toy <- toy[-c(1:2), -c(1:17)]


########## 3. Validating baseline assumptions ####
cis <-read_sav('dec19.sav')

# from CIS, what is approval for Abascal, what is SD?
#around 2 and 3 seem appropriate
cis$B29_3 <- ifelse(cis$B29_3>10, NA, cis$B29_3)
mean(cis$B29_3, na.rm = T)
sd(cis$B29_3, na.rm = T)


#cis$C8 <- ifelse(cis$C8>95, NA, cis$C8)
#cis$C8 <- ifelse(cis$C8 == 18, 1, 0)
#mean(cis$C8, na.rm = T)
#sd(cis$C8, na.rm = T)



############# 4. Simulate Data and run a regression####
set.seed(1234)

#function for simulating data
simulate_data <- function(N, effects){
  #initialize data frame
  df <- data.frame(matrix(ncol = 20, nrow = N))
  names(df) <- names(toy)
  
  #covariates
  df$sex <- sample(c(1,2), N, replace = T)
  df$age <- sample(c(1:6), N, replace = T)
  df$education <- sample(c(1:6), N, replace = T)
  df$region_1 <- sample(c(1:19), N, replace = T)
  df$ideology_1 <- sample(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 7), N, replace = T)
  
  #treatment
  df$treat <- sample(c(0:5), N, replace = T)
  df$normal <- case_when(df$treat == 0 ~ 0,
                         df$treat == 1 ~ 1,
                         df$treat == 2 ~ 0,
                         df$treat == 3 ~ 1,
                         df$treat == 4 ~ 0,
                         df$treat == 5 ~ 1)
  
  df$history <- case_when(df$treat == 0 ~ 0,
                          df$treat == 1 ~ 0,
                          df$treat == 2 ~ 1,
                          df$treat == 3 ~ 1,
                          df$treat == 4 ~ NA,
                          df$treat == 5 ~ NA)
  
  df$lgbtq <- case_when(df$treat == 0 ~ 0,
                        df$treat == 1 ~ 0,
                        df$treat == 2 ~ NA,
                        df$treat == 3 ~ NA,
                        df$treat == 4 ~ 1,
                        df$treat == 5 ~ 1)
  
  #Simulate treatment effects
  ### hypothesize average sympathy for vox is 3
  base <- effects[1]
  ### hypothesize treatment effect of issue is -0.5
  history_tau <- effects[2]
  ### hypothesize treatment effect of normalization is 0.7
  normal_tau <- effects[3]
  ### hypothesize that treatment interaction effect is 0.4
  interact_tau <- effects[4]
  ### hypothesize treatment effect of lgbtq is -0.2
  lgbtq_tau <- effects[5]
  
  ### hypothesize standard deviation in outcome (e_i)
  sd_outcome <- effects[6]
  
  
  #simulate treatment
  for (i in 1:nrow(df)){
    if (df$treat[i] <= 3){
      df$sympathy_1[i] <- 
        rnorm(1, mean = (base + normal_tau*df$normal[i] +
                           history_tau*df$history[i] +
                           interact_tau*df$normal[i]*df$history[i]), sd = sd_outcome)
    } else {
      df$sympathy_1[i] <- rnorm(1, mean = (base + normal_tau*df$normal[i] +
                                             lgbtq_tau*df$lgbtq[i]), sd = sd_outcome)
    }
  }
  
  #keep all outcomes within bounds of possibility (0:10)
  for (i in 1:nrow(df)){
    if(df$sympathy_1[i] < 0){
      df$sympathy_1[i] <- 0
    } else if(df$sympathy_1[i] > 10){
      df$sympathy_1[i] <- 10
    } else{
      df$sympathy_1[i]
    }
  }
  
  return(df)
}

# Analyze for one simulation
effects <- c(3,    #baseline
             -0.5, #history effect
             0.7,  #normalization effect
             0.4,  #interaction effect
             -0.2, #lgbtq effect
             2)    #sd in outcome

#test: simulate some data
sim <- simulate_data(2400, effects)

#extract relevant coefficients
sympathy_coef <- as.data.frame(coef(summary(lm(sympathy_1 ~ normal + history + normal*history, data = sim))))
names(sympathy_coef) <- c("estimate", "sd", "tval", "pval")
sympathy_coef$variable <- row.names(sympathy_coef)

ggplot(data = sympathy_coef[-c(1),]) +
  geom_pointrange(aes(x = estimate, xmin = estimate-1.96*sd, xmax = estimate + 1.96*sd, y = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(title = "Regression coefficients",
       y = "variable", 
       x = "treatment effect")



######### 5. RUN SIMULATIONS (power calcs) ####

#create df to store results
simulations <- data.frame(
  n = seq(100, 5100, 1000),
  sig_results_4  = rep(NA, length(n)),
  sig_results_2 = rep(NA, length(n))
)

#hypothesize [LARGER] effect sizes
effects <- c(3,    #baseline
             -0.9, #history effect
             1.1,  #normalization effect
             0.8,  #interaction effect
             -0.5, #lgbtq effect
             2)    #sd in outcome

#simulate results, calculate power
for(i in 1:nrow(simulations)){
  sig_int <- 0
  for (j in 1:100){ #note: just running 100 simulations for now
    sim <- simulate_data(simulations[i,1], effects)
    sig_int <- sig_int + 
      ifelse(coef(summary(lm(sympathy_1 ~ 
                               normal + history + normal*history, data = sim)))[4,4] < 0.10, 1, 0)
  }
  simulations[i,2] <- sig_int/100
}

#hypothesize [SMALLER] effect sizes
effects <- c(3,    #baseline
             -0.5, #history effect
             0.7,  #normalization effect
             0.4,  #interaction effect
             -0.2, #lgbtq effect
             2)    #sd in outcome

#simulate results, calculate power
for(i in 1:nrow(simulations)){
  sig_int <- 0
  for (j in 1:100){ #note: just running 100 simulations for now
    sim <- simulate_data(simulations[i,1], effects)
    sig_int <- sig_int + 
      ifelse(coef(summary(lm(sympathy_1 ~ 
                               normal + history + normal*history, data = sim)))[4,4] < 0.10, 1, 0)
  }
  simulations[i,3] <- sig_int/100
}

############# 6. Plot power calculations ####

cols <- c("H1 (d = 0.4)"= "darkred",
          "H1 (d = 0.2)"= "grey50")


ggplot(data = simulations) +
  geom_point(aes(x = n, y = sig_results_4, color = "H1 (d = 0.4)"), shape = 16, size = 2) + 
  geom_smooth(aes(x = n, y = sig_results_4, color = "H1 (d = 0.4)"), se = F, method = "loess") +
  
  geom_point(aes(x = n, y = sig_results_2, color = "H1 (d = 0.2)"), shape = 16, size = 2) + 
  geom_smooth(aes(x = n, y = sig_results_2, color = "H1 (d = 0.2)"), se = F, method = "loess") +
  
  geom_hline(yintercept = 0.8, linetype = "dashed", size = 1) +
  scale_x_continuous(limits = c(0, 5200), breaks = seq(0, 5000, 1000))+
  scale_color_manual(values = cols)+
  theme_bw()+
  labs(title = "Simulating Significance on Interaction Term",
       x = "Sample Size Required (total)", y = "Power") +
  theme(legend.title = element_blank())





