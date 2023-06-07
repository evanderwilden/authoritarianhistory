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
getwd()
setwd('C:/Users/19785/Documents/Projects/Laia_projects/')
toy <- read.csv("stigmatization/history_toy.csv")
attributes(toy)$subheaders <- toy[1,]
toy <- toy[-c(1:2), -c(1:17)]


########## 3. Validating baseline assumptions ####
cis <-read_sav('replications/oct22.sav')

# from CIS, what is approval for Abascal, what is SD?
cis$abascal <- ifelse(cis$VALORALIDERES_4 >10, NA, cis$VALORALIDERES_4)
mean(cis$abascal, na.rm = T) # 26.1
sd(cis$abascal, na.rm = T) # 23.9


############# 4. Write functions ####
#function for simulating data
simulate_data <- function(N, effects){
  #initialize data frame
  df <- data.frame(matrix(ncol = 20, nrow = N))
  names(df) <- names(toy)
  
  #treatment
  df$treat <- c(rep(0, N*0.2), rep(1, N*0.2),
                     rep(2, N*0.2), rep(3, N*0.2),
                     rep(4, N*0.1), rep(5, N*0.1))
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
    if (df$treat[i] < 4){
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
    } else if(df$sympathy_1[i] > 100){
      df$sympathy_1[i] <- 100
    } else{
      df$sympathy_1[i]
    }
  }
  
  return(df)
}

# Analyze for one simulation
effects <- c(30,    #baseline
             -5, #history effect
             5,  #normalization effect
             5,  #interaction effect
             -2, #lgbtq effect
             23) #sd in outcome

#test: simulate some data
sim <- simulate_data(2400, effects)




######### 5. RUN SIMULATIONS (power calcs) ####
set.seed(1234)
#create df to store results
outputs <- data.frame(
  n = seq(500, 5600, 300),
  sig_int = rep(NA, length(n))
)

#hypothesize [LARGER] effect sizes
effects <- c(30,    #baseline
             -5, #history effect
             5,  #normalization effect
             4.8,  #interaction effect
             -2, #lgbtq effect
             24)   #sd in outcome

calc_h1 <- function(data){
  #store p-values
  results <- 
    c((coef(summary(lm(sympathy_1 ~ normal + history + normal*history, data = data))))[4,4])
  
  #sort from low to high
  results <- sort(results)
  
  #add BH correction
  ifelse(length(which(results < c(.1))) == 1, 1, 0)
}


#simulate 100 data draws
run_power <- function(outputs, first, last){
  #simulate results, calculate power
  for(i in first:last){
    sig_int <- 0

    for (j in 1:500){ #note: just running 100 simulations for now
      sim <- simulate_data(outputs[i,1], effects)
      sig_int <- sig_int + calc_h1(sim)
    }
    outputs[i,2] <- sig_int/500 #calculate proportion that was significant
  }
  
  return(outputs)
}

outputs <- run_power(outputs, 1, 18) #run simulations


############# 6. Plot power calculations ####

ggplot(data = outputs) +
  #geom_point(aes(x = n, y = sig_results_4, color = "H1 (d = 0.4)"), shape = 16, size = 2) + 
  #geom_smooth(aes(x = n, y = sig_results_4, color = "H1 (d = 0.4)"), se = F, method = "loess") +
  
  geom_point(aes(x = n, y = sig_int), shape = 16, size = 2) + 
  geom_smooth(aes(x = n, y = sig_int), se = F) +
  
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1) +
  scale_x_continuous(limits = c(0, 6000), breaks = seq(0, 6000, 1000))+
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 0.8, 0.2), 0.9, 1))+
  theme_bw()+
  labs(title = "Simulating Significance on Interaction Term (H1)",
       subtitle = "Effect size of interaction at d = 0.2",
       x = "Sample Size Required (total)", y = "Power") +
  theme(legend.title = element_blank())





