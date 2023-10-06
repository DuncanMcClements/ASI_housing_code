library(ggplot2)
library(haven)

#set parameters
alpha <- 0.65
eta <- 0.25
theta <- 1/0.3
beta <- 0.32
housing_data <- read.csv(file = 'C:\\Users\\dunca\\Downloads\\FINAL_ASI_DATA.csv') #data
#correct for population composition
Wnaive <- housing_data[,38]
housing.data.lm<-lm(Median..ASHE.7 ~ Aged.under.15.years + Aged.15.to.64.years + Asian..Bangladeshi + Asian..Chinese + Asian..Indian + Asian..Pakistani + Asian..Other.Asian + Black..African + Black..Caribbean + Black..Other.Black + Mixed..White.and.Asian + Mixed..White.and.Black.African + Mixed..White.and.Black.Caribbean + Mixed..Other.Mixed + White..British + White..Irish + White..Gypsy + White..Roma + White..Other.White + Other.ethnic.group..Arab + X.8 + X0 + X1 + X2 + X3 + X4 + X5, data = housing_data)
summary(housing.data.lm)
regression_results <- c(77.133, 3.703, 0, -23.089, 21.749, -15.172, -10.82, 12.638, -4.827, -14.496, -45.333, -169.165, -78.938, -10.188, 123.792, -13.554, -47.762, 4.176, -97.418, -18.212, -64.725, 0, -4861.323, 1698.54, 2375.223, 1564.301, 747.205, 849.446, 2570.612,0)
W_pred <- c() 
for(i in 1:length(housing_data[,4])){W_pred <- append(W_pred, as.numeric(-187.065 + sum(regression_results * housing_data[i,4:33])))}
W_pred_vs_ac <- data.frame(Wnaive <- Wnaive, W_predicted <- W_pred)
reg<-lm(formula = W_predicted ~ Wnaive, data=W_pred_vs_ac)                      
coeff<-coefficients(reg)          
intercept<-coeff[1]
slope<- coeff[2]
ggplot(W_pred_vs_ac, aes(x = Wnaive, y = W_predicted)) + geom_point() + geom_abline(intercept = intercept, slope = slope) + labs(x = "Median weekly earnings, 2023", y = "Predicted weekly earnings, 2023")

W2023 <- W_pred
L2023 <- housing_data[,37]
P2023 <- housing_data[,41]

P_max <- housing_data[,45] #set to 43 for conservative, 44 for central and 45 for stretch
gamma <- 0 #set to 1/1.75 for conservative, 1/10 for central and 0 for stretch

Z2023_p <- (P2023^beta)/W2023
Z2023_ip <- (L2023^(1/theta))*(P2023^beta)/W2023
A2023 <- L2023*(W2023^((1-eta)/(1-eta-alpha)))

#Perfect mobility
#Existing equilibrium
P_bar <- P2023/((L2023/sum(L2023))^gamma)
eq_L <- (((A2023^(1-eta-alpha))*((Z2023_p/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*gamma*(1-eta)))))/sum(((A2023^(1-eta-alpha))*((Z2023_p/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*gamma*(1-eta)))))
lon_pop <- eq_L[1]
eq_L1 <- eq_L
Q2023 <- (P2023^beta)/Z2023_p
Q_bar <- sum(eq_L*Q2023)
Y2023 <- sum(A2023*((Q_bar/Q2023)^((1-eta)/(1-alpha-eta))))^((1-alpha-eta)/(1-eta))
GDP_norm <- Y2023
V_norm <- alpha * Y2023/Q_bar

#compute counterfactual
P_trunc <- c()
for(i in 1:length(P2023)){
  P_trunc <- append(P_trunc, min(P_max[i], P2023[i]))
}
#Use first P_bar formula for just London: use second for all cities
#P_bar <- c(c(P_max[1], P2023[2:length(P2023)])/((L2023/sum(L2023))^gamma))
P_bar <- c(P_trunc/((L2023/sum(L2023))^gamma))
eq_L <- (((A2023^(1-eta-alpha))*((Z2023_p/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*gamma*(1-eta)))))/sum(((A2023^(1-eta-alpha))*((Z2023_p/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*gamma*(1-eta)))))
Phypo <- P_bar*(eq_L^gamma) #prices given labour distribution
Q2023 <- (Phypo^beta)/Z2023_p
Q_bar <- sum(Q2023*eq_L)
Y2023 <- sum(A2023*((Q_bar/Q2023)^((1-eta)/(1-alpha-eta))))^((1-alpha-eta)/(1-eta))
V <- alpha * Y2023/Q_bar
(Y2023/GDP_norm) #nominal growth
(Y2023/GDP_norm)+(beta*(sum(P2023*eq_L) - sum(P2023*eq_L1))/sum(P2023*eq_L1)) #real growth
eq_L[1]/lon_pop #London growth
eq_L/eq_L1 #all cities change
V/V_norm #welfare growth


#imperfect mobility
#Existing equilibrium
P_bar <- P2023/((L2023/sum(L2023))^gamma)
Z2023_ip <- (P_bar^beta)*(((L2023^(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta))))/(A2023^(1-alpha-eta)))^(1/(1-eta))) #((L2023/sum(L2023))^((1-alpha-eta)/(1-eta))) * (P_bar^beta)/(A2023^((1-alpha-eta)/(1-eta))) #lab_rats^((1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))/(1-eta))
eq_L <- (((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))
eq_L1 <- eq_L
Phypo <- P_bar*(eq_L^gamma)
Q2023 <- (Phypo^beta)/Z2023_ip
Q_bar <- sum(eq_L*Q2023)
eq_L/(L2023/sum(L2023))
W_hypo <- (A2023/eq_L)^((1-alpha-eta)/(1-eta))
GDP_alt <- sum(eq_L * W_hypo)
Vnormalt <- alpha * GDP_alt/Q_bar

#counterfactual
#Use first P_bar formula for just London: use second for all cities
#P_bar <- c(c(P_max[1], P2023[2:length(P2023)])/((L2023/sum(L2023))^gamma))
P_bar <- c(P_trunc/((L2023/sum(L2023))^gamma))
eq_L <- (((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta))))) #eq_L <- (((A2023^(1-alpha-eta))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-alpha-eta))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))
Phypo <- P_bar*(eq_L^gamma)
Q2023 <- (Phypo^beta)/Z2023_ip
Q_bar <- sum(eq_L*Q2023)
W_hypo <- (A2023/eq_L)^((1-alpha-eta)/(1-eta))
Yalt <- sum(eq_L * W_hypo)
Yalt/GDP_alt
(Yalt/GDP_alt) + (beta*(sum(P2023*eq_L) - sum(P2023*eq_L1))/sum(P2023*eq_L1))
eq_L/eq_L1
sum(abs(eq_L-eq_L1)/2)




#welfare calculation - change P_bar_goal for whole UK/just London respectively
P_bar_goal <- c(P_trunc/((L2023/sum(L2023))^gamma))
#P_bar_goal <- c(c(housing_data[1,43], P2023[2:length(P2023)])/((L2023/sum(L2023))^gamma))
P_bar_2023 <- P2023/((L2023/sum(L2023))^gamma)
adjust <- 100000
P_mat <- matrix(ncol = 60, nrow = adjust)
W_mat <- matrix(ncol = 60, nrow = adjust)
L_mat <- matrix(ncol = 60, nrow = adjust)
#compute result from gradual adjustment of prices to estimate welfare change
for(i in 1:adjust){
  P_bar_temp <- (P_bar_goal - P_bar_2023)*(i/adjust) + P_bar_2023
  eq_L <- (((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar_temp^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar_temp^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta))))) #eq_L <- (((A2023^(1-alpha-eta))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-alpha-eta))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))
  Phypo <- P_bar_temp*(eq_L^gamma)
  Q2023 <- (Phypo^beta)/Z2023_ip
  Q_bar <- sum(eq_L*Q2023)
  W_hypo <- (A2023/eq_L)^((1-alpha-eta)/(1-eta))
  P_mat[i,] <- Phypo
  W_mat[i,] <- W_hypo
  L_mat[i,] <- eq_L
}
delta_L <- L_mat[2:adjust,] - L_mat[1:(adjust-1),]
#for group who move, pre-migration
#At each step work out previous welfare, assume equal of each going
welfare_gain_pre_migration <- matrix(ncol = 60, nrow = adjust-1)
for(i in 1:(adjust-1)){
  for(j in 1:60){
    if(delta_L[i,j] < 0){
      welfare_gain_pre_migration[i,j] <- (-delta_L[i,j])*((W_mat[i,j]/(P_mat[i,j]^beta))/(W_mat[1,j]/(P_mat[1,j]^(beta))) -1)
    }else{
      welfare_gain_pre_migration[i,j] <- 0
    }
  }
}
#for group who move, post-migration
welfare_gain_post_migration <- matrix(ncol = 60, nrow = adjust-1)
for(i in 1:(adjust-1)){
  for(j in 1:60){
    if(delta_L[i,j] > 0){
      welfare_gain_post_migration[i,j] <- delta_L[i,j]*((W_mat[adjust,j]/(P_mat[adjust,j]^beta))/(W_mat[i,j]/(P_mat[i,j]^(beta))) -1)
    }else{
      welfare_gain_post_migration[i,j] <- 0
    }
  }
}
sum(welfare_gain_post_migration)
#for group who don't move
eq_L_remaining <- c()
eq_L <- (((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar_goal^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-eta-alpha))*((Z2023_ip/(P_bar_goal^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta))))) #eq_L <- (((A2023^(1-alpha-eta))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))/sum(((A2023^(1-alpha-eta))*((Z2023_ip/(P_bar^beta))^(1-eta)))^(1/(1-alpha-eta+(beta*(gamma+(1/theta))*(1-eta)))))
W_hypo <- (A2023/eq_L)^((1-alpha-eta)/(1-eta))
W_original <- (A2023/(L2023/sum(L2023)))^((1-alpha-eta)/(1-eta))
P_hypo <- P_bar*(eq_L^gamma)
for(i in 1:60){
  eq_L_remaining <- append(eq_L_remaining, min(eq_L[i], eq_L1[i]))
}
#compute resulting welfare growth
delta_remaining_welfare <- sum(eq_L_remaining*(((W_hypo/(P_hypo^beta))/(W_original/(P2023^beta))) - 1))
delta_remaining_welfare + sum(welfare_gain_post_migration) + sum(welfare_gain_pre_migration)