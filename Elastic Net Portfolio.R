weight_enet <- matrix(NA, nrow = 59, ncol = 30)
# Create a weight matrix for elastic net portfolio
for(i in 1:nexpwin){
  retloop = choiceret[1:(minwin+i),]
  oneloop = rep(1,nrow(retloop))
  choice_enet = enet(retloop,oneloop,lambda=1,
                     normalize=FALSE,intercept=FALSE) 
  nasset = order(choice_enet$Cp)[1]
  # if all get zero coeff, use equal weights
  if(nasset==1) beta = rep(1,ncol(choiceret)) 
  else beta = choice_enet$beta.pure[nasset,]
  w = beta/sum(beta)
  weight_enet[i,] = w
}
# Load values into the weight_enet through elsticnet function