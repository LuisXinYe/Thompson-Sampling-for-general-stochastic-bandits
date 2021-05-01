# a generalized stochastic bandits Thompson Sampling algorithm

output<-{}
b_Probs<-c(0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)
b_Var<-c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
b_Sent<-rep(0, length(b_Probs))
b_Reward<-rep(0, length(b_Probs))

batch_size<-1
N<-2000
steps<-floor(N/batch_size)
msgs<-length(b_Probs)

for (i in 1:steps) {
  B<-matrix(rbeta(msgs, b_Reward+1, (b_Sent-b_Reward)+1),1, byrow = TRUE)
  P<-table(factor(which.min(B), levels=1:ncol(B)))/dim(B)[1]
  # tmp are the weights for each time step
  tmp<-round(P*batch_size,0)
  
  # Update the Rewards
  
  b_Probs.tmp <- rbeta(msgs, (b_Probs*b_Var)/(b_Probs-b_Var), (1-b_Probs)*b_Var/(b_Probs-b_Var))
  b_Reward<-b_Reward+rbinom(rep(1,msgs), size=tmp, prob = b_Probs.tmp)
  
  # Update the Sent
  b_Sent<-b_Sent+tmp
  
  # print(P)
  output<-rbind(output, t(matrix(P)))
}

# the weights of every step
# output
colSums(output)
