# JAGS test model
model {
	for (i in 1:N) {
		af[i,1:7]~dmnorm(mu.af[i,1:7], tau.af[1:7,1:7])
		mu.af[i,1:7]<-R%*%t(A)%*%x
		}
		R~dmvnorm(mu.R,tau.R)
			mu.R[1:7]<-c(0.8442,0.809,0.757,0.7249,0.6886,0.6482,0.6037)
			tau.R[1:7,1:7]~dwish(exptd.tau.R[,],7)
		A~dmvnorm
	}