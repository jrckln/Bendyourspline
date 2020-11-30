load("data/nhanes_sample.RData")
#no transformation
Calcium <- nhanes_sample$LBDSCASI[!is.na(nhanes_sample$LBDSCASI)]
Calcium_trans <- fp.scale(Calcium)
#only shift
X <- runif(nrow(nhanes_sample), -1,1)
X_trans <- fp.scale(X)
#only scale
Height <-  nhanes_sample$BMXHT[!is.na(nhanes_sample$BMXHT)]
Height_trans <- fp.scale(Height)
#shift and scale
BPdifference <-  nhanes_sample$BPXSY1 - nhanes_sample$BPXSY2
BPdifference <- BPdifference[!is.na(BPdifference)]
BPdifference_trans <- fp.scale(BPdifference)

data.FP <- list("X"=X,
                "Calcium"=Calcium, 
                "Height"=Height,
                "Blood pressure difference"=BPdifference)


