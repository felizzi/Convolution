## module for the area under the qol curve 

#input 
#1 a qol dataset with time evolution
# a time vector
# a survival (density) function


#create vector of QoL, the firs
v_qol <- rev(smooth50)

v_qol <- c(v_qol, rep(v_qol[length(v_qol)],1501-length(v_qol)) )
v_qol_res <- v_qol
#vector of qol integrals 
for (i in 1:1501){
  v_qol_res[i] <- sum(v_qol[1:i])/i
}
