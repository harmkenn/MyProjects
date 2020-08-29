i <- 1
for (r in 2:nrow(MFValue)){
  j <- 1
  if (MFValue$Date[r] < Etrade_Tx$Date[i]){
    MFValue$inv[r] <- MFValue$inv[r-1]
  } else if (MFValue$Date[r] == Etrade_Tx$Date[i]){
    for(j in 1:20){
      if (j == 1 && MFValue$Date[r] == Etrade_Tx$Date[i]){
        MFValue$inv[r] <- MFValue$inv[r-1] - Etrade_Tx$Amount[i]
        i = i + 1
      } else if (j > 1 && MFValue$Date[r] == Etrade_Tx$Date[i]){
        MFValue$inv[r] <- MFValue$inv[r] - Etrade_Tx$Amount[i]
        i = i + 1
      }
    }
  }
}