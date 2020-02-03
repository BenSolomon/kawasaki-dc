kdRisk <- function(
  WBC,
  Platelet,
  Hgb,
  AST,
  Na,
  Alb,
  Temp,
  Classic)
    {
    b0 <- -9.093
    bWBC <- -0.042
    bPLAT <- 0.002
    bHGB <- 0.314
    bAST <- 0.005
    bNA <- -0.094
    bALB <- -1.145
    bTEMP <- 0.544
    bCLASSIC <- -0.589
    
    linear <- b0 + 
      bWBC*WBC +
      bPLAT*Platelet +
      bHGB*Hgb +
      bAST*AST +
      bNA*Na +
      bALB*Alb +
      bTEMP*Temp +
      bCLASSIC*Classic
    
    p <- exp(linear)/(1+exp(linear))
    
    return(p)
      
}


