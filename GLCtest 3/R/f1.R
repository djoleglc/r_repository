test_leverage_effect=function(model_fitted){
  res=residuals(model_fitted)
  sigma=sigma(model_fitted)
  res_stand=res/sigma
  res_stand2=res_stand^2

  test_c =function(res){
    c= cor(res[-1]^2,res[-length(res)])
    print( "CORRELATION TEST")
    if( c<0){
      print(paste("Possible leverage effect",c))



    }
    else{

      print(paste("No leverage effect",c))

    }

  }
  sign_bias_test=function(res){
    a=res_stand2[-1]
    b=res[-length(res)]
    d=numeric(length(0))
    for(i in 1:length(b)){
      d[i]=ifelse(b[i]<0,1,0)
    }
    fit.lm=lm(a~d)
    print("SIGN BIAS TEST")
    print(coeftest(fit.lm))

  }
  neg_size_test=function(res){
    a=res_stand2[-1]
    b=res[-length(res)]
    d=numeric(length(0))
    for(i in 1:length(b)){
      d[i]=ifelse(b[i]<0,1,0)
    }
    f=b*d
    fit.lm1=lm(a~1+f)
    coeftest(fit.lm1)
    print("NEGATIVE SIZE BIAS TEST")
    print(coeftest(fit.lm1))

  }
  joint_test=function(res){
    a=res_stand2[-1]
    b=res[-length(res)]
    d=numeric(length(0))
    for(i in 1:length(b)){
      d[i]=ifelse(b[i]<0,1,0)
    }
    f=b*d
    fit.lm2=lm(a~1+f+d)
    coeftest(fit.lm2)
    print("JOINT TEST")
    print(coeftest(fit.lm2))

  }
  test_c(res)

  sign_bias_test(res)

  neg_size_test(res)

  joint_test(res)

}
