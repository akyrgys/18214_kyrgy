y x = if (x==(-1)) then error "not scope"
    else x^2/(1+x)
    
y1 x = if (2>=abs(x)) then error "not scope"
    else log(x^2-4)/log(10)
    
y3 x = if (sin(2*x)<0 || sin(3*x)<0) then error "not scope"
    else sqrt(sin(2*x))-sqrt(sin(3*x))