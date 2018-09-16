f :: Float -> Float -> Float -> (Float, Float)
f a b c | a==0 = error "not correct"
        | b^2-4*a*c==0  = (-b/(2*a), -b/(2*a))
        | b^2-4*a*c>0 =  ((-b+sqrt(b^2-4*a*c))/(2*a), (-b-sqrt(b^2-4*a*c))/(2*a))
        | b^2-4*a*c<0 = error "there is no real roots"