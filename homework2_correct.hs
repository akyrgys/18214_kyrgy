quadraticSolver:: (Floating a, Ord a) => a -> a -> a -> (a, a)
quadraticSolver a b c   |a==0 && b/=0 = (cdivb, cdivb)
						|a==0 && b==0 = error "Not correct"
						|d>=0 = (x1, x2)
						|otherwise = error "No real roots"
						where
                         d = b^2-4*a*c
                         x1 = (-b+sqrt(d))/(2*a)
                         x2 = (-b-sqrt(d))/(2*a)
                         cdivb = (-b)/c