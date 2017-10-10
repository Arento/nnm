separateRoots f maxdepth depth a b 
  | depth == maxdepth = case (f a) * (f b) < 0 of
                    True  -> [(a,b)]
                    False -> []
  | otherwise   =  separateRoots' a middle ++ separateRoots' middle b 
  where 
    separateRoots' = separateRoots f maxdepth (depth+1)
    middle         = (a+b)/2


newton' maxdepth eps f f' depth x 
  | f' x == 0          = (x,  depth, False)
  | abs (x - x') < eps = (x', depth, True)
  | depth == maxdepth  = (x', depth, False)
  | otherwise          = newton' maxdepth eps f f' (depth + 1) x' 
  where x' = x - ((f x) / (f' x))

newton eps f f' a b = newton' 100  eps  f f' 0 b

newtonModified' maxdepth eps f f' depth x x0 
  | f' x0 == 0           = (x,  depth, False)
  | abs (x - x')  < eps = (x', depth, True)
  | depth == maxdepth   = (x', depth, False)
  | otherwise           = newtonModified' maxdepth eps f f' (depth + 1) x' x0
  where x' = x - ((f x) / (f' x0))

newtonModified eps f f' a b = newtonModified' 100 eps f f' 0 b b 


secantMethod' maxdepth eps f depth  x0 x1 
  | abs(x0-x1) < eps  = (x1, depth, True)
  | maxdepth == depth = (x1, depth, False)
  | otherwise = secantMethod' maxdepth eps f (depth+1) x1 x2
  where x2 = x0 - (((f x0) * (x1 - x0)) / ((f x1) - (f x0)))
                            
secantMethod eps f _ a b = secantMethod' 100 eps f 0 a b

bisection' maxdepth eps f depth a b 
  | abs(a-b) < (2*eps)     = ((a+b)/2, depth, True)
  | (f a) * (f middle) < 0 = bisection' maxdepth eps f (depth+1) a middle
  | depth == maxdepth      = ((a+b)/2, depth, False)
  | otherwise              = bisection' maxdepth eps f (depth+1) middle b
  where middle = (a+b)/2

bisection eps f f' a b = bisection' 100 eps f 0 a b

f  x = sqrt (4*x+7) - (3*(cos x))
f' x = 2/(sqrt (4*x+7)) + (3*(sin x))

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

showResults eps f f' intervals method = show ( map (\(a,b) -> method eps f f' a b) intervals)

methods = [(bisection,      "Bisection method"),
           (newton,         "Newton method"),
           (newtonModified, "Modified Newton method"),
           (secantMethod,   "Secant method")]

outputString f f' a b eps = 
  let intervals = separateRoots f 10 0 a b
  in  foldl (\s (method, name) -> s ++ name ++ "\n" ++ (showResults eps f f' intervals method ) ++ "\n\n") "Nonlinear equations\n f = sqrt(4x+7) -3cos(x)\n" methods 

promt s = do
  putStrLn s
  getLine

main = do
  a <- promt "Enter begin"
  b <- promt "Enter end"
  eps <- promt "Enter epsilon"
  putStrLn "123"
  putStrLn (outputString f f' (read a :: Double) (read b :: Double) (read eps ::Double))
    
