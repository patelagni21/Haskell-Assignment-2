data Poly a = P [a] deriving (Show, Eq)

degree :: Poly a -> Int
degree ( P [] ) = 0
degree ( P x  ) = length x - 1


scale :: (Num a, Eq a) => a -> Poly a -> Poly a
scale scalar (P coefflist) = P ( map ( \coeff -> coeff * scalar ) coefflist )



comp_point_effective :: (Num a) => a -> a -> [a] -> a
comp_point_effective point rp (x:xs)   = (x * rp) + (comp_point_effective  point (rp * point) xs )
comp_point_effective _ _ [] = 0

($$) :: (Num a, Eq a) => Poly a -> a -> a
($$) (P [])        point = 0
($$) (P coefflist) point = comp_point_effective point 1 coefflist


-- Coefficient list extractor

cle :: (Poly a) -> [a]
cle (P cl) = cl
--
addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
addPoly (P[]) y = y
addPoly x (P[]) = x
addPoly (P (x:xs)) (P (y:ys)) = P( (x+y): (cle ( addPoly (P xs) (P ys) ) ) )


incDegree :: (Num a, Eq a) => Poly a -> Poly a
incDegree (P x) = P ( 0: x )

multiPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multiPoly ( P []      ) ( P    p2   ) = P []
multiPoly ( P p1      ) ( P []      ) = P []
multiPoly ( P (x: xs) )        p2     = addPoly ( scale x p2 ) ( incDegree ( multiPoly (P xs) p2 ) )




instance (Num a, Eq a) => Num (Poly a) where
        (+) = addPoly
        negate = scale (-1)
        (*) = multiPoly
        fromInteger 0 = P[]
        fromInteger n = P[ fromInteger n]
        abs = error "No abs"
        signum = error "No sign"

x:: (Num a) => Poly a
x = P [0,1]


y :: (Num a) => Poly (Poly a)
y = P [ P [ 0, 1 ] ]


-- [P [0,0,1],P [],P [1]]
Appendix:



We have implemented the Num interface for our Polynomial data type. Besides regular numeric options, we also implement a `fromInteger` option which allows Haskell to treat another `Num` type as this type. 

Appendix 1: 

x + 1 

Here x is P[0, 1]. Haskell applied the addPoly function and then to satisfy the types of addPoly, treats 1 as Poly. To do so it applies fromInteger and makes it P[1]

x^2-2*x

x^k is treated by Haskell internally as k repeated multiplications of x. Thus the above is equivalent to



addPoly  (multiPoly x x) ( negate ( scale 2 x ) )

which as expected yields P[0, -2,1] (Question incorrectly says it's P[0,-2,2]


Appendix 2:

This is similar to the first part above, so satisfy the type of + infix operator , implicit fromInteger calls are added. A couple of initial calculations:

y^2 

= P[P[0,1]]^2 

= P[ P[ 0,1]^2 ] (the "constant" term of polynomial gets squared)

= P[ P[ 0,0,1]]


y-1

= P [ P [ 0,1]] - (fromInteger 1)

= P [ P [ 0, 1]] - P[1]

= P [ P [ 0,1]] - P [ (fromInteger 1)]

= P [ P [ -1, 1 ] ] (After scaling the second term and adding the "constant" term of the outer polynomial)

x^2+ y^2

P[0,0,1] + P[P[0,0,1]]

= P[ (fromInteger 0), (fromInteger 0), (fromInteger 1) ]     +   P[P[0,0,1]](to satisfy types)

= P[ P[], P[], P[1]] + P[P[0,0,1]]

= P[ P[0,0,1], P[], P[1]]



(x+1)*(y-1)

= P[1,1] * P[P[-1,1]]

= P[ P[1], P[1]] * P[P[-1,1]]   (fromInteger applications to satisfy types)

= P [ P[-1,1], P[-1,1] ] (formally, rules of multiplication; informally, second polynomial only has constant term so scale the first's coefficient by it )
