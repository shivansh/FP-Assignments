data Point = Point Float Float
type Region = Point -> Bool

circle_maker :: Float -> Region
circle_maker r (Point x y) = x*x + y*y <= r*r

rectangle_maker :: Float -> Float -> Region
rectangle_maker l b (Point x y) = abs (x) <= l/2 && abs (y) <= b/2

not_in :: Region -> Region
not_in r = not . r

intersection :: Region -> Region -> Region
intersection r1 r2 p = r1 p && r2 p

union :: Region -> Region -> Region
union r1 r2 p = r1 p || r2 p

annulus :: Region -> Region -> Region
annulus r1 r2 p = (r1 p && not (r2 p)) || (not (r1 p) && r2 p)

-- Instead of translating the region, translate the point relatively.
translate :: Region -> Point -> Region
translate r (Point x1 y1) (Point x2 y2) = r (Point (x2-x1) (y2-y1))
