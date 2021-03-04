-----------------------
-- Truta Gabriela
-- 12.10.2020
-----------------------


--Exercise 1.2.1
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card Face Suit


--Exercise 1.2.2
faceToString: Face -> String
faceToString face = Debug.toString(face)

suitToString: Suit -> String
suitToString suit = Debug.toString(suit)

cardToString: Card -> String
cardToString (Card face suit) = Debug.toString(face) ++ " of" ++ " " ++ Debug.toString(suit)



--Exercise 1.2.3
type Point = Point {x: Float, y: Float}
type Line = Line {x1: Float, y1: Float, x2: Float, y2: Float}

onSegment: Point -> Point -> Point -> Bool
onSegment (Point p) (Point q) (Point r) =
    if ((q.x <= max p.x r.x) && (q.x >= min p.x r.x) &&
        (q.y <= max p.y r.y) && (q.y <= max p.y r.y))
    then
        True
    else
        False

orientation: Point -> Point -> Point -> String
orientation (Point p) (Point q) (Point r) =
    let
        s = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
    in
        if (s == 0) then
            "Collinear"
        else if (s > 0) then
            "Clockwise"
        else
            "Counterclockwise"

linesIntersect: Line -> Line -> Bool
linesIntersect (Line line1) (Line line2) =
    let
        p1 = (Point {x = line1.x1, y = line1.y1})
        p2 = (Point {x = line2.x1, y = line2.y1})
        q1 = (Point {x = line1.x2, y = line1.y2})
        q2 = (Point {x = line2.x2, y = line2.y2})
        o1 = orientation p1 p2 q1
        o2 = orientation p1 q2 q1
        o3 = orientation p2 p1 q2
        o4 = orientation p2 q1 q2
    in
        if (o1 /= o2 && o3 /= o4)
            then True
        else if (o1 == "Collinear" && onSegment p1 p2 q1)
            then True
        else if (o2 == "Collinear" && onSegment p1 q2 q1)
            then True
        else if (o3 == "Collinear" && onSegment p2 p1 q2)
            then True
        else if (o4 == "Collinear" && onSegment p2 q1 q2)
            then True
        else
            False


--Exercise 1.2.4
trailingZeros: Int -> Int
trailingZeros n =
    let
        trailingHelper i acc = if (i < 5) then acc else trailingHelper (i//5) (acc+ i//5)
    in
        trailingHelper n 0