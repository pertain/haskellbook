letThreeTimesPlusY =
    let
        x = 3
        y = 1000
    in
        x * 3 + y

letFiveTimes =
    let
        y = 10
        x = 10 * 5 + y
    in
        x * 5

letZoverXplusY =
    let
        x = 7
        y = negate x
        z = y * 10
    in
        z / x + y

whereThreeTimesPlusY = x * 3 + y
    where
        x = 3
        y = 1000

whereFiveTimes = x * 5
    where
        y = 10
        x = 10 * 5 + y

whereZoverXplusY = z / x + y
    where
        x = 7
        y = negate x
        z = y * 10
