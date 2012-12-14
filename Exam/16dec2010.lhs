1a.

> f = (5+) . (8/)

> g = flip ((+) . (3*))



2.

> f2 f xs = map (f. (+4)) $ filter (>5) xs


3.

data digits doesn't derive Enum which is required


4.

pure = no side effects

5.

["christmas","christmas","christmas","christmas","christmas"]

6.

