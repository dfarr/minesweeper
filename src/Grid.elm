module Grid exposing
  ( Grid, Coord
  , repeat, indexToCoord, coordToIndex, withCoord
  , map, indexedMap, mapRelationship, indexedMapRelationship, mapIdentity
  )

import List.Extra


-- MODELS

type alias Grid a
  = List (List a)

type alias Coord
  = ( Int, Int )

type alias F a b
  = ( Coord, a ) -> b

type Relationship
  = Identity
  | Neighbor
  | Neither


-- FUNCTIONS

repeat : Int -> Int -> a -> Grid a
repeat m n =
  List.repeat (m * n) >> List.Extra.groupsOf n

indexToCoord : Int -> Int -> Coord
indexToCoord m i =
  ( remainderBy m i, i // m )

coordToIndex : Int -> Coord -> Int
coordToIndex m ( i, j ) =
  m * j + i

withCoord : (Coord -> a -> b) -> Int -> Int -> a -> b
withCoord f i j =
  f ( i, j )

relationship : Coord -> Coord -> Relationship
relationship ( i1, j1 ) ( i2, j2 ) =
  case ( i1-i2 |> abs, j1-j2 |> abs ) of
    ( 0, 0 ) -> Identity
    ( 0, 1 ) -> Neighbor
    ( 1, 0 ) -> Neighbor
    ( 1, 1 ) -> Neighbor
    _ -> Neither

mapRelationship : (a -> b) -> (a -> b) -> (a -> b) -> Coord -> Coord -> a -> b
mapRelationship f1 f2 f3 =
  indexedMapRelationship (f1 << Tuple.second) (f2 << Tuple.second) (f3 << Tuple.second)

indexedMapRelationship : F a b -> F a b -> F a b -> Coord -> Coord -> a -> b
indexedMapRelationship f1 f2 f3 c1 c2 =
  case relationship c1 c2 of
    Identity ->
      f1 << Tuple.pair c2
    Neighbor ->
      f2 << Tuple.pair c2
    Neither ->
      f3 << Tuple.pair c2

map : (a -> b) -> (a -> b) -> (a -> b) -> Coord -> Grid a -> Grid b
map f1 f2 f3 =
  indexedMap (f1 << Tuple.second) (f2 << Tuple.second) (f3 << Tuple.second)

indexedMap : F a b -> F a b -> F a b -> Coord -> Grid a -> Grid b
indexedMap f1 f2 f3 c1 =
  withCoord (indexedMapRelationship f1 f2 f3 c1)
    >> List.indexedMap
    |> List.indexedMap

mapIdentity : a -> Coord -> Grid a -> Grid a
mapIdentity x =
  indexedMap (always x) Tuple.second Tuple.second
