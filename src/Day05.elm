module Day05 exposing (part1)

import Parser as P exposing ((|.), (|=))


type Slot
    = Crate String
    | Empty


type alias Move =
    { count : Int
    , from : Int
    , to : Int
    }


toSlot : String -> Maybe Slot
toSlot line =
    let
        parser =
            P.oneOf
                [ P.succeed Empty
                    |. P.token "   "
                , P.succeed Crate
                    |. P.symbol "["
                    |= P.getChompedString (P.chompUntil "]")
                ]
    in
    P.run parser line
        |> Result.toMaybe


toMove : String -> Maybe Move
toMove line =
    let
        parser =
            P.succeed Move
                |. P.keyword "move"
                |. P.spaces
                |= P.int
                |. P.spaces
                |. P.keyword "from"
                |. P.spaces
                |= P.int
                |. P.spaces
                |. P.keyword "to"
                |. P.spaces
                |= P.int
    in
    P.run parser line
        |> Result.toMaybe


part1 =
    let
        sections =
            String.split "\n\n" sample
                |> List.map String.lines
                |> List.map (List.filter ((/=) ""))

        crateSection =
            sections
                |> List.head
                |> Maybe.withDefault []

        cols =
            crateSection
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
                |> String.words
                |> List.filterMap String.toInt

        count =
            cols
                |> List.maximum
                |> Maybe.withDefault 0

        getColumn i =
            crateSection
                |> List.map (String.padRight ((count * 4) - 1) ' ')
                |> List.map (String.dropLeft ((i - 1) * 4))
                |> List.filterMap toSlot
                |> List.filter ((/=) Empty)

        crates =
            List.map getColumn cols

        instructions =
            sections
                |> List.reverse
                |> List.head
                |> Maybe.withDefault []

        moves =
            List.filterMap toMove instructions
    in
    ( crates, moves )


sample : String
sample =
    """
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""
