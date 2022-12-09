module Day05 exposing (part1, part2)

{-| Day 5: <https://adventofcode.com/2022/day/5>

Given input representing stacks of crates and instructions like this:

        [D]
    [N] [C]
    [Z] [M] [P]
     1   2   3

    move 1 from 2 to 1
    move 3 from 1 to 3
    move 2 from 2 to 1
    move 1 from 1 to 2

Perform the list of moves and return the new top of each stack.

In part 1, the crates should be moved one at a time.
For example, after performing an instruction of:

    move 2 from 2 to 3

the resulting stacks should look like this:

            [C]
    [N]     [D]
    [Z] [M] [P]
     1   2   3

with resulting top crates being: NMC

In part 2, the crates are moved all at once.
For example, that same instruction would result in:

            [D]
    [N]     [C]
    [Z] [M] [P]
     1   2   3

with resulting top crates being: NMD

-}

import Array exposing (Array)
import Parser as P exposing ((|.), (|=))


type Slot
    = Crate String
    | Empty


type alias Stacks =
    Array (List Slot)


type alias Move =
    { count : Int
    , from : Int
    , to : Int
    }


type Part
    = Part1
    | Part2


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
        |> Maybe.map (\m -> { m | from = m.from - 1, to = m.to - 1 })


part1 =
    solve Part1


part2 =
    solve Part2


solve part =
    let
        sections =
            String.split "\n\n" input
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

        getStack i =
            crateSection
                |> List.map (String.padRight ((count * 4) - 1) ' ')
                |> List.map (String.dropLeft ((i - 1) * 4))
                |> List.filterMap toSlot
                |> List.filter ((/=) Empty)

        stacks =
            cols
                |> List.map getStack
                |> Array.fromList

        instructions =
            sections
                |> List.reverse
                |> List.head
                |> Maybe.withDefault []

        moves =
            List.filterMap toMove instructions
    in
    moves
        |> doMoves part stacks
        |> Array.toList
        |> List.filterMap List.head
        |> List.map unWrap
        |> String.join ""


unWrap : Slot -> String
unWrap slot =
    case slot of
        Crate c ->
            c

        Empty ->
            ""


doMoves : Part -> Stacks -> List Move -> Stacks
doMoves part stacks moves =
    case moves of
        move :: rest ->
            let
                newStacks =
                    doMove part stacks move
            in
            case rest of
                [] ->
                    newStacks

                _ ->
                    doMoves part newStacks rest

        [] ->
            stacks


doMove : Part -> Stacks -> Move -> Stacks
doMove part stacks move =
    let
        from =
            Array.get move.from stacks
                |> Maybe.withDefault []

        newFrom =
            List.drop move.count from

        crates =
            List.take move.count from
                |> (case part of
                        Part1 ->
                            List.reverse

                        Part2 ->
                            identity
                   )

        to =
            Array.get move.to stacks
                |> Maybe.withDefault []

        newTo =
            crates ++ to

        newCrates =
            stacks
                |> Array.set move.to newTo
                |> Array.set move.from newFrom
    in
    newCrates


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


input : String
input =
    """
    [C]             [L]         [T]
    [V] [R] [M]     [T]         [B]
    [F] [G] [H] [Q] [Q]         [H]
    [W] [L] [P] [V] [M] [V]     [F]
    [P] [C] [W] [S] [Z] [B] [S] [P]
[G] [R] [M] [B] [F] [J] [S] [Z] [D]
[J] [L] [P] [F] [C] [H] [F] [J] [C]
[Z] [Q] [F] [L] [G] [W] [H] [F] [M]
 1   2   3   4   5   6   7   8   9 

move 1 from 5 to 6
move 5 from 6 to 7
move 10 from 7 to 3
move 4 from 8 to 4
move 2 from 5 to 4
move 4 from 3 to 6
move 6 from 2 to 4
move 8 from 6 to 9
move 5 from 9 to 2
move 7 from 2 to 7
move 2 from 1 to 4
move 3 from 3 to 8
move 1 from 5 to 9
move 1 from 3 to 8
move 1 from 1 to 2
move 11 from 4 to 6
move 2 from 5 to 6
move 10 from 9 to 1
move 4 from 8 to 3
move 7 from 7 to 1
move 9 from 1 to 2
move 1 from 6 to 5
move 1 from 5 to 9
move 5 from 3 to 8
move 2 from 9 to 1
move 5 from 3 to 9
move 3 from 6 to 8
move 5 from 9 to 6
move 6 from 6 to 3
move 3 from 3 to 2
move 1 from 9 to 8
move 13 from 2 to 3
move 3 from 8 to 1
move 11 from 1 to 4
move 3 from 4 to 1
move 2 from 6 to 5
move 4 from 6 to 8
move 17 from 3 to 9
move 1 from 1 to 8
move 1 from 6 to 5
move 1 from 3 to 7
move 1 from 7 to 4
move 3 from 4 to 1
move 1 from 3 to 8
move 4 from 8 to 1
move 3 from 5 to 9
move 1 from 6 to 4
move 4 from 4 to 8
move 2 from 8 to 4
move 2 from 1 to 6
move 4 from 8 to 6
move 1 from 8 to 3
move 6 from 6 to 3
move 6 from 3 to 9
move 6 from 1 to 4
move 5 from 8 to 4
move 1 from 3 to 6
move 3 from 1 to 7
move 1 from 6 to 7
move 4 from 4 to 5
move 24 from 9 to 5
move 2 from 9 to 1
move 27 from 5 to 7
move 13 from 7 to 2
move 1 from 5 to 9
move 7 from 2 to 7
move 1 from 9 to 8
move 5 from 2 to 8
move 1 from 2 to 5
move 1 from 5 to 7
move 21 from 4 to 1
move 1 from 4 to 6
move 1 from 6 to 5
move 22 from 7 to 5
move 2 from 7 to 8
move 7 from 5 to 4
move 1 from 4 to 5
move 2 from 7 to 9
move 5 from 5 to 2
move 5 from 4 to 2
move 3 from 5 to 1
move 7 from 8 to 7
move 1 from 4 to 1
move 23 from 1 to 8
move 2 from 9 to 4
move 11 from 8 to 3
move 3 from 1 to 3
move 1 from 4 to 2
move 12 from 3 to 2
move 7 from 7 to 3
move 3 from 2 to 1
move 1 from 4 to 9
move 1 from 1 to 3
move 9 from 8 to 6
move 2 from 5 to 4
move 3 from 1 to 7
move 3 from 2 to 4
move 7 from 2 to 3
move 9 from 3 to 4
move 7 from 5 to 2
move 2 from 7 to 2
move 1 from 7 to 2
move 13 from 4 to 6
move 1 from 9 to 8
move 2 from 8 to 2
move 12 from 2 to 1
move 3 from 3 to 1
move 1 from 8 to 1
move 5 from 3 to 7
move 3 from 2 to 8
move 7 from 2 to 5
move 3 from 8 to 3
move 1 from 4 to 8
move 22 from 6 to 4
move 1 from 3 to 6
move 3 from 5 to 8
move 4 from 5 to 8
move 1 from 3 to 9
move 8 from 4 to 2
move 8 from 8 to 3
move 1 from 6 to 3
move 4 from 2 to 6
move 1 from 9 to 4
move 5 from 3 to 9
move 2 from 8 to 1
move 3 from 2 to 1
move 10 from 4 to 8
move 4 from 7 to 6
move 10 from 1 to 3
move 9 from 8 to 2
move 1 from 7 to 1
move 15 from 3 to 1
move 1 from 8 to 9
move 4 from 4 to 1
move 17 from 1 to 3
move 3 from 2 to 3
move 3 from 6 to 8
move 5 from 9 to 7
move 11 from 1 to 8
move 4 from 7 to 8
move 6 from 2 to 5
move 2 from 1 to 4
move 4 from 6 to 8
move 16 from 8 to 6
move 2 from 6 to 1
move 1 from 9 to 5
move 1 from 7 to 5
move 2 from 5 to 6
move 5 from 6 to 3
move 2 from 8 to 5
move 1 from 2 to 1
move 10 from 6 to 3
move 6 from 5 to 9
move 2 from 1 to 2
move 2 from 4 to 2
move 1 from 2 to 4
move 5 from 9 to 2
move 1 from 4 to 3
move 1 from 9 to 7
move 1 from 6 to 1
move 1 from 1 to 7
move 2 from 7 to 5
move 7 from 2 to 5
move 6 from 5 to 1
move 1 from 2 to 3
move 1 from 4 to 1
move 2 from 8 to 9
move 8 from 1 to 3
move 2 from 5 to 3
move 29 from 3 to 9
move 5 from 3 to 8
move 6 from 8 to 5
move 1 from 6 to 5
move 6 from 3 to 2
move 2 from 2 to 4
move 1 from 1 to 7
move 18 from 9 to 6
move 2 from 2 to 9
move 2 from 2 to 8
move 13 from 6 to 8
move 1 from 7 to 4
move 3 from 5 to 6
move 1 from 5 to 7
move 1 from 7 to 4
move 14 from 9 to 3
move 3 from 4 to 5
move 1 from 9 to 7
move 14 from 3 to 2
move 1 from 7 to 3
move 4 from 2 to 5
move 16 from 8 to 6
move 11 from 6 to 9
move 13 from 6 to 4
move 5 from 5 to 2
move 12 from 2 to 4
move 19 from 4 to 3
move 7 from 4 to 5
move 14 from 5 to 2
move 2 from 3 to 6
move 3 from 9 to 5
move 2 from 6 to 2
move 1 from 5 to 2
move 3 from 5 to 4
move 3 from 4 to 1
move 7 from 9 to 6
move 4 from 6 to 1
move 1 from 1 to 8
move 3 from 6 to 9
move 1 from 8 to 7
move 1 from 9 to 6
move 4 from 1 to 2
move 1 from 7 to 2
move 2 from 9 to 8
move 10 from 2 to 9
move 2 from 2 to 9
move 11 from 3 to 7
move 1 from 8 to 9
move 2 from 3 to 7
move 1 from 1 to 7
move 10 from 2 to 4
move 3 from 4 to 1
move 4 from 1 to 8
move 1 from 6 to 5
move 6 from 7 to 9
move 3 from 9 to 1
move 1 from 5 to 1
move 4 from 4 to 2
move 5 from 2 to 1
move 1 from 2 to 7
move 2 from 7 to 6
move 1 from 2 to 1
move 2 from 9 to 1
move 3 from 4 to 7
move 1 from 3 to 7
move 2 from 8 to 3
move 2 from 6 to 5
move 2 from 5 to 8
move 10 from 7 to 2
move 6 from 9 to 1
move 1 from 7 to 3
move 2 from 8 to 9
move 7 from 3 to 7
move 7 from 3 to 9
move 1 from 8 to 9
move 6 from 2 to 8
move 13 from 9 to 1
move 6 from 9 to 8
move 2 from 2 to 7
move 3 from 7 to 1
move 1 from 8 to 1
move 1 from 1 to 6
move 16 from 1 to 4
move 2 from 7 to 5
move 12 from 4 to 9
move 4 from 8 to 6
move 2 from 5 to 1
move 8 from 8 to 4
move 2 from 4 to 5
move 1 from 8 to 6
move 4 from 6 to 8
move 19 from 1 to 9
move 3 from 8 to 5
move 1 from 6 to 9
move 2 from 2 to 1
move 10 from 4 to 9
move 1 from 1 to 2
move 2 from 1 to 5
move 4 from 7 to 9
move 1 from 8 to 2
move 1 from 2 to 6
move 7 from 5 to 4
move 11 from 9 to 8
move 1 from 4 to 3
move 10 from 8 to 1
move 1 from 2 to 3
move 29 from 9 to 3
move 2 from 6 to 5
move 1 from 5 to 3
move 5 from 9 to 3
move 1 from 8 to 9
move 1 from 9 to 3
move 6 from 4 to 6
move 1 from 5 to 1
move 1 from 6 to 3
move 2 from 1 to 5
move 1 from 9 to 5
move 37 from 3 to 2
move 3 from 6 to 2
move 1 from 6 to 2
move 1 from 6 to 4
move 3 from 1 to 3
move 2 from 1 to 6
move 35 from 2 to 1
move 1 from 6 to 8
move 5 from 1 to 8
move 7 from 1 to 6
move 5 from 3 to 7
move 1 from 8 to 7
move 3 from 7 to 5
move 4 from 2 to 9
move 1 from 2 to 1
move 1 from 4 to 3
move 3 from 7 to 1
move 1 from 3 to 6
move 1 from 1 to 9
move 5 from 9 to 2
move 18 from 1 to 3
move 6 from 1 to 8
move 6 from 3 to 7
move 4 from 8 to 6
move 4 from 6 to 7
move 9 from 7 to 8
move 3 from 2 to 7
move 4 from 6 to 1
move 3 from 5 to 3
move 3 from 2 to 5
move 3 from 6 to 1
move 4 from 7 to 4
move 6 from 5 to 9
move 3 from 1 to 9
move 1 from 6 to 1
move 15 from 8 to 2
move 1 from 8 to 5
move 3 from 4 to 8
move 1 from 5 to 1
move 1 from 6 to 5
move 11 from 3 to 9
move 12 from 2 to 3
move 3 from 8 to 1
move 15 from 1 to 2
move 8 from 9 to 4
move 8 from 4 to 9
move 4 from 2 to 5
move 1 from 4 to 6
move 1 from 2 to 8
move 1 from 6 to 7
move 4 from 3 to 1
move 1 from 8 to 5
move 5 from 3 to 9
move 14 from 9 to 2
move 1 from 7 to 4
move 4 from 1 to 3
move 1 from 4 to 7
move 8 from 3 to 7
move 8 from 7 to 5
move 1 from 7 to 9
move 3 from 3 to 2
move 7 from 9 to 8
move 1 from 9 to 5
move 2 from 8 to 5
move 7 from 5 to 4
move 4 from 9 to 2
move 6 from 4 to 3
move 18 from 2 to 5
move 1 from 4 to 7
move 15 from 5 to 4
move 1 from 4 to 6
move 2 from 2 to 7
move 3 from 8 to 5
move 1 from 7 to 3
move 8 from 2 to 6
move 4 from 2 to 3
move 1 from 7 to 5
move 3 from 4 to 6
move 5 from 6 to 9
move 8 from 5 to 6
move 2 from 4 to 3
move 7 from 4 to 2
move 2 from 8 to 5
move 7 from 5 to 6
move 3 from 5 to 8
move 1 from 8 to 9
move 13 from 3 to 8
move 2 from 2 to 7
move 9 from 8 to 9
move 6 from 8 to 5
move 5 from 5 to 2
move 2 from 7 to 8
move 9 from 2 to 5
move 1 from 7 to 5
move 1 from 5 to 7
move 21 from 6 to 2
move 1 from 7 to 8
move 3 from 8 to 9
move 1 from 4 to 2
move 23 from 2 to 7
move 8 from 9 to 8
move 20 from 7 to 4
move 3 from 7 to 2
move 1 from 2 to 7
move 1 from 6 to 7
move 3 from 5 to 4
move 8 from 5 to 9
move 2 from 7 to 1
move 1 from 8 to 7
move 4 from 2 to 4
move 2 from 8 to 7
move 2 from 8 to 2
move 1 from 7 to 6
move 3 from 9 to 7
move 2 from 2 to 7
move 5 from 7 to 1
move 8 from 9 to 6
move 15 from 4 to 3
move 4 from 4 to 7
move 6 from 1 to 4
move 11 from 3 to 4
move 8 from 6 to 1
move 24 from 4 to 7
move 6 from 1 to 8
move 27 from 7 to 3
move 2 from 7 to 8
move 5 from 8 to 3
move 4 from 8 to 4
move 1 from 8 to 6
move 1 from 6 to 9
move 1 from 6 to 5
move 2 from 4 to 2
move 1 from 8 to 1
move 1 from 5 to 2
move 4 from 1 to 6
move 1 from 7 to 5
move 1 from 5 to 8
move 1 from 8 to 7
move 1 from 7 to 8
move 1 from 8 to 1
move 1 from 2 to 3
move 2 from 4 to 8
move 7 from 9 to 6
move 2 from 8 to 1
move 3 from 3 to 8
move 3 from 1 to 8
move 2 from 2 to 3
move 1 from 4 to 1
move 1 from 1 to 8
move 5 from 8 to 3
move 8 from 6 to 2
move 1 from 9 to 4
move 2 from 4 to 8
move 2 from 8 to 3
move 2 from 6 to 2
move 33 from 3 to 2
move 2 from 8 to 7
move 1 from 6 to 1
move 1 from 1 to 7
move 2 from 3 to 8
move 2 from 8 to 4
move 1 from 4 to 8
move 2 from 7 to 2
move 2 from 3 to 7
move 12 from 2 to 1
move 1 from 8 to 4
move 1 from 4 to 8
move 1 from 4 to 3
move 1 from 8 to 2
move 3 from 7 to 2
move 37 from 2 to 7
move 1 from 1 to 7
move 12 from 7 to 1
move 13 from 1 to 7
move 1 from 3 to 4
move 35 from 7 to 6
move 1 from 4 to 5
move 3 from 7 to 4
move 1 from 5 to 7
move 2 from 3 to 4
move 23 from 6 to 9
move 3 from 1 to 5
move 3 from 3 to 7
move 1 from 3 to 6
move 2 from 5 to 3
move 23 from 9 to 8
move 2 from 4 to 9
move 16 from 8 to 2
move 2 from 7 to 3
move 1 from 5 to 8
move 3 from 7 to 6
move 1 from 9 to 8
move 3 from 8 to 1
move 1 from 9 to 1
move 11 from 6 to 5
move 2 from 4 to 1
move 4 from 8 to 6
move 16 from 2 to 3
move 9 from 1 to 9
move 1 from 8 to 4
move 3 from 9 to 3
move 1 from 1 to 4
move 1 from 9 to 4
move 7 from 5 to 2
move 6 from 2 to 5
move 1 from 8 to 6
move 22 from 3 to 7
move 8 from 5 to 8
move 4 from 4 to 9
move 2 from 1 to 8
move 16 from 7 to 2
move 1 from 3 to 5
move 14 from 2 to 7
move 2 from 2 to 4
move 6 from 9 to 3
"""
