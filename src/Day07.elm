module Day07 exposing (part1)

{-| Day 7: <https://adventofcode.com/2022/day/7>

Given a list of `cd` and `ls` commands and their output,
build a file structure and report on file sizes.

-}

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=))


type Line
    = Ascend
    | GoToRoot
    | GoTo String
    | File FileDetails


type alias FileDetails =
    { size : Int
    , name : String
    }


type alias Path =
    List String


type alias Dirs =
    Dict Path (List FileDetails)


sample : String
sample =
    """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""


part1 =
    sample
        |> String.trim
        |> String.lines
        |> List.filterMap parseLine
        |> List.foldl runLine ( [], Dict.empty )
        |> Tuple.second
        |> Dict.map getSize


getSize : Path -> List FileDetails -> Int
getSize _ files =
    files |> List.map .size |> List.sum


parseLine : String -> Maybe Line
parseLine line =
    let
        parser =
            P.oneOf
                [ P.succeed GoToRoot
                    |. P.token "$ cd /"
                , P.succeed Ascend
                    |. P.token "$ cd .."
                , P.succeed GoTo
                    |. P.token "$ cd "
                    |= P.getChompedString (P.chompUntilEndOr "\n")
                , P.map File
                    (P.succeed FileDetails
                        |= P.int
                        |. P.spaces
                        |= P.getChompedString (P.chompUntilEndOr "\n")
                    )
                ]
    in
    P.run parser line
        |> Result.toMaybe


runLine : Line -> ( Path, Dirs ) -> ( Path, Dirs )
runLine line ( cwd, dirs ) =
    case line of
        Ascend ->
            ( cwd |> List.reverse |> List.drop 1 |> List.reverse
            , dirs
            )

        GoToRoot ->
            ( [ "" ], dirs )

        GoTo dir ->
            ( cwd ++ [ dir ], dirs )

        File details ->
            let
                files =
                    Dict.get cwd dirs
                        |> Maybe.withDefault []

                newDirs =
                    Dict.insert cwd
                        (files ++ [ details ])
                        dirs
            in
            ( cwd, newDirs )
