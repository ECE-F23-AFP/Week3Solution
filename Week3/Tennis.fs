
module Week3.Tennis

open System

type Player = {id: int; point: int}
type Game = (Player*Player)

let increasePoint player = {player with point = player.point + 1}

let addPoint game winner =    
    match game with
    | (p1, p2) when winner = p1.id -> (increasePoint p1, p2)
    | (p1, p2) when winner = p2.id -> (p1, increasePoint p2)
    | _ -> failwith ("Id " + string winner + " not part of game")
    
let printPlayer player =
    let score = match player.point with 
                | 0 -> "zero"
                | 1 -> "fifteen"
                | 2 -> "thirty"
                | 3 -> "forty"
                | _ -> failwith "Function supports standard points"
    ("Player: " + string player.id + " score: " + score)

    
let printGame game =
    match game with
    | (p1, p2) when p1.point < 4 && p2.point < 4 -> printPlayer p1 + "\n" + printPlayer p2
    | (p1, p2) when p1.point = p2.point          -> "deuce"
    | (p1, p2) when p1.point + 1 = p2.point      -> ("Player: " + string p2.id + " advantage")
    | (p1, p2) when p1.point = p2.point + 1      -> ("Player: " + string p1.id + " advantage")
    | (p1, p2)                                   -> ("Player: " + string (if p1.point > p2.point then p1.id else p2.id) + " wins")

let hasWinner = function
    | (p1, p2) when p1.point + 1 < p2.point && p2.point > 3 -> true
    | (p1, p2) when p1.point > p2.point + 1 && p1.point > 3 -> true
    | _                                                     -> false
    
let pointPlayed game winner =
    addPoint game winner |> printGame
    
let playGame () =
    let rec playGameRec game =
        Console.WriteLine "Type winner (1, 2)"
        let winner = Convert.ToInt32(Console.ReadLine())
        let game' = addPoint game winner
        Console.WriteLine (printGame game')
        match hasWinner game' with
        | false -> playGameRec game'
        | true -> ()
   
    playGameRec ({id = 1; point = 0}, {id = 2; point = 0})
    
let test () =
    let value = Convert.ToInt32(Console.Read())
    Console.WriteLine(value)
