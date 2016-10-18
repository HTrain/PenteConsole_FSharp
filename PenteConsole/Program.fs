open System

type Move =
    {
        Coordinates : (int * int)
        Player : string
        IsCaptured : bool
    }

let makeMove (move : Move) (previousMoves :  List<Move>) =
    move :: previousMoves

let parseCoordinates (input: string) =
    let coords = input.Split ','
    let x : int = int32 coords.[0]
    let y : int = int32 coords.[1]
    (x, y)

let getPlayer (moves : List<Move>) =
    if moves.Length % 2 = 0 then "white"
    else "black"

let up (x, y) = (x, y + 1)
let down (x, y) = (x, y - 1)

let left (x, y) = (x - 1, y)
let right (x, y) = (x + 1, y)

let upRight (x, y) = (x + 1, y + 1)
let downLeft (x, y) = (x - 1, y - 1)

let upLeft (x, y) = (x - 1, y + 1)
let downRight (x, y) = (x + 1, y - 1)

let checkDirection (x, y) direction = direction (x,y)

let rec getAdjacentMovesBySamePlayer move (moves : List<Move>) adjacentMoves direction1 direction2 =
    let nextCoordInDirection1 = checkDirection move.Coordinates (direction1) 
    let adjMoves = List.filter (fun x -> x.Coordinates = nextCoordInDirection1 && x.Player = move.Player && x.IsCaptured = false) moves 
    if adjMoves.Length = 0 then
        let nextCoordInDirection2 = checkDirection move.Coordinates (direction2)
        let adjMoves = List.filter (fun x -> x.Coordinates = nextCoordInDirection1 && x.Player = move.Player && x.IsCaptured = false) moves
        if adjMoves.Length = 0 then
            adjMoves
        else adjMoves.[0] :: getAdjacentMovesBySamePlayer adjMoves.[0] moves adjMoves direction1 direction2
    else adjMoves.[0] :: getAdjacentMovesBySamePlayer adjMoves.[0] moves adjMoves direction1 direction2

let checkForFiveInARow move (moves : List<Move>) = 
    let adjMoves = getAdjacentMovesBySamePlayer move moves [] up down
    if adjMoves.Length >= 4 then true
    else
        let adjMoves = getAdjacentMovesBySamePlayer move moves [] left right
        if adjMoves.Length >= 4 then true
        else
            let adjMoves = getAdjacentMovesBySamePlayer move moves [] upLeft downRight
            if adjMoves.Length >= 4 then true
            else
                let adjMoves = getAdjacentMovesBySamePlayer move moves [] upRight downLeft
                if adjMoves.Length >= 4 then true
                else false

let removeCaptured coord1 coord2 (moves : List<Move>) = 
    moves |> List.map(fun move -> 
    if (move.Coordinates = coord1 || move.Coordinates = coord2) then { move with IsCaptured = true }
    else move)
  
let checkForCapture move (moves : List<Move>) direction =
    let firstAdjacent = checkDirection move.Coordinates direction
    let adjMoves = List.filter (fun x -> x.Coordinates = firstAdjacent && x.Player <> move.Player && x.IsCaptured = false) moves
    if adjMoves.Length = 1 then
        let secondAdjacent = checkDirection firstAdjacent direction
        let adjMoves = List.filter (fun x -> x.Coordinates = secondAdjacent && x.Player <> move.Player && x.IsCaptured = false) moves
        if adjMoves.Length = 1 then
            let thirdAdjacent = checkDirection secondAdjacent direction
            let adjMoves = List.filter (fun x -> x.Coordinates = thirdAdjacent && x.Player = move.Player) moves
            if adjMoves.Length = 1 then
                let moves = removeCaptured firstAdjacent secondAdjacent moves
                moves
            else moves
        else moves
    else moves

let recordCaptures move moves =    
    let moves = checkForCapture move moves up
    let moves = checkForCapture move moves down
    let moves = checkForCapture move moves left
    let moves = checkForCapture move moves right
    let moves = checkForCapture move moves upRight
    let moves = checkForCapture move moves downLeft
    let moves = checkForCapture move moves upLeft
    let moves = checkForCapture move moves downRight
    moves

let rec validatedMove move moves =
    let x = fst move.Coordinates
    let y = snd move.Coordinates

    if ((moves |> List.filter(fun z -> z.Coordinates = move.Coordinates || z.IsCaptured = true)).Length > 0) || (x > 18 || y > 18) then
        printfn "%A" "That move is invalid, please try again."

        let coordinates = parseCoordinates (Console.ReadLine())

        let (newMove : Move) = 
            { 
                Coordinates = coordinates
                Player = move.Player
                IsCaptured = false
            }
        validatedMove newMove moves
    else
        move                                                  

let mutable continuePlay = true

let rec play (moves : List<Move>) = 
        let player = getPlayer(moves)

        printfn "%A" (String.concat " " [player; "move. Enter coordinates."])

        let coordinates = parseCoordinates (Console.ReadLine())

        let (move : Move) = 
            { 
                Coordinates = coordinates
                Player = player
                IsCaptured = false
            }

        let move = validatedMove move moves 
        let moves = makeMove move moves
        printfn "%A" (String.concat " " [player; "moved on"; move.Coordinates.ToString()])            
        if checkForFiveInARow move moves then
            printfn "%A" (String.concat " " [player; "won!"])
        
        let moves = recordCaptures move moves

        if (moves |> List.filter (fun x -> x.IsCaptured = true && x.Player <> player) ).Length >= 10 then
            printfn "%A" (String.concat " " [player; "won!"])
                                 
        printfn "%A" "Continue? y/n"

        let answer = Console.ReadLine()

        if answer = "n" then
            continuePlay <- false
        else 
            Console.Clear()
            ignore (play(moves))    

[<EntryPoint>]
let main argv =
    let moves = List.empty 
    while continuePlay do
        play(moves)
    0 

 
    

