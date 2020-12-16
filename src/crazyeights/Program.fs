module CrazyEights
// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type Rank =
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King

type Suit = Club | Spade | Diamond | Heart

type Card =
    { Rank: Rank
      Suit: Suit }

let (^) rank suit = { Rank = rank; Suit = suit }

[<Struct>]
type Player = Player of int

type Effect =
    | Next
    | Skip
    | Back
    | Interrupt
    | BreakingInterrupt of Player

[<Struct>]
type Players = private Players of int

type Direction =
    | Clockwise
    | CounterClockwise


type Table =
    { Players : Players 
      Player: Player
      Direction: Direction }

module Direction =
    let flip dir =
        match dir with
        | Clockwise -> CounterClockwise
        | CounterClockwise -> Clockwise

module Table =
    let start players =
        { Players = players
          Player = Player 0
          Direction = Clockwise }
    
    let nextPlayer table =
        let (Players n ) = table.Players
        let (Player p ) = table.Player
        let nextPlayer = 
            match table.Direction with
            | Clockwise -> (p+1) % n
            | CounterClockwise -> (p-1+n) % n
        { table with Player = Player nextPlayer }
    
    let skip = nextPlayer >> nextPlayer

    let flip table = { table with Direction = Direction.flip table.Direction }

    let back = flip >> nextPlayer

    let setPlayer p table =  {table with Player = p }

    let breakingInterrupt p table = 
        table
        |> setPlayer p
        |> nextPlayer

    let nextTable effect table =
        match effect with
        | Skip -> skip table
        | Back -> back table
        | Next -> nextPlayer table
        | Interrupt -> table
        | BreakingInterrupt player -> breakingInterrupt player table


exception TooFewPlayers
exception AlreadyStarted
exception NotYetStarted

let tryPlayers n =
    if n < 2 then
        Error TooFewPlayers
    else
        Ok (Players n)


type Command =
| StartGame of StartGame
| Play of Play
and StartGame =
    { Players : Players
      FirstCard: Card }
and Play =
    { Player: Player
      Card : Card }
      

type Event =
| GameStarted of GameStarted
| CardPlayed of CardPlayed
| WrongCardPlayed of CardPlayed
| WrongPlayerPlayed of CardPlayed
| InterruptMissed of CardPlayed
and GameStarted =
    { Players: Players
      FirstCard: Card
      Effect: Effect }
and CardPlayed =
    { Player: Player
      Card: Card
      Effect: Effect }

type Pile = 
    { TopCard: Card
      SecondCard: Card option }

module Pile =
    let start card =
        { TopCard = card
          SecondCard = None }

    let put card pile =
        { TopCard = card
          SecondCard = Some pile.TopCard }


type State =
    | NotStarted
    | Started of Started
and Started =
    { Pile: Pile
      Table: Table
    }

let initialState = NotStarted

let effect card =
    match card.Rank with
    | Seven -> Skip
    | Jack -> Back
    | _ -> Next

let decide cmd state =
    match state, cmd with
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players; FirstCard = c.FirstCard; Effect = effect c.FirstCard } ]
    | _, StartGame _ ->
        raise AlreadyStarted
    | NotStarted, _ -> raise NotYetStarted
    | Started s, Play c when c.Card = s.Pile.TopCard ->
        [ CardPlayed { Player = c.Player; Card = c.Card; Effect = Interrupt }]
    | Started s,  Play c when s.Table.Player <> c.Player ->
        if Some c.Card = s.Pile.SecondCard then
            [ InterruptMissed { Player = c.Player; Card = c.Card; Effect = effect c.Card } ]
        else
            [ WrongPlayerPlayed { Player = c.Player; Card = c.Card; Effect = effect c.Card }]
    | Started s,  Play c when c.Card.Rank <> s.Pile.TopCard.Rank && c.Card.Suit <> s.Pile.TopCard.Suit ->
        [ WrongCardPlayed { Player = c.Player; Card = c.Card; Effect = effect c.Card }]
    | Started s,  Play c ->
        [ CardPlayed { Player = c.Player; Card = c.Card; Effect = effect c.Card }]


let evolve (state: State) (event: Event) : State =
    match state, event with
    | NotStarted, GameStarted e -> 
        Started { Pile = Pile.start e.FirstCard 
                  Table = Table.start e.Players |> Table.nextTable e.Effect }
    | Started s, CardPlayed e ->
        Started { s with 
                    Pile = Pile.put e.Card s.Pile
                    Table = s.Table |> Table.nextTable e.Effect  }
    | _ -> state

[<EntryPoint>]
let main argv =
    0 // return an integer exit code