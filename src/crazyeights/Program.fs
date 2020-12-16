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

let raiseOnError result =
    match result with
    | Ok v -> v
    | Error ex -> raise ex

let failOnError result =
    match result with
    | Ok v -> v
    | Error ex -> failwith ex

let players = tryPlayers >> raiseOnError

module Players =
    let value (Players n) = n


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

module Result =
    let map2 f rx ry =
        match rx, ry with
        | Ok x, Ok y -> Ok (f x y)
        | Error e, Ok _
        | Ok _ , Error e -> Error e
        | Error ex, Error ey -> Error (sprintf "%s, %s" ex ey)

module Serialization =
    let (|StartsWith|_|) (prefix: string) (s: string) = 
        if s.StartsWith(prefix) then Some() else None
    let (|EndsWith|_|) (suffix: string) (s: string) = 
        if s.EndsWith(suffix) then Some() else None
        
    let (|Int|_|) (s: string) =
        match Int32.TryParse s with
        | true, v -> Some v
        | false, _ -> None

    module Players =
        let tryOfString s =
            match Int32.TryParse(s :string) with
            | true, v ->
                match tryPlayers v with
                | Ok p -> Ok p
                | Error _ -> Error "Invalid player count"
            | false, _ ->
                Error "Players should be an int"

    module Player =
        let tryOfString s =
            match Int32.TryParse(s :string) with
            | true, p ->
                Ok (Player p)
            | false, _ ->
                Error "Player should be an int"
    module Rank =
        let toString = function
            | Ace -> "1"
            | Two -> "2"
            | Three -> "3"
            | Four -> "4"
            | Five -> "5"
            | Six -> "6"
            | Seven -> "7"
            | Eight -> "8"
            | Nine -> "9"
            | Ten -> "10"
            | Jack -> "J"
            | Queen -> "Q"
            | King -> "K"
        let tryOfString = function
            | StartsWith "K" -> Ok King
            | StartsWith "Q" -> Ok Queen
            | StartsWith "J" -> Ok Jack
            | StartsWith "10" -> Ok Ten
            | StartsWith "9" -> Ok Nine
            | StartsWith "8" -> Ok Eight
            | StartsWith "7" -> Ok Seven
            | StartsWith "6" -> Ok Six
            | StartsWith "5" -> Ok Five
            | StartsWith "4" -> Ok Four
            | StartsWith "3" -> Ok Three
            | StartsWith "2" -> Ok Two
            | StartsWith "1" -> Ok Ace
            | s -> Error (sprintf "Unknown rank in %s" s)
        
        let ofString = tryOfString >> failOnError

    module Suit =
        let toString = function
            | Club -> "C"
            | Spade -> "S"
            | Diamond -> "D"
            | Heart -> "H"

        let tryOfString = function
            | EndsWith "C" -> Ok Club
            | EndsWith "S" -> Ok Spade
            | EndsWith "D" -> Ok Diamond
            | EndsWith "H" -> Ok Heart
            | s -> Error (sprintf "Unknown suit in %s" s)

        let ofString = tryOfString >> failOnError
    
    module Card =
        let toString card =
            Rank.toString card.Rank + Suit.toString card.Suit

        let tryOfString input =
            match Rank.tryOfString input, Suit.tryOfString input with
            | Ok rank, Ok suit -> Ok { Rank = rank; Suit = suit }
            | Error e, Ok _
            | Ok _, Error e -> Error e
            | Error er, Error es -> Error (er + " " + es)
        
        let ofString = tryOfString >> failOnError

    module Effect =
        let toString = function
            | Next -> "next"
            | Skip -> "skip"
            | Back -> "back"
            | Interrupt -> "int"
            | BreakingInterrupt (Player p) -> sprintf "int%d" p

        let tryOfString = function
            | "next" -> Ok Next
            | "skip" -> Ok Skip
            | "back" -> Ok Back
            | StartsWith "int" as s ->
                match Int32.TryParse(s.AsSpan().Slice(3)) with
                | true, v -> Ok (BreakingInterrupt (Player v))
                | _ -> Ok Interrupt
            | s -> Error (sprintf "Unknown effect %s" s)

        let ofString = tryOfString >> failOnError

    type CardPlayedDto =
        { Player: int
          Card: string
          Effect: string }
    module CardPlayed =
        let toDto (e: CardPlayed): CardPlayedDto =
            let (Player p) = e.Player
            { Player = p
              Card = Card.toString e.Card
              Effect = Effect.toString e.Effect }

        let ofDto (dto: CardPlayedDto) : CardPlayed =
            { Player = Player dto.Player
              Card = Card.ofString dto.Card 
              Effect = Effect.ofString dto.Effect }
    type GameStartedDto =
        { Players: int
          FirstCard: string
          Effect: string }
    module GameStarted =
        let toDto (e: GameStarted): GameStartedDto =
            { Players = Players.value e.Players
              FirstCard = Card.toString e.FirstCard
              Effect = Effect.toString e.Effect }

        let ofDto (dto: GameStartedDto) : GameStarted =
            { Players = players dto.Players
              FirstCard = Card.ofString dto.FirstCard 
              Effect = Effect.ofString dto.Effect }

    module Event =
        open System.Text.Json

        let toBytes eventType converter e =
            eventType, JsonSerializer.SerializeToUtf8Bytes (converter e, JsonSerializerOptions(WriteIndented=false))
        
        let ofBytes case (converter: 'b -> 'e) (bytes:byte[]) =
            try
                [JsonSerializer.Deserialize<'b>(Span.op_Implicit (bytes.AsSpan())) |> converter |> case]
            with
            | _ -> []

        let serialize (event:Event) : string * byte[] =
            match event with
            | GameStarted e -> toBytes "GameStarted" GameStarted.toDto e
            | CardPlayed e -> toBytes "CardPlayed" CardPlayed.toDto e
            | WrongCardPlayed e -> toBytes "GameStarted" CardPlayed.toDto e
            | WrongPlayerPlayed e -> toBytes "WrongPlayerPlayed" CardPlayed.toDto e
            | InterruptMissed e -> toBytes "InterruptMissed" CardPlayed.toDto e

        let deserialize (eventType, bytes) =
            match eventType with
            | "GameStarted" -> ofBytes GameStarted GameStarted.ofDto bytes
            | "CardPlayed" -> ofBytes CardPlayed CardPlayed.ofDto bytes
            | "WrongCardPlayed" -> ofBytes WrongCardPlayed CardPlayed.ofDto bytes
            | "WrongPlayerPlayed" -> ofBytes WrongPlayerPlayed CardPlayed.ofDto bytes
            | "InterruptMissed" -> ofBytes InterruptMissed CardPlayed.ofDto bytes
            | _ -> []

    let toString (t,b) = sprintf "%s %s" t (Text.Encoding.UTF8.GetString(b: byte[]))

    let rx = Text.RegularExpressions.Regex(@"^([^\s{]+)?\s*({.*$)")
    let ofString (s: string) = 
        let m = rx.Match(s)
        if m.Success then
            m.Groups.[1].Value, Text.Encoding.UTF8.GetBytes m.Groups.[2].Value
        else
            "",[||]



    module Command =
        let tryParseArgs args =
            match args with
            | [|"start"; p; card|] ->
                Result.map2 
                    (fun p card -> StartGame { Players = p; FirstCard = card} )
                    (Players.tryOfString p)
                    (Card.tryOfString card)
            | [| "p"; p; card |] ->
                Result.map2
                    (fun p card -> Play { Player = p; Card = card})
                    (Player.tryOfString p)
                    (Card.tryOfString card)
            | _ ->
                Error "Unknown command"
        let tryParse (s:string) =
            s.Split(' ') |> tryParseArgs



    
        

[<EntryPoint>]
let main argv =
    let b = 
        GameStarted { Players = players 4; FirstCard = Six ^ Club; Effect = Next}
        |> Serialization.Event.serialize
        |> Serialization.toString
    let c = 
        CardPlayed { Player = Player 2; Card = Six ^ Club; Effect = Next}
        |> Serialization.Event.serialize
        |> Serialization.toString
    let e =
        """GameStarted {"Players":4,"FirstCard":"6C","Effect":"next"}"""
        |> Serialization.ofString
        |> Serialization.Event.deserialize

    let e2 =
        """CardPlayed {"Player":2,"Card":"6C","Effect":"next"}"""
        |> Serialization.ofString
        |> Serialization.Event.deserialize

    let c1 = Serialization.Command.tryParse "start 4 3C"
    let c2 = Serialization.Command.tryParse "p 1 1C"
    0 // return an integer exit code