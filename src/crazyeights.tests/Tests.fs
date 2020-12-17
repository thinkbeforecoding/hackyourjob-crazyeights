module Tests

open System
open Xunit
open Swensen.Unquote
open CrazyEights

let (=>) (events: Event list) (cmd: Command) : Event list =
    events 
    |> List.fold evolve initialState
    |> decide cmd

[<Fact>]
let ``Game should start`` () =
    [ ]
    => StartGame { Players = players 4; FirstCard = Three ^ Club }
    =! [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next} ]


[<Fact>]
let ``It's not fun to play alone`` () =
        tryPlayers 1 =! Error TooFewPlayers

[<Fact>]
let ``Should not start game twice``() =
    raises<AlreadyStarted>
        <@
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next } ]
        => StartGame { Players = players 4; FirstCard = Three ^ Club}
        @>

[<Fact>]
let ``Player 1 should play``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next } ]
        => Play { Player = Player 2; Card = Three ^ Spade}
        =! [ WrongPlayerPlayed { Player  = Player 2; Card = Three ^ Spade; Effect = Next} ]

[<Fact>]
let ``Player 2 can play after player 1``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player  = Player 1; Card = Three ^ Spade; Effect = Next } ]
        => Play { Player = Player 2; Card = Four ^ Spade}
        =! [ CardPlayed { Player  = Player 2; Card = Four ^ Spade; Effect = Next } ]


[<Fact>]
let ``A 7 skips next player turn``() =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next } ]
    => Play { Player = Player 1; Card = Seven ^ Club}
    =! [ CardPlayed { Player = Player 1; Card = Seven ^ Club; Effect = Skip } ]

[<Fact>]
let ``Player's turn is skiped after a Seven``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player  = Player 1; Card = Seven ^ Club; Effect = Skip} ]
        => Play { Player = Player 3; Card = Four ^ Club}
        =! [ CardPlayed { Player = Player 3; Card = Four ^ Club; Effect = Next} ]

[<Fact>]
let ``Player's turn is skiped after a Seven on start``() =
        [ GameStarted { Players = players 4; FirstCard = Seven ^ Club; Effect = Skip }]
        => Play { Player = Player 2; Card = Four ^ Club}
        =! [ CardPlayed { Player = Player 2; Card = Four ^ Club; Effect = Next} ]

[<Fact>]
let ``Seven on start is skip``() =
        [ ]
        => StartGame { Players = players 4; FirstCard = Seven ^ Club; }
        =! [ GameStarted { Players = players 4; FirstCard = Seven ^ Club; Effect = Skip } ]


[<Fact>]
let ``A Jack skips change direction``() =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next } ]
    => Play { Player = Player 1; Card = Jack ^ Club}
    =! [ CardPlayed { Player = Player 1; Card = Jack ^ Club; Effect = Back } ]

[<Fact>]
let ``Direction changed after Jack``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player  = Player 1; Card = Jack ^ Club; Effect = Back} ]
        => Play { Player = Player 0; Card = Four ^ Club}
        =! [ CardPlayed { Player = Player 0; Card = Four ^ Club; Effect = Next} ]

[<Fact>]
let ``Direction changed after a Jack on start``() =
        [ GameStarted { Players = players 4; FirstCard = Jack ^ Club; Effect = Back }]
        => Play { Player = Player 3; Card = Four ^ Club}
        =! [ CardPlayed { Player = Player 3; Card = Four ^ Club; Effect = Next} ]

[<Fact>]
let ``Jack on start is Back``() =
        [ ]
        => StartGame { Players = players 4; FirstCard = Jack ^ Club; }
        =! [ GameStarted { Players = players 4; FirstCard = Jack ^ Club; Effect = Back } ]


[<Fact>]
let ``Player can interrupt event while not their turn``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player = Player 1; Card = Four ^ Club; Effect = Next } ]
        => Play { Player = Player 3; Card = Four ^ Club }
        =! [ CardPlayed { Player = Player 3; Card = Four ^ Club; Effect = Interrupt } ]

[<Fact>]
let ``Interrupt doesn't change turn``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player = Player 1; Card = Four ^ Club; Effect = Next }
          CardPlayed { Player = Player 3; Card = Four ^ Club; Effect = Interrupt } ]
        => Play { Player = Player 2; Card = Six ^ Club }
        =! [ CardPlayed { Player = Player 2; Card = Six ^ Club; Effect = Next } ]

[<Fact>]
let ``Player can interrupt at their turn``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player = Player 1; Card = Four ^ Club; Effect = Next } ]
        => Play { Player = Player 2; Card = Four ^ Club }
        =! [ CardPlayed { Player = Player 2; Card = Four ^ Club; Effect = Interrupt } ]
[<Fact>]
let ``Player can play after interrupting at their turn``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player = Player 1; Card = Four ^ Club; Effect = Next } 
          CardPlayed { Player = Player 2; Card = Four ^ Club; Effect = Interrupt }  ]
        => Play { Player = Player 2; Card = Six ^ Club }
        =! [ CardPlayed { Player = Player 2; Card = Six ^ Club; Effect = Next } ]

[<Fact>]
let ``Missed interrupt is detected``() =
        [ GameStarted { Players = players 4; FirstCard = Three ^ Club; Effect = Next }
          CardPlayed { Player = Player 1; Card = Four ^ Club; Effect = Next }
          CardPlayed { Player = Player 2; Card = Six ^ Club; Effect = Next } ]
        => Play { Player = Player 0; Card = Four ^ Club }
        =! [ InterruptMissed { Player = Player 0; Card = Four ^ Club; Effect = Next } ]