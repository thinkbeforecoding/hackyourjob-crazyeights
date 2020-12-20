module Db
open System.Data
open Microsoft.Data.SqlClient
open EventStore.Client


module Sql =
    let createCommand (cnx: SqlConnection, transaction: SqlTransaction) (query: string) (sqlParams: SqlParameter list) =
        let cmd = new SqlCommand(query,cnx)
        cmd.Transaction <- transaction
        for p in sqlParams do
            cmd.Parameters.Add(p) |> ignore
        cmd

    let inline param n (v: ^a) = SqlParameter(n,v) 

    let openCnx (cmd: SqlCommand) f =
        let isClosed = cmd.Connection.State = ConnectionState.Closed
        if isClosed then
            cmd.Connection.Open()
        try
            f cmd
        finally
            if isClosed then
                cmd.Connection.Close()


    let exec (cmd: SqlCommand) =
        openCnx cmd (fun _ -> cmd.ExecuteNonQuery())

    let transaction cnxString f =
        use cnx = new SqlConnection(cnxString)
        cnx.Open()
        use t = cnx.BeginTransaction()
        f (cnx,t)
        t.Commit()

    let run cnxString f =
        use cnx = new SqlConnection(cnxString)
        f (cnx,null)

    


module CardCount =
    let increment cnx (game: int, guard: string, position: Position) =
        use cmd = 
            Sql.createCommand cnx """
MERGE dbo.CardCount AS c
    USING (SELECT @game Game) AS s
        ON c.Game = s.Game
    WHEN MATCHED THEN UPDATE
        SET c.Count = c.Count+1
    WHEN NOT MATCHED THEN 
        INSERT (Game, Count) 
        VALUES (s.Game, 1);
MERGE dbo.ProjectionPosition AS c
    USING (SELECT @ppos PPos, @cpos CPos, @guard Guard) AS s
        ON c.Projection = 'CardCount'
    WHEN MATCHED THEN UPDATE
        SET
            c.Guard = s.Guard, 
            c.PPos = s.PPos,
            c.CPos = s.CPos
    WHEN NOT MATCHED THEN 
        INSERT (Projection, Guard, PPos, CPos) 
        VALUES ('CardCount', s.Guard, s.PPos, s.Cpos);
            """
                [ Sql.param "game" game
                  Sql.param "guard" guard
                  Sql.param "ppos" (int64 position.PreparePosition)
                  Sql.param "cpos" (int64 position.CommitPosition) ]
        Sql.exec cmd |> ignore
    
    let insert cnx (game: int, count: int) =
        use cmd =
            Sql.createCommand cnx """
INSERT INTO dbo.CardCount
    (Game, Count)
    VALUES
    (@game, @count)
            """
        
                [ Sql.param "game" game
                  Sql.param "count" count ]
        Sql.exec cmd |> ignore

    let load cnx =
        use cmd =
            Sql.createCommand cnx """
SELECT Game, Count FROM dbo.CardCount
SELECT Guard, PPos, CPos FROM ProjectionPosition WHERE Projection = 'CardCount'
            """ []
        Sql.openCnx cmd (fun cmd ->
            let r = cmd.ExecuteReader()
            let counts =
                [ while r.Read() do
                    r.GetInt32("Game"), r.GetInt32("Count")
                 ]
            r.NextResult() |> ignore
            let guard, pos = 
                if r.Read() then
                    r.GetString("Guard"), Position(uint64 (r.GetInt64("PPos")), uint64 (r.GetInt64("CPos")))
                else
                    "",Position.Start
            guard,pos,counts
        )

    let loadPosition cnx =
        use cmd =
            Sql.createCommand cnx """
SELECT Guard, PPos, CPos FROM ProjectionPosition WHERE Projection = 'CardCount'
            """ []
        Sql.openCnx cmd (fun cmd ->
            let r = cmd.ExecuteReader()
            let guard, pos = 
                if r.Read() then
                    r.GetString("Guard"), Position(uint64 (r.GetInt64("PPos")), uint64 (r.GetInt64("CPos")))
                else
                    "",Position.Start
            guard, pos
        )
    let deleteAll cnx =
        use cmd =
            Sql.createCommand cnx """
DELETE FROM dbo.CardCount
DELETE FROM dbo.ProjectionPosition WHERE Projection = 'CardCount' """
                []
        Sql.exec cmd |> ignore


    let insertPosition cnx (guard: string, position: Position) =
        use cmd =
            Sql.createCommand cnx """
INSERT INTO dbo.ProjectionPosition
    (Projection, Guard, PPos, CPos)
    VALUES
    ('CardCount', @guard, @ppos, @cpos) """
                [ Sql.param "guard" guard
                  Sql.param "ppos" (int64 position.PreparePosition)
                  Sql.param "cpos" (int64 position.CommitPosition) ]
        Sql.exec cmd |> ignore

module TurnsSinceLastError =
    let increment cnx (game: int, player: int, guard: string, position: Position) =
        use cmd =
            Sql.createCommand cnx """
MERGE dbo.TurnsSinceLastError AS c
    USING (SELECT @game Game, @player Player) AS s
        ON c.Game = s.Game AND c.Player = s.Player
    WHEN MATCHED THEN UPDATE
        SET c.Turns = c.Turns+1

    WHEN NOT MATCHED THEN 
        INSERT (Game, Player, Turns) 
        VALUES (s.Game, s.Player, 1);

MERGE dbo.ProjectionPosition AS c
    USING (SELECT @ppos PPos, @cpos CPos, @guard Guard) AS s
        ON c.Projection = 'TurnsSinceLastError'
    WHEN MATCHED THEN UPDATE
        SET c.PPos = s.PPos,
            c.CPos = s.CPos,
            c.Guard = s.Guard
    WHEN NOT MATCHED THEN 
        INSERT (Projection, Guard, PPos, CPos) 
        VALUES ('TurnsSinceLastError', s.Guard, s.PPos, s.CPos); """
                [ Sql.param "game" game
                  Sql.param "player" player
                  Sql.param "guard" guard
                  Sql.param "ppos" (int64 position.PreparePosition)
                  Sql.param "cpos" (int64 position.CommitPosition) ]
        Sql.exec cmd |> ignore

    let reset cnx (game: int, player: int, guard: string, position: Position) =
        use cmd =
            Sql.createCommand cnx """
MERGE dbo.TurnsSinceLastError AS c
    USING (SELECT @game Game, @player Player) AS s
        ON c.Game = s.Game AND c.Player = s.Player
    WHEN MATCHED THEN UPDATE
        SET c.Turns = 0

    WHEN NOT MATCHED THEN 
        INSERT (Game, Player, Turns) 
        VALUES (s.Game, s.Player, 0);

MERGE dbo.ProjectionPosition AS c
    USING (SELECT @ppos PPos, @cpos CPos, @guard Guard) AS s
        ON c.Projection = 'TurnsSinceLastError'
    WHEN MATCHED THEN UPDATE
        SET c.PPos = s.PPos,
            c.CPos = s.CPos,
            c.Guard = s.Guard
    WHEN NOT MATCHED THEN 
        INSERT (Projection, Guard, PPos, CPos) 
        VALUES ('TurnsSinceLastError', s.Guard, s.PPos, s.Cpos); """
                [ Sql.param "game" game
                  Sql.param "player" player
                  Sql.param "guard" guard
                  Sql.param "ppos" (int64 position.PreparePosition)
                  Sql.param "cpos" (int64 position.CommitPosition) ]
        Sql.exec cmd |> ignore

    let load cnx =
        use cmd =
            Sql.createCommand cnx """
SELECT Game, Player, Turns FROM dbo.TurnsSinceLastError
SELECT Guard,PPos, CPos FROM ProjectionPosition WHERE Projection = 'TurnsSinceLastError'
            """ []
        Sql.openCnx cmd (fun cmd ->
            let r = cmd.ExecuteReader()
            let counts =
                [ while r.Read() do
                    r.GetInt32("Game"), r.GetInt32("Player"), r.GetInt32("Turns")
                 ]
            r.NextResult() |> ignore
            let guard, pos = 
                if r.Read() then
                    r.GetString("Guard"), Position(uint64 (r.GetInt64("PPos")), uint64 (r.GetInt64("CPos")))
                else
                    "",Position.Start
            guard, pos,counts
        )

    let loadPosition cnx =
        use cmd =
            Sql.createCommand cnx """
SELECT Guard,PPos, CPos FROM ProjectionPosition WHERE Projection = 'TurnsSinceLastError'
            """ []
        Sql.openCnx cmd (fun cmd ->
            let r = cmd.ExecuteReader()
            let guard,pos = 
                if r.Read() then
                    r.GetString("Guard"), Position(uint64 (r.GetInt64("PPos")), uint64 (r.GetInt64("CPos")))
                else
                    "",Position.Start
            guard,pos
        )

    let deleteAll cnx =
        use cmd =
            Sql.createCommand cnx """
DELETE FROM dbo.TurnsSinceLastError
DELETE FROM dbo.ProjectionPosition WHERE Projection = 'TurnsSinceLastError' """ 
                []
        Sql.exec cmd |> ignore

    let insert cnx (game: int, player: int, turns: int) =
        use cmd =
            Sql.createCommand cnx """
INSERT INTO dbo.TurnsSinceLastError 
    (Game, Player, Turns) 
    VALUES
    (@game, @player, @turns)
    """
                [ Sql.param "game" game
                  Sql.param "player" player
                  Sql.param "turns" turns ]
        Sql.exec cmd |> ignore
    let insertPosition cnx (guard: string, position: Position) =
        use cmd =
            Sql.createCommand cnx """
INSERT INTO dbo.ProjectionPosition
    (Projection, Guard, PPos, CPos)
    VALUES
    ('TurnsSinceLastError', @guard, @ppos, @cpos) """
                [ Sql.param "guard" guard
                  Sql.param "ppos" (int64 position.PreparePosition)
                  Sql.param "cpos" (int64 position.CommitPosition) ]
        Sql.exec cmd |> ignore


