CREATE DATABASE crazyeights
GO
USE crazyeights
GO

CREATE TABLE CardCount (
	Game int PRIMARY KEY,
	Count int,
)
GO

CREATE TABLE TurnsSinceLastError (
	Game int ,
	Player int,
	Turns int,
	CONSTRAINT PK_Name PRIMARY KEY (Game, Player)
)
GO

CREATE TABLE ProjectionPosition (
	Projection varchar(32) PRIMARY KEY,
	Guard varchar(32),
	PPos bigint,
	CPos bigint
)
GO
