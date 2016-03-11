USE ChessDb 
CREATE TABLE Players (
PlayerId int IDENTITY(1,1) NOT NULL,
Player nvarchar(32) NOT NULL)
CREATE TABLE Games (
GameId int IDENTITY(1,1) NOT NULL,
Event nvarchar(32) NOT NULL,
Site nvarchar(32) NOT NULL,
Year smallint NULL, 
Month tinyint NULL,
Day tinyint NULL,
Round nvarchar(2) NOT NULL,
WhitePlayer int NOT NULL,
BlackPlayer int NOT NULL,
Result tinyint NOT NULL,
AdditionalInfo varbinary(MAX) NOT NULL,
Tags varbinary(MAX) NOT NULL,
MoveText varbinary(MAX) NOT NULL)
