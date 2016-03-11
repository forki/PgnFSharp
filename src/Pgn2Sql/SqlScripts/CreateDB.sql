CREATE DATABASE ChessDb ON PRIMARY
(NAME = ChessDb, 
FILENAME = 'I:\SqlServer\Data\ChessDb.mdf', 
SIZE = 5MB, MAXSIZE = UNLIMITED, FILEGROWTH = 1MB)
LOG ON (NAME = ChessDb_Log, 
FILENAME = 'I:\\SqlServer\\Data\\ChessDb_log.ldf', 
SIZE = 1MB, 
MAXSIZE = 2048GB, 
FILEGROWTH = 10%)
