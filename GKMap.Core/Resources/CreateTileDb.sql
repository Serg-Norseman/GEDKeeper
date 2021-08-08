CREATE TABLE IF NOT EXISTS Tiles (id INTEGER NOT NULL PRIMARY KEY, X INTEGER NOT NULL, Y INTEGER NOT NULL, Zoom INTEGER NOT NULL, Type UNSIGNED INTEGER  NOT NULL, CacheTime DATETIME);
CREATE INDEX IF NOT EXISTS IndexOfTiles ON Tiles (X, Y, Zoom, Type);

CREATE TABLE IF NOT EXISTS TilesData (id INTEGER NOT NULL PRIMARY KEY CONSTRAINT fk_Tiles_id REFERENCES Tiles(id) ON DELETE CASCADE, Tile BLOB NULL);

-- Foreign Key Preventing insert
CREATE TRIGGER fki_TilesData_id_Tiles_id
BEFORE INSERT ON [TilesData]
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'insert on table "TilesData" violates foreign key constraint "fki_TilesData_id_Tiles_id"')
  WHERE (SELECT id FROM Tiles WHERE id = NEW.id) IS NULL;
END;

-- Foreign key preventing update
CREATE TRIGGER fku_TilesData_id_Tiles_id
BEFORE UPDATE ON [TilesData] 
FOR EACH ROW BEGIN
    SELECT RAISE(ROLLBACK, 'update on table "TilesData" violates foreign key constraint "fku_TilesData_id_Tiles_id"')
      WHERE (SELECT id FROM Tiles WHERE id = NEW.id) IS NULL;
END;

-- Cascading Delete
CREATE TRIGGER fkdc_TilesData_id_Tiles_id
BEFORE DELETE ON Tiles
FOR EACH ROW BEGIN 
    DELETE FROM TilesData WHERE TilesData.id = OLD.id;
END;