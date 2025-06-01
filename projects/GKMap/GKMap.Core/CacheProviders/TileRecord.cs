using System;
using SQLite;

namespace GKMap.CacheProviders
{
    /// <summary>
    /// Represents a tile record in the database
    /// </summary>
    [Table("Tiles")]
    public class TileRecord
    {
        [PrimaryKey, AutoIncrement]
        public int Id { get; set; }

        [Indexed("IndexOfTiles", 1)]
        public long X { get; set; }

        [Indexed("IndexOfTiles", 2)]
        public long Y { get; set; }

        [Indexed("IndexOfTiles", 3)]
        public int Zoom { get; set; }

        [Indexed("IndexOfTiles", 4)]
        public uint Type { get; set; }

        public DateTime? CacheTime { get; set; }
    }
}
