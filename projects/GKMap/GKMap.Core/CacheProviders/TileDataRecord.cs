using SQLite;

namespace GKMap.CacheProviders
{
    /// <summary>
    /// Represents tile data (blob) in the database
    /// </summary>
    [Table("TilesData")]
    public class TileDataRecord
    {
        [PrimaryKey]
        public int Id { get; set; }

        public byte[] Tile { get; set; }
    }
}
