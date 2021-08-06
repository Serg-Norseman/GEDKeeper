/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

namespace GKMap
{
    /// <summary>
    /// cache queue item
    /// </summary>
    internal struct CacheQueueItem
    {
        public RawTile Tile;
        public byte[] Img;

        public CacheQueueItem(RawTile tile, byte[] img)
        {
            Tile = tile;
            Img = img;
        }

        public override string ToString()
        {
            return Tile.ToString();
        }

        public void Clear()
        {
            Img = null;
        }
    }
}
