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
    /// pure abstraction for image cache
    /// </summary>
    public interface IPureImageCache
    {
        /// <summary>
        /// puts image to db
        /// </summary>
        /// <param name="tile"></param>
        /// <param name="type"></param>
        /// <param name="pos"></param>
        /// <param name="zoom"></param>
        /// <returns></returns>
        bool PutImageToCache(byte[] tile, int type, GPoint pos, int zoom);

        /// <summary>
        /// gets image from db
        /// </summary>
        /// <param name="type"></param>
        /// <param name="pos"></param>
        /// <param name="zoom"></param>
        /// <returns></returns>
        PureImage GetImageFromCache(int type, GPoint pos, int zoom);
    }
}
