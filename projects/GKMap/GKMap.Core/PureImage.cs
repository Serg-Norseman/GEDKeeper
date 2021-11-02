/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.IO;

namespace GKMap
{
    /// <summary>
    /// image abstraction proxy
    /// </summary>
    public abstract class PureImageProxy
    {
        public abstract PureImage FromStream(Stream stream);

        public PureImage FromArray(byte[] data)
        {
            MemoryStream m = new MemoryStream(data, 0, data.Length, false, true);
            var pi = FromStream(m);
            if (pi != null) {
                m.Position = 0;
                pi.Data = m;
            } else {
                m.Dispose();
            }

            return pi;
        }
    }

    /// <summary>
    /// image abstraction
    /// </summary>
    public abstract class PureImage : IDisposable
    {
        public MemoryStream Data;

        internal bool IsParent;
        internal long Ix;
        internal long Xoff;
        internal long Yoff;

        public abstract void Dispose();
    }
}
