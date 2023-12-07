/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Diagnostics;
using System.IO;
using SkiaSharp;

namespace GKMap.Xamarin
{
    /// <summary>
    /// image abstraction
    /// </summary>
    public class GMapImage : PureImage
    {
        public SKImage Img;

        public override void Dispose()
        {
            if (Img != null) {
                Img.Dispose();
                Img = null;
            }

            if (Data != null) {
                Data.Dispose();
                Data = null;
            }
        }
    }

    /// <summary>
    /// image abstraction proxy
    /// </summary>
    public class GMapImageProxy : PureImageProxy
    {
        public static readonly GMapImageProxy Instance = new GMapImageProxy();

        private GMapImageProxy()
        {
        }

        public override PureImage FromStream(Stream stream)
        {
            GMapImage ret;
            try {
                ret = new GMapImage();
                ret.Img = SKImage.FromEncodedData(stream);
            } catch (Exception ex) {
                ret = null;
                Debug.WriteLine("FromStream: " + ex);
            }
            return ret;
        }
    }
}
