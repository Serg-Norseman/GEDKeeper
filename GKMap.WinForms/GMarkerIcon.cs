/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;

namespace GKMap.WinForms
{
    public enum GMarkerIconType
    {
        none = 0,
        arrow,
        blue,
        blue_small,
        blue_dot,
        brown_small,
        gray_small,
        green,
        green_small,
        green_dot,
        yellow,
        yellow_small,
        yellow_dot,
        lightblue,
        lightblue_dot,
        orange,
        orange_small,
        orange_dot,
        pink,
        pink_dot,
        purple,
        purple_small,
        purple_dot,
        red,
        red_small,
        red_dot,
        black_small,
        white_small,
    }

    public class GMarkerIcon : GMapMarker
    {
        private Bitmap fBitmap;
        private Bitmap fBitmapShadow;

        private static Bitmap fArrowShadow;
        private static Bitmap fMarkerShadow;
        private static Bitmap fShadowSmall;

        public readonly GMarkerIconType Type;

        private static readonly Dictionary<string, Bitmap> IconCache = new Dictionary<string, Bitmap>();

        public GMarkerIcon(PointLatLng p, GMarkerIconType type)
            : base(p)
        {
            Type = type;

            if (type != GMarkerIconType.none) {
                LoadBitmap();
            }
        }

        /// <summary>
        /// marker using manual bitmap
        /// </summary>
        /// <param name="p"></param>
        /// <param name="bitmap"></param>
        public GMarkerIcon(PointLatLng p, Bitmap bitmap)
            : base(p)
        {
            fBitmap = bitmap;
            Size = new Size(bitmap.Width, bitmap.Height);
            Offset = new Point(-Size.Width / 2, -Size.Height);
        }

        private void LoadBitmap()
        {
            fBitmap = GetIcon(Type.ToString());
            Size = new Size(fBitmap.Width, fBitmap.Height);

            switch (Type) {
                case GMarkerIconType.arrow:
                    {
                        Offset = new Point(-11, -Size.Height);
                        if (fArrowShadow == null) {
                            fArrowShadow = GetIcon("arrow_shadow");
                        }
                        fBitmapShadow = fArrowShadow;
                    }
                    break;

                case GMarkerIconType.blue:
                case GMarkerIconType.blue_dot:
                case GMarkerIconType.green:
                case GMarkerIconType.green_dot:
                case GMarkerIconType.yellow:
                case GMarkerIconType.yellow_dot:
                case GMarkerIconType.lightblue:
                case GMarkerIconType.lightblue_dot:
                case GMarkerIconType.orange:
                case GMarkerIconType.orange_dot:
                case GMarkerIconType.pink:
                case GMarkerIconType.pink_dot:
                case GMarkerIconType.purple:
                case GMarkerIconType.purple_dot:
                case GMarkerIconType.red:
                case GMarkerIconType.red_dot:
                    {
                        Offset = new Point(-Size.Width / 2 + 1, -Size.Height + 1);
                        if (fMarkerShadow == null) {
                            fMarkerShadow = GetIcon("msmarker_shadow");
                        }
                        fBitmapShadow = fMarkerShadow;
                    }
                    break;

                case GMarkerIconType.black_small:
                case GMarkerIconType.blue_small:
                case GMarkerIconType.brown_small:
                case GMarkerIconType.gray_small:
                case GMarkerIconType.green_small:
                case GMarkerIconType.yellow_small:
                case GMarkerIconType.orange_small:
                case GMarkerIconType.purple_small:
                case GMarkerIconType.red_small:
                case GMarkerIconType.white_small:
                    {
                        Offset = new Point(-Size.Width / 2, -Size.Height + 1);
                        if (fShadowSmall == null) {
                            fShadowSmall = GetIcon("shadow_small");
                        }
                        fBitmapShadow = fShadowSmall;
                    }
                    break;
            }
        }

        internal static Bitmap GetIcon(string name)
        {
            Bitmap ret;
            if (!IconCache.TryGetValue(name, out ret)) {
                ret = LoadResourceBitmap("GKMap.WinForms.Resources." + name + ".png");
                IconCache.Add(name, ret);
            }
            return ret;
        }

        private static Bitmap LoadResourceBitmap(string resName)
        {
            Assembly assembly = typeof(GMarkerIcon).Assembly;
            Stream resStream = assembly.GetManifestResourceStream(resName);
            return new Bitmap(resStream);
        }

        public override void OnRender(Graphics g)
        {
            if (fBitmapShadow != null) {
                g.DrawImage(fBitmapShadow, LocalPosition.X, LocalPosition.Y, fBitmapShadow.Width, fBitmapShadow.Height);
            }                
            g.DrawImage(fBitmap, LocalPosition.X, LocalPosition.Y, Size.Width, Size.Height);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && fBitmap != null) {
                if (!IconCache.ContainsValue(fBitmap)) {
                    fBitmap.Dispose();
                    fBitmap = null;
                }
            }
            base.Dispose(disposing);
        }
    }
}
