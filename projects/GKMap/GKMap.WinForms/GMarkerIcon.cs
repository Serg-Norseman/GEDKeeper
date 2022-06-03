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
using GKMap.MapObjects;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap marker with icon.
    /// </summary>
    public sealed class GMarkerIcon : MapIconMarker, IRenderable
    {
        private static readonly Dictionary<string, Bitmap> IconCache = new Dictionary<string, Bitmap>();


        private Bitmap fBitmap;
        private Bitmap fBitmapShadow;

        private static Bitmap fArrowShadow;
        private static Bitmap fMarkerShadow;
        private static Bitmap fShadowSmall;


        public override string ToolTipText
        {
            get {
                return base.ToolTipText;
            }
            set {
                if (ToolTip == null && !string.IsNullOrEmpty(value)) {
                    ToolTip = new GMapRoundedToolTip(this);
                }
                base.ToolTipText = value;
            }
        }


        public GMarkerIcon(PointLatLng p, GMarkerIconType type)
            : base(p, type)
        {
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
            Size = new GSize(bitmap.Width, bitmap.Height);
            Offset = new GPoint(-Size.Width / 2, -Size.Height);
        }

        protected override void LoadBitmap()
        {
            fBitmap = GetIcon(Type.ToString());
            Size = new GSize(fBitmap.Width, fBitmap.Height);

            switch (Type) {
                case GMarkerIconType.arrow:
                    {
                        Offset = new GPoint(-11, -Size.Height);
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
                        Offset = new GPoint(-Size.Width / 2 + 1, -Size.Height + 1);
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
                        Offset = new GPoint(-Size.Width / 2, -Size.Height + 1);
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
                string resName = "GKMap.Resources.Images." + name + ".png";
                Stream resStream = Stuff.LoadResourceStream(resName);
                ret = new Bitmap(resStream);
                IconCache.Add(name, ret);
            }
            return ret;
        }

        public void OnRender(Graphics g)
        {
            if (fBitmapShadow != null) {
                g.DrawImage(fBitmapShadow, LocalPosition.X, LocalPosition.Y, fBitmapShadow.Width, fBitmapShadow.Height);
            }                
            g.DrawImage(fBitmap, LocalPosition.X, LocalPosition.Y, Size.Width, Size.Height);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && fBitmap != null && !IconCache.ContainsValue(fBitmap)) {
                fBitmap.Dispose();
                fBitmap = null;
            }
            base.Dispose(disposing);
        }
    }
}
