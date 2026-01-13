/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Graphics;
using SkiaSharp;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FontHandler: TypeHandler<SKPaint>, IFont
    {
        public string FontFamilyName
        {
            get { return Handle.Typeface.FamilyName; }
        }

        public string Name
        {
            get { return Handle.Typeface.FamilyName; }
        }

        public float Size
        {
            get { return Handle.TextSize; }
        }

        public float Height
        {
            get { return Handle.TextSize /* pixels! */; }
        }

        public FontHandler(SKPaint handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Handle.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}
