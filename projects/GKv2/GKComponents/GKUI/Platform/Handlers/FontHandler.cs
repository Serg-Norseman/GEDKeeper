/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Drawing;
using BSLib;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FontHandler: TypeHandler<Font>, IFont
    {
        private float fHeight;

        public string FontFamilyName
        {
            get { return Handle.FontFamily.Name; }
        }

        public string Name
        {
            get { return Handle.Name; }
        }

        public float Size
        {
            get { return Handle.Size; }
        }

        public float Height
        {
            get { return fHeight; }
        }

        public FontHandler(Font handle) : base(handle)
        {
            fHeight = handle.GetHeight();
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
