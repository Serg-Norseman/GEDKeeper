/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    public sealed class FontHandler : TypeHandler<string>, IFont
    {
        public string FontFamilyName
        {
            get { return string.Empty; }
        }

        public float Height
        {
            get { return 1; }
        }

        public string Name
        {
            get { return string.Empty; }
        }

        public float Size
        {
            get { return 1; }
        }

        public FontHandler(string handle) : base(handle)
        {
        }
    }
}
