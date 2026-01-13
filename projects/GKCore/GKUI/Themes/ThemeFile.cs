/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKUI.Themes
{
    internal sealed class TFElement
    {
        public string Element;
        public string Value;
    }

    internal class ThemeFile
    {
        public string Name { get; set; }
        public TFElement[] Elements { get; set; }

        public ThemeFile()
        {
            Elements = new TFElement[0];
        }
    }
}
