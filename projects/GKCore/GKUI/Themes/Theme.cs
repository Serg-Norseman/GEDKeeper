/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;

namespace GKUI.Themes
{
    public sealed class ThemeElementsDictionary : Dictionary<ThemeElement, object>
    {
    }


    public sealed class Theme
    {
        public string Name { get; private set; }
        public ThemeElementsDictionary Elements { get; private set; }
        public bool SysDefault { get; private set; }

        public Theme(string name, ThemeElementsDictionary elements, bool sysDefault = false)
        {
            Name = name;
            Elements = elements;
            SysDefault = sysDefault;
        }
    }
}
