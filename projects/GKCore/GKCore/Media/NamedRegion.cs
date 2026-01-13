/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;

namespace GKCore.Media
{
    public class NamedRegion
    {
        public readonly string Name;
        public readonly ExtRect Region;

        public NamedRegion(string name, ExtRect region)
        {
            Name = name;
            Region = region;
        }
    }
}
