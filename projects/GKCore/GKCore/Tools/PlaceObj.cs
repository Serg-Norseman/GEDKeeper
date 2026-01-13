/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel;

namespace GKCore.Tools
{
    /// <summary>
    ///
    /// </summary>
    public sealed class PlaceObj
    {
        public readonly string Name;
        public readonly List<GDMCustomEvent> Facts;

        public PlaceObj(string name)
        {
            Name = name;
            Facts = new List<GDMCustomEvent>();
        }
    }
}
