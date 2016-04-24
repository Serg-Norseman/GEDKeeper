/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Collections.Generic;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMFactory
    {
        private static GEDCOMFactory fInstance;
        private readonly Dictionary<string, TagConstructor> fConstructors;

        public static GEDCOMFactory GetInstance()
        {
            if (fInstance == null) fInstance = new GEDCOMFactory();
            return fInstance;
        }

        public GEDCOMFactory()
        {
            this.fConstructors = new Dictionary<string, TagConstructor>();
        }

        public void RegisterTag(string key, TagConstructor constructor)
        {
            if (fConstructors.ContainsKey(key))
                fConstructors[key] = constructor;
            else
                fConstructors.Add(key, constructor);
        }

        public GEDCOMTag CreateTag(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            TagConstructor constructor;

            if (fConstructors.TryGetValue(tagName, out constructor))
            {
                return constructor(owner, parent, tagName, tagValue);
            }

            return null;
        }
    }
}
