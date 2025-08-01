/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System;
using System.IO;
using GKCore.Utilities;

namespace GKCore
{
    public sealed class ExtResource
    {
        public string Name;
        public string Ident;
        public string URL;
    }


    internal class ExtResourcesList
    {
        public ExtResource[] Resources { get; set; }

        public ExtResourcesList()
        {
            Resources = new ExtResource[0];
        }
    }


    /// <summary>
    /// Directory of external resources (sites) that can be identified by
    /// user references in GEDCOM data.
    /// </summary>
    public sealed class ExtResources
    {
        private ExtResourcesList fResources;

        public ExtResources()
        {
            fResources = new ExtResourcesList();
        }

        public void Load(string fileName)
        {
            if (!File.Exists(fileName)) return;

            try {
                // loading database
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fResources = YamlHelper.Deserialize<ExtResourcesList>(content);
                }
            } catch (Exception ex) {
                Logger.WriteError("ExtResources.Load()", ex);
            }
        }

        public ExtResource FindURL(string ident)
        {
            if (string.IsNullOrEmpty(ident))
                throw new ArgumentNullException("ident");

            try {
                for (int i = 0; i < fResources.Resources.Length; i++) {
                    var res = fResources.Resources[i];

                    if (res.Ident == ident) {
                        return res;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("ExtResources.FindURL()", ex);
            }

            return null;
        }
    }
}
