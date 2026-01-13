/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using GKCore.Utilities;

namespace GKCore.ExtData
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
                throw new ArgumentNullException(nameof(ident));

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
