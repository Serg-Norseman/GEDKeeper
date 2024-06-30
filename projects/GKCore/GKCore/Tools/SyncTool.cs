/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKCore.Tools
{
    public enum RecordStatus
    {
        Unknown,        // gray
        Identical,      // white
        Changed,        // yellow
        Deleted,        // orange or red
        Added,          // lightblue or cyan
    }


    /// <summary>
    ///
    /// </summary>
    public class SyncTool
    {
        public void LoadOtherFile(GDMTree mainTree, string fileName)
        {
            if (mainTree == null)
                throw new ArgumentNullException("mainTree");

            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            using (var extTree = new GDMTree()) {
                var gedcomProvider = new GEDCOMProvider(extTree);
                gedcomProvider.LoadFromFile(fileName);

                //
            }
        }
    }
}
