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

using System;
using GKCommon;
using GKCore.Types;

namespace GKCore.Options
{
    [Serializable]
    public class EPedigreeOptionsException : Exception
    {
        public EPedigreeOptionsException()
        {
        }
        public EPedigreeOptionsException(string message) : base(message)
        {
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class PedigreeOptions : BaseObject
    {
        public PedigreeFormat Format;
        public bool IncludeAttributes;
        public bool IncludeNotes;
        public bool IncludeSources;

        public PedigreeOptions()
        {
            this.IncludeAttributes = true;
            this.IncludeNotes = true;
            this.IncludeSources = true;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null) {
                throw new ArgumentNullException("iniFile");
            }

            try
            {
                this.IncludeAttributes = iniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                this.IncludeNotes = iniFile.ReadBool("Pedigree", "IncludeNotes", true);
                this.IncludeSources = iniFile.ReadBool("Pedigree", "IncludeSources", true);
                this.Format = (PedigreeFormat)iniFile.ReadInteger("Pedigree", "Format", 0);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading PedigreeOptions"); // FIXME
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null) {
                throw new ArgumentNullException("iniFile");
            }

            iniFile.WriteBool("Pedigree", "IncludeAttributes", this.IncludeAttributes);
            iniFile.WriteBool("Pedigree", "IncludeNotes", this.IncludeNotes);
            iniFile.WriteBool("Pedigree", "IncludeSources", this.IncludeSources);
            iniFile.WriteInteger("Pedigree", "Format", (sbyte)this.Format);
        }
    }
}
