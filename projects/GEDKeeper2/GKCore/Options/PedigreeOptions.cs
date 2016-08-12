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
using GKCore.Interfaces;
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
    public sealed class PedigreeOptions : BaseObject, IOptions
    {
        public PedigreeFormat Format;
        public bool IncludeAttributes;
        public bool IncludeNotes;
        public bool IncludeSources;
        public bool IncludeGenerations;

        public PedigreeOptions()
        {
            this.IncludeAttributes = true;
            this.IncludeNotes = true;
            this.IncludeSources = true;
            this.IncludeGenerations = true;
        }

        public void Assign(IOptions source)
        {
            PedigreeOptions srcOptions = source as PedigreeOptions;
            if (srcOptions == null) return;

            this.Format = srcOptions.Format;
            this.IncludeAttributes = srcOptions.IncludeAttributes;
            this.IncludeNotes = srcOptions.IncludeNotes;
            this.IncludeSources = srcOptions.IncludeSources;
            this.IncludeGenerations = srcOptions.IncludeGenerations;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            try
            {
                this.Format = (PedigreeFormat)iniFile.ReadInteger("Pedigree", "Format", 0);
                this.IncludeAttributes = iniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                this.IncludeNotes = iniFile.ReadBool("Pedigree", "IncludeNotes", true);
                this.IncludeSources = iniFile.ReadBool("Pedigree", "IncludeSources", true);
                this.IncludeGenerations = iniFile.ReadBool("Pedigree", "IncludeGenerations", true);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading PedigreeOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteInteger("Pedigree", "Format", (sbyte)this.Format);
            iniFile.WriteBool("Pedigree", "IncludeAttributes", this.IncludeAttributes);
            iniFile.WriteBool("Pedigree", "IncludeNotes", this.IncludeNotes);
            iniFile.WriteBool("Pedigree", "IncludeSources", this.IncludeSources);
            iniFile.WriteBool("Pedigree", "IncludeGenerations", this.IncludeGenerations);
        }
    }
}
