/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Options
{
    public class PedigreeOptionsException : Exception
    {
        public PedigreeOptionsException()
        {
        }
        public PedigreeOptionsException(string message) : base(message)
        {
        }
    }

    /// <summary>
    ///
    /// </summary>
    public sealed class PedigreeOptions : IOptions
    {
        public PedigreeFormat Format;
        public bool IncludeAttributes;
        public bool IncludeNotes;
        public bool IncludeSources;
        public bool IncludeGenerations;

        public PedigreeOptions()
        {
            IncludeAttributes = true;
            IncludeNotes = true;
            IncludeSources = true;
            IncludeGenerations = true;
        }

        public void Assign(IOptions source)
        {
            PedigreeOptions srcOptions = source as PedigreeOptions;
            if (srcOptions == null) return;

            Format = srcOptions.Format;
            IncludeAttributes = srcOptions.IncludeAttributes;
            IncludeNotes = srcOptions.IncludeNotes;
            IncludeSources = srcOptions.IncludeSources;
            IncludeGenerations = srcOptions.IncludeGenerations;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            try
            {
                Format = (PedigreeFormat)iniFile.ReadInteger("Pedigree", "Format", 0);
                IncludeAttributes = iniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                IncludeNotes = iniFile.ReadBool("Pedigree", "IncludeNotes", true);
                IncludeSources = iniFile.ReadBool("Pedigree", "IncludeSources", true);
                IncludeGenerations = iniFile.ReadBool("Pedigree", "IncludeGenerations", true);
            }
            catch (Exception)
            {
                throw new PedigreeOptionsException("Error loading PedigreeOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteInteger("Pedigree", "Format", (sbyte)Format);
            iniFile.WriteBool("Pedigree", "IncludeAttributes", IncludeAttributes);
            iniFile.WriteBool("Pedigree", "IncludeNotes", IncludeNotes);
            iniFile.WriteBool("Pedigree", "IncludeSources", IncludeSources);
            iniFile.WriteBool("Pedigree", "IncludeGenerations", IncludeGenerations);
        }
    }
}
