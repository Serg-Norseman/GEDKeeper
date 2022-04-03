/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Interfaces;

namespace GKCore.Options
{
    public class ListOptionsException : Exception
    {
        public ListOptionsException(string message) : base(message)
        {
        }
    }

    /// <summary>
    ///
    /// </summary>
    public class ListOptions : IOptions
    {
        public static readonly string[] RecordTypeNames = new string[] {
            "None",
            "Individual",
            "Family",
            "Note",
            "Multimedia",
            "Source",
            "Repository",
            "Group",
            "Research",
            "Task",
            "Communication",
            "Location",
            "Submission",
            "Submitter"
        };

        private readonly string fName;

        public int SortColumn { get; set; }

        public ListOptions(GDMRecordType recType)
        {
            fName = RecordTypeNames[(int)recType] + "List";

            SortColumn = 0;
        }

        public void Assign(IOptions source)
        {
            ListOptions srcOptions = source as ListOptions;
            if (srcOptions == null) return;

            SortColumn = srcOptions.SortColumn;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            try {
                SortColumn = iniFile.ReadInteger(fName, "SortColumn", 0);
            } catch (Exception) {
                throw new ListOptionsException("Error loading ListOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteInteger(fName, "SortColumn", SortColumn);
        }
    }

    public sealed class ListOptionsCollection
    {
        private ListOptions[] fOptions;

        public ListOptions this[GDMRecordType rt]
        {
            get { return fOptions[(int)rt]; }
        }

        public ListOptionsCollection()
        {
            fOptions = new ListOptions[(int)GDMRecordType.rtLast + 1];
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                fOptions[(int)rt] = new ListOptions(rt);
            }
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                fOptions[(int)rt].LoadFromFile(iniFile);
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                fOptions[(int)rt].SaveToFile(iniFile);
            }
        }
    }
}
