/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;

namespace GKCore.Options
{
    public sealed class FamilyBookOptions : IOptions
    {
        public bool IncludeEvents;
        public bool IncludeNotes;
        public bool MergeNotes;

        public FamilyBookOptions()
        {
            ResetDefaults();
        }

        public void Assign(IOptions source)
        {
            FamilyBookOptions srcOptions = source as FamilyBookOptions;
            if (srcOptions == null) return;

            IncludeEvents = srcOptions.IncludeEvents;
            IncludeNotes = srcOptions.IncludeNotes;
            MergeNotes = srcOptions.MergeNotes;
        }

        public void ResetDefaults()
        {
            IncludeEvents = true;
            IncludeNotes = true;
            MergeNotes = true;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            try {
                IncludeEvents = iniFile.ReadBool("FamilyBook", "IncludeEvents", true);
                IncludeNotes = iniFile.ReadBool("FamilyBook", "IncludeNotes", true);
                MergeNotes = iniFile.ReadBool("FamilyBook", "MergeNotes", true);
            } catch (Exception) {
                throw new Exception("Error loading FamilyBookOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            iniFile.WriteBool("FamilyBook", "IncludeEvents", IncludeEvents);
            iniFile.WriteBool("FamilyBook", "IncludeNotes", IncludeNotes);
            iniFile.WriteBool("FamilyBook", "MergeNotes", MergeNotes);
        }
    }
}
