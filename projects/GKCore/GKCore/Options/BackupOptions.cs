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
using BSLib;
using GKCore.Backups;

namespace GKCore.Options
{
    /// <summary>
    ///
    /// </summary>
    public sealed class BackupOptions : IOptions
    {
        public bool Autosave { get; set; }
        public int AutosaveInterval { get; set; }
        public FileBackup FileBackup { get; set; }
        public int FileBackupEachRevisionMaxCount { get; set; }

        public bool ExtendedBackup { get; set; }
        public string ExtendedFolder { get; set; }


        public BackupOptions()
        {
            ResetDefaults();
        }

        public void Assign(IOptions source)
        {
            var srcOptions = source as BackupOptions;
            if (srcOptions == null) return;
        }

        public void ResetDefaults()
        {
            Autosave = false;
            AutosaveInterval = 10;
            FileBackupEachRevisionMaxCount = 0;

            ExtendedBackup = false;
            ExtendedFolder = string.Empty;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            try {
                Autosave = iniFile.ReadBool("Common", "Autosave", false);
                AutosaveInterval = iniFile.ReadInteger("Common", "AutosaveInterval", 10);
                FileBackup = (FileBackup)iniFile.ReadInteger("Common", "FileBackup", 0);
                FileBackupEachRevisionMaxCount = iniFile.ReadInteger("Common", "FileBackupEachRevisionMaxCount", 0);

                ExtendedBackup = iniFile.ReadBool("Backup", "ExtendedBackupEnabled", false);
                ExtendedFolder = iniFile.ReadString("Backup", "Folder", string.Empty);
            } catch (Exception) {
                throw new PedigreeOptionsException("Error loading BackupOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteBool("Common", "Autosave", Autosave);
            iniFile.WriteInteger("Common", "AutosaveInterval", AutosaveInterval);
            iniFile.WriteInteger("Common", "FileBackup", (int)FileBackup);
            iniFile.WriteInteger("Common", "FileBackupEachRevisionMaxCount", FileBackupEachRevisionMaxCount);

            iniFile.WriteBool("Backup", "ExtendedBackupEnabled", ExtendedBackup);
            iniFile.WriteString("Backup", "Folder", ExtendedFolder);
        }
    }
}
