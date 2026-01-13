/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
                throw new ArgumentNullException(nameof(iniFile));

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
                throw new ArgumentNullException(nameof(iniFile));

            iniFile.WriteBool("Common", "Autosave", Autosave);
            iniFile.WriteInteger("Common", "AutosaveInterval", AutosaveInterval);
            iniFile.WriteInteger("Common", "FileBackup", (int)FileBackup);
            iniFile.WriteInteger("Common", "FileBackupEachRevisionMaxCount", FileBackupEachRevisionMaxCount);

            iniFile.WriteBool("Backup", "ExtendedBackupEnabled", ExtendedBackup);
            iniFile.WriteString("Backup", "Folder", ExtendedFolder);
        }
    }
}
