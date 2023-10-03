/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using BSLib;
using GKCore.Design.Graphics;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKBackupPlugin")]
[assembly: AssemblyDescription("GEDKeeper Backup plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2022 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKBackupPlugin
{
    public enum PLS
    {
        Backup = 1,
        Enabled,
        Folder,
        FolderChoose,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKBackupPlugin";
        private ILangMan fLangMan;
        private BackupWidget fForm;

        private bool fExtendedBackupEnabled = false;
        private string fFolder = string.Empty;


        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }


        public bool ExtendedBackupEnabled
        {
            get { return fExtendedBackupEnabled; }
            set { fExtendedBackupEnabled = value; }
        }

        public string Folder
        {
            get { return fFolder; }
            set { fFolder = value; }
        }


        public Plugin()
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null) {
                    fForm.Dispose();
                    fForm = null;
                }
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
            if (fForm != null) {
                fForm = null;
            }
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new BackupWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
                fForm = null;
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Backup);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKBackupPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                CloseForm();
            } catch (Exception ex) {
                Logger.WriteError("GKBackupPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        public override void BaseSaved(IBaseWindow baseWin, string fileName)
        {
            if (fExtendedBackupEnabled) {
                string pureFileName = Path.GetFileName(fileName);
                string backupFileName = Path.Combine(fFolder, pureFileName);

                baseWin.Context.CopyFile(fileName, backupFileName, true);
                baseWin.Context.MoveMediaContainers(fileName, backupFileName, true);
            }
        }

        public override void LoadOptions(IniFile ini)
        {
            fExtendedBackupEnabled = ini.ReadBool("GKBackupPlugin", "ExtendedBackupEnabled", false);
            fFolder = ini.ReadString("GKBackupPlugin", "Folder", string.Empty);
        }

        public override void SaveOptions(IniFile ini)
        {
            ini.WriteBool("GKBackupPlugin", "ExtendedBackupEnabled", fExtendedBackupEnabled);
            ini.WriteString("GKBackupPlugin", "Folder", fFolder);
        }
    }
}
