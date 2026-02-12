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
    /// <summary>
    /// Introduced for isolation from System.WinForms.
    /// </summary>
    public enum WindowState
    {
        Normal,
        Minimized,
        Maximized
    }


    /// <summary>
    /// Most Recently Used (MRU) files.
    /// </summary>
    public sealed class MRUFile
    {
        public string FileName;
        public ExtRect WinRect;
        public WindowState WinState;
        public string LastTreeRecord;

        public MRUFile()
        {
        }

        public MRUFile(string fileName)
        {
            FileName = fileName;
        }

        public void LoadFromFile(IniFile iniFile, string section)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            FileName = iniFile.ReadString(section, "FileName", "");
            WinRect.Left = iniFile.ReadInteger(section, "WinL", 10);
            WinRect.Top = iniFile.ReadInteger(section, "WinT", 10);
            WinRect.Right = iniFile.ReadInteger(section, "WinR", 778);
            WinRect.Bottom = iniFile.ReadInteger(section, "WinB", 312);
            WinState = (WindowState)((uint)iniFile.ReadInteger(section, "WinState", 0));
            LastTreeRecord = iniFile.ReadString(section, "LastTreeRecord", "");
        }

        public void SaveToFile(IniFile iniFile, string section)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            iniFile.WriteString(section, "FileName", FileName);
            iniFile.WriteInteger(section, "WinL", WinRect.Left);
            iniFile.WriteInteger(section, "WinT", WinRect.Top);
            iniFile.WriteInteger(section, "WinR", WinRect.Right);
            iniFile.WriteInteger(section, "WinB", WinRect.Bottom);
            iniFile.WriteInteger(section, "WinState", (int)WinState);
            iniFile.WriteString(section, "LastTreeRecord", LastTreeRecord);
        }

        public static void DeleteKeys(IniFile iniFile, string section)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            iniFile.DeleteKey(section, "FileName");
            iniFile.DeleteKey(section, "WinL");
            iniFile.DeleteKey(section, "WinT");
            iniFile.DeleteKey(section, "WinR");
            iniFile.DeleteKey(section, "WinB");
            iniFile.DeleteKey(section, "WinState");
            iniFile.DeleteKey(section, "LastTreeRecord");
        }
    }
}
