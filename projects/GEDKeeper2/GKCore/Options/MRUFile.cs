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
using System.Windows.Forms;
using GKCommon;

namespace GKCore.Options
{
    public sealed class MRUFile
    {
        public string FileName;
        public ExtRect WinRect;
        public FormWindowState WinState;

        public MRUFile()
        {
        }

        public MRUFile(string fileName)
        {
            this.FileName = fileName;
        }

        public void LoadFromFile(IniFile iniFile, string section)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            this.FileName = iniFile.ReadString(section, "FileName", "");
            this.WinRect.Left = iniFile.ReadInteger(section, "WinL", 10);
            this.WinRect.Top = iniFile.ReadInteger(section, "WinT", 10);
            this.WinRect.Right = iniFile.ReadInteger(section, "WinR", 778);
            this.WinRect.Bottom = iniFile.ReadInteger(section, "WinB", 312);
            this.WinState = (FormWindowState)((uint)iniFile.ReadInteger(section, "WinState", 0));
        }

        public void SaveToFile(IniFile iniFile, string section)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteString(section, "FileName", this.FileName);
            iniFile.WriteInteger(section, "WinL", this.WinRect.Left);
            iniFile.WriteInteger(section, "WinT", this.WinRect.Top);
            iniFile.WriteInteger(section, "WinR", this.WinRect.Right);
            iniFile.WriteInteger(section, "WinB", this.WinRect.Bottom);
            iniFile.WriteInteger(section, "WinState", (int)this.WinState);
        }

        public static void DeleteKeys(IniFile iniFile, string section)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.DeleteKey(section, "FileName");
            iniFile.DeleteKey(section, "WinL");
            iniFile.DeleteKey(section, "WinT");
            iniFile.DeleteKey(section, "WinR");
            iniFile.DeleteKey(section, "WinB");
            iniFile.DeleteKey(section, "WinState");
        }
    }
}
