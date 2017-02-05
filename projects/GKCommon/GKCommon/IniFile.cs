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
using Externals.IniFiles;

namespace GKCommon
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class IniFile : BaseObject
    {
        private readonly string fFileName;
        private readonly IniFileEx fHandler;


        public IniFile()
        {
            fFileName = "";
            fHandler = new IniFileEx();
        }

        public IniFile(string fileName)
        {
            fFileName = fileName;
            fHandler = IniFileEx.FromFile(fileName);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (!string.IsNullOrEmpty(fFileName)) {
                    fHandler.Save(fFileName);
                }
            }
            base.Dispose(disposing);
        }

        public int ReadInteger(string section, string ident, int defaultValue)
        {
            string intStr = ReadString(section, ident, "");

            if (!string.IsNullOrEmpty(intStr))
            {
                if (intStr.Length > 2 && intStr[0] == '0' && (intStr[1] == 'X' || intStr[1] == 'x'))
                {
                    intStr = intStr.Substring(2);
                    return Convert.ToInt32(intStr, 16);
                }
            }
            return SysUtils.ParseInt(intStr, defaultValue);
        }

        public void WriteInteger(string section, string ident, int value)
        {
            WriteString(section, ident, value.ToString());
        }

        public bool ReadBool(string section, string ident, bool defaultValue)
        {
            return ReadInteger(section, ident, (defaultValue ? 1 : 0)) > 0;
        }

        public void WriteBool(string section, string ident, bool value)
        {
            WriteInteger(section, ident, (value ? 1 : 0));
        }

        public DateTime ReadDateTime(string section, string name, DateTime defaultValue)
        {
            string dateStr = ReadString(section, name, "");
            DateTime result = (string.IsNullOrEmpty(dateStr)) ? defaultValue : DateTime.Parse(dateStr);
            return result;
        }

        public void WriteDateTime(string section, string name, DateTime value)
        {
            WriteString(section, name, value.ToString());
        }

        public double ReadFloat(string section, string name, double defaultValue)
        {
            string floatStr = ReadString(section, name, "");

            double result = (string.IsNullOrEmpty(floatStr)) ? defaultValue : double.Parse(floatStr);
            return result;
        }

        public void WriteFloat(string section, string name, double value)
        {
            WriteString(section, name, value.ToString());
        }

        public string ReadString(string section, string ident, string defaultValue)
        {
            string result = fHandler[section][ident];
            return (result == null) ? defaultValue : result;
        }

        public void WriteString(string section, string ident, string value)
        {
            fHandler[section][ident] = value;
        }

        public void DeleteSection(string section)
        {
            fHandler.DeleteSection(section);
        }

        public void DeleteKey(string section, string ident)
        {
            fHandler[section].DeleteKey(ident);
        }
    }
}
