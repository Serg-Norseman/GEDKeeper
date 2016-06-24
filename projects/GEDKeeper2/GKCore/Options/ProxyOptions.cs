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

using GKCommon;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ProxyOptions : BaseObject
    {
        public string Server;
        public string Port;
        public string Login;
        public string Password;
        public bool UseProxy;

        public ProxyOptions()
        {
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null) return;

            this.UseProxy = iniFile.ReadBool("Proxy", "UseProxy", false);
            this.Server = iniFile.ReadString("Proxy", "Server", "");
            this.Port = iniFile.ReadString("Proxy", "Port", "");
            this.Login = iniFile.ReadString("Proxy", "Login", "");
            this.Password = SCCrypt.scDecrypt(iniFile.ReadString("Proxy", "Password", ""), unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null) return;

            iniFile.WriteBool("Proxy", "UseProxy", this.UseProxy);
            iniFile.WriteString("Proxy", "Server", this.Server);
            iniFile.WriteString("Proxy", "Port", this.Port);
            iniFile.WriteString("Proxy", "Login", this.Login);

            string pw = SCCrypt.scEncrypt(this.Password, unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
            iniFile.WriteString("Proxy", "Password", pw);
        }
    }
}
