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
using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ProxyOptions : BaseObject, IOptions
    {
        public string Server;
        public string Port;
        public string Login;
        public string Password;
        public bool UseProxy;

        public ProxyOptions()
        {
        }

        public void Assign(IOptions source)
        {
            ProxyOptions srcOptions = source as ProxyOptions;
            if (srcOptions == null) return;

            this.Server = srcOptions.Server;
            this.Port = srcOptions.Port;
            this.Login = srcOptions.Login;
            this.Password = srcOptions.Password;
            this.UseProxy = srcOptions.UseProxy;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            this.UseProxy = iniFile.ReadBool("Proxy", "UseProxy", false);
            this.Server = iniFile.ReadString("Proxy", "Server", "");
            this.Port = iniFile.ReadString("Proxy", "Port", "");
            this.Login = iniFile.ReadString("Proxy", "Login", "");
            this.Password = SCCrypt.scDecrypt(iniFile.ReadString("Proxy", "Password", ""), unchecked((ushort)SysUtils.CrcStr("GEDKeeper")));
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteBool("Proxy", "UseProxy", this.UseProxy);
            iniFile.WriteString("Proxy", "Server", this.Server);
            iniFile.WriteString("Proxy", "Port", this.Port);
            iniFile.WriteString("Proxy", "Login", this.Login);

            string pw = SCCrypt.scEncrypt(this.Password, unchecked((ushort)SysUtils.CrcStr("GEDKeeper")));
            iniFile.WriteString("Proxy", "Password", pw);
        }
    }
}
