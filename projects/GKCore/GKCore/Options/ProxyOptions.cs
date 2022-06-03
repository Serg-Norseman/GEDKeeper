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
using BSLib;
using GKCore.Interfaces;

namespace GKCore.Options
{
    /// <summary>
    ///
    /// </summary>
    public sealed class ProxyOptions : IOptions
    {
        public string Server;
        public string Port;
        public string Login;
        public string Password;
        public bool UseProxy;

        public void Assign(IOptions source)
        {
            ProxyOptions srcOptions = source as ProxyOptions;
            if (srcOptions == null) return;

            Server = srcOptions.Server;
            Port = srcOptions.Port;
            Login = srcOptions.Login;
            Password = srcOptions.Password;
            UseProxy = srcOptions.UseProxy;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            UseProxy = iniFile.ReadBool("Proxy", "UseProxy", false);
            Server = iniFile.ReadString("Proxy", "Server", "");
            Port = iniFile.ReadString("Proxy", "Port", "");
            Login = iniFile.ReadString("Proxy", "Login", "");
            Password = SCCrypt.scDecrypt(iniFile.ReadString("Proxy", "Password", ""), unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteBool("Proxy", "UseProxy", UseProxy);
            iniFile.WriteString("Proxy", "Server", Server);
            iniFile.WriteString("Proxy", "Port", Port);
            iniFile.WriteString("Proxy", "Login", Login);

            string pw = SCCrypt.scEncrypt(Password, unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
            iniFile.WriteString("Proxy", "Password", pw);
        }
    }
}
