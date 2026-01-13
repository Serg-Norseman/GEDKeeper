/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Utilities;

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

        public ProxyOptions()
        {
            ResetDefaults();
        }

        public void ResetDefaults()
        {
            Server = string.Empty;
            Port = string.Empty;
            Login = string.Empty;
            Password = string.Empty;
            UseProxy = false;
        }

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
                throw new ArgumentNullException(nameof(iniFile));

            UseProxy = iniFile.ReadBool("Proxy", "UseProxy", false);
            Server = iniFile.ReadString("Proxy", "Server", "");
            Port = iniFile.ReadString("Proxy", "Port", "");
            Login = iniFile.ReadString("Proxy", "Login", "");
            Password = SCCrypt.scDecrypt(iniFile.ReadString("Proxy", "Password", ""), unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            iniFile.WriteBool("Proxy", "UseProxy", UseProxy);
            iniFile.WriteString("Proxy", "Server", Server);
            iniFile.WriteString("Proxy", "Port", Port);
            iniFile.WriteString("Proxy", "Login", Login);

            string pw = SCCrypt.scEncrypt(Password, unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
            iniFile.WriteString("Proxy", "Password", pw);
        }
    }
}
