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
