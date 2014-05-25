using System;
using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore.Options
{
    public sealed class ProxyOptions : IDisposable
	{
		private string FServer;
		private string FPort;
		private string FLogin;
		private string FPassword;
		private bool FUseProxy;

        private bool fDisposed;

		public string Server
		{
			get { return this.FServer; }
			set { this.FServer = value; }
		}

		public string Port
		{
			get { return this.FPort; }
			set { this.FPort = value; }
		}

		public string Login
		{
			get { return this.FLogin; }
			set { this.FLogin = value; }
		}

		public string Password
		{
			get { return this.FPassword; }
			set { this.FPassword = value; }
		}

		public bool UseProxy
		{
			get { return this.FUseProxy; }
			set { this.FUseProxy = value; }
		}

		public ProxyOptions()
		{
		}

        public void Dispose()
        {
            if (!this.fDisposed)
            {
                this.fDisposed = true;
            }
        }

		public void LoadFromFile(IniFile iniFile)
		{
            if (iniFile == null) return;

			this.FUseProxy = iniFile.ReadBool("Proxy", "UseProxy", false);
			this.FServer = iniFile.ReadString("Proxy", "Server", "");
			this.FPort = iniFile.ReadString("Proxy", "Port", "");
			this.FLogin = iniFile.ReadString("Proxy", "Login", "");
			this.FPassword = SCCrypt.scDecrypt(iniFile.ReadString("Proxy", "Password", ""), unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
		}

		public void SaveToFile(IniFile iniFile)
		{
            if (iniFile == null) return;

			iniFile.WriteBool("Proxy", "UseProxy", this.FUseProxy);
			iniFile.WriteString("Proxy", "Server", this.FServer);
			iniFile.WriteString("Proxy", "Port", this.FPort);
			iniFile.WriteString("Proxy", "Login", this.FLogin);

			string pw = SCCrypt.scEncrypt(this.FPassword, unchecked((ushort)CRC32.CrcStr("GEDKeeper")));
			iniFile.WriteString("Proxy", "Password", pw);
		}

    }
}
