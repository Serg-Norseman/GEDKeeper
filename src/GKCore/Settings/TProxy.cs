using System;
using System.Runtime.InteropServices;

using GKSys;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore.Settings
{
	public class TProxy
	{
		private string FServer;
		private string FPort;
		private string FLogin;
		private string FPassword;
		private bool FUseProxy;

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

		public TProxy()
		{
			SysUtils.BuildCRCTable();
		}

		public void LoadFromFile([In] IniFile aIniFile)
		{
			this.FUseProxy = aIniFile.ReadBool("Proxy", "UseProxy", false);
			this.FServer = aIniFile.ReadString("Proxy", "Server", "");
			this.FPort = aIniFile.ReadString("Proxy", "Port", "");
			this.FLogin = aIniFile.ReadString("Proxy", "Login", "");
			this.FPassword = SysUtils.scDecrypt(aIniFile.ReadString("Proxy", "Password", ""), unchecked((ushort)SysUtils.CrcStr("GEDKeeper")));
		}

		public void SaveToFile([In] IniFile aIniFile)
		{
			aIniFile.WriteBool("Proxy", "UseProxy", this.FUseProxy);
			aIniFile.WriteString("Proxy", "Server", this.FServer);
			aIniFile.WriteString("Proxy", "Port", this.FPort);
			aIniFile.WriteString("Proxy", "Login", this.FLogin);
			
			string pw = SysUtils.scEncrypt(this.FPassword, unchecked((ushort)SysUtils.CrcStr("GEDKeeper")));
			aIniFile.WriteString("Proxy", "Password", pw);
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
