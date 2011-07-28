using GKSys;
using System;
using System.Runtime.InteropServices;

namespace GKCore
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
			TGKSys.BuildCRCTable();
		}

		public void LoadFromFile([In] TIniFile aIniFile)
		{
			this.FUseProxy = aIniFile.ReadBool("Proxy", "UseProxy", false);
			this.FServer = aIniFile.ReadString("Proxy", "Server", "");
			this.FPort = aIniFile.ReadString("Proxy", "Port", "");
			this.FLogin = aIniFile.ReadString("Proxy", "Login", "");
			AnsiString s;
			s.Data = BDSSystem.LStrFromWStr(aIniFile.ReadString("Proxy", "Password", ""));
			this.FPassword = BDSSystem.WStrFromLStr(TGKSys.scDecrypt(s, (ushort)TGKSys.CrcStr("GEDKeeper")).Data);
		}

		public void SaveToFile([In] TIniFile aIniFile)
		{
			aIniFile.WriteBool("Proxy", "UseProxy", this.FUseProxy);
			aIniFile.WriteString("Proxy", "Server", this.FServer);
			aIniFile.WriteString("Proxy", "Port", this.FPort);
			aIniFile.WriteString("Proxy", "Login", this.FLogin);
			AnsiString s;
			s.Data = BDSSystem.LStrFromWStr(this.FPassword);
			aIniFile.WriteString("Proxy", "Password", BDSSystem.WStrFromLStr(TGKSys.scEncrypt(s, (ushort)TGKSys.CrcStr("GEDKeeper")).Data));
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
