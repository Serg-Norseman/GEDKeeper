using System;
using System.Runtime.InteropServices;
using System.Text;

#if GK_LINUX
using Externals.IniFiles;
#endif

namespace GKCommon
{
    [Serializable]
	public class EIniFileException : Exception
	{
		public EIniFileException()
		{
		}
		public EIniFileException(string message) : base(message)
		{
		}
	}

    /// <summary>
    /// 
    /// </summary>
    //[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
	public sealed class IniFile : IDisposable
	{
		private readonly string fFileName;
		private bool fDisposed;

		#if GK_LINUX
		private readonly IniFileEx fHandler;
		#endif

		public string FileName
		{
			get { return this.fFileName; }
		}

		public IniFile(string fileName)
		{
			this.fFileName = fileName;
			#if GK_LINUX
			this.fHandler = IniFileEx.FromFile(fileName);
			#endif
		}

		public void Dispose()
		{
			if (!this.fDisposed)
			{
				this.UpdateFile();
				this.fDisposed = true;
			}
		}

		public int ReadInteger(string section, string ident, int Default)
		{
			string intStr = this.ReadString(section, ident, "");

            if (!string.IsNullOrEmpty(intStr))
			{
				if (intStr.Length > 2 && intStr[0] == '0' && (intStr[1] == 'X' || intStr[1] == 'x'))
				{
					intStr = "$" + intStr.Substring(2);
				}
			}
			return ConvHelper.ParseInt(intStr, Default);
		}

		public void WriteInteger(string section, string ident, int value)
		{
			this.WriteString(section, ident, value.ToString());
		}

		public bool ReadBool(string section, string ident, bool Default)
		{
			return this.ReadInteger(section, ident, (Default ? 1 : 0)) > 0;
		}

		public void WriteBool(string section, string ident, bool value)
		{
			this.WriteInteger(section, ident, (value ? 1 : 0));
		}

		public DateTime ReadDateTime(string section, string name, DateTime Default)
		{
			string dateStr = this.ReadString(section, name, "");

            DateTime result = (string.IsNullOrEmpty(dateStr)) ? Default : DateTime.Parse(dateStr);
			return result;
		}

		public void WriteDateTime(string section, string name, DateTime value)
		{
			this.WriteString(section, name, value.ToString());
		}

		public double ReadFloat(string section, string name, double Default)
		{
			string floatStr = this.ReadString(section, name, "");

            double result = (string.IsNullOrEmpty(floatStr)) ? Default : double.Parse(floatStr);
			return result;
		}

		public void WriteFloat(string section, string name, double value)
		{
			this.WriteString(section, name, value.ToString());
		}

		public string ReadString(string section, string ident, string Default)
		{
			#if GK_LINUX
			string result = this.fHandler[section][ident];
			return result == null ? Default : result;
			#else
			StringBuilder buffer = new StringBuilder(2048);

		    uint res = GetPrivateProfileString(section, ident, Default, buffer, (uint) buffer.Capacity, this.FileName);

            string result = (res != 0u) ? buffer.ToString() : "";
			return result;
			#endif
		}

		public void WriteString(string section, string ident, string value)
		{
			#if GK_LINUX
			this.fHandler[section][ident] = value;
			#else
            if (WritePrivateProfileString(section, ident, value, this.FileName) == false)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", this.FileName));
			}
            #endif
		}

		/*public void EraseSection(string section)
		{
            if (WritePrivateProfileString(section, IntPtr.Zero, IntPtr.Zero, this.FileName) == false)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", this.FileName));
			}
		}*/

		public void DeleteKey(string section, string ident)
		{
			#if GK_LINUX
			this.fHandler[section].DeleteKey(ident);
			#else
            WritePrivateProfileString(section, ident, IntPtr.Zero, this.FileName);
            #endif
		}

		public void UpdateFile()
		{
			#if GK_LINUX
			this.fHandler.Save(this.fFileName);
			#else
            WritePrivateProfileString(IntPtr.Zero, IntPtr.Zero, IntPtr.Zero, this.FileName);
            #endif
		}

		#region NativeMethods

		#if GK_LINUX

		#else
        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern uint GetPrivateProfileString(string lpAppName, string lpKeyName, string lpDefault, StringBuilder lpReturnedString, uint nSize, string lpFileName);

        //[DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        //private static extern uint GetPrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

        //[DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        //private static extern uint GetPrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool WritePrivateProfileString(string lpAppName, string lpKeyName, string lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool WritePrivateProfileString(string lpAppName, string lpKeyName, IntPtr lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool WritePrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool WritePrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);
        #endif
        
        #endregion
	}
}
