using System;
using System.Text;

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

		public string FileName
		{
			get { return this.fFileName; }
		}

		public IniFile(string fileName)
		{
			this.fFileName = fileName;
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
			string IntStr = this.ReadString(section, ident, "");
			if (!string.IsNullOrEmpty(IntStr))
			{
				if (IntStr.Length > 2 && IntStr[0] == '0' && (IntStr[1] == 'X' || IntStr[1] == 'x'))
				{
					IntStr = "$" + IntStr.Substring(2);
				}
			}
			return SysUtils.ParseInt(IntStr, Default);
		}

		public void WriteInteger(string section, string ident, int value)
		{
			this.WriteString(section, ident, value.ToString());
		}

		public bool ReadBool(string section, string ident, bool Default)
		{
			return this.ReadInteger(section, ident, (int)(Default ? 1 : 0)) > 0;
		}

		public void WriteBool(string section, string ident, bool value)
		{
			this.WriteInteger(section, ident, (value ? 1 : 0));
		}

		public DateTime ReadDateTime(string section, string name, DateTime Default)
		{
			string DateStr = this.ReadString(section, name, "");
			DateTime Result = Default;
			if (DateStr != "")
			{
				try
				{
					Result = DateTime.Parse(DateStr);
				}
				catch (Exception)
				{
					throw;
				}
			}
			return Result;
		}

		public void WriteDateTime(string section, string name, DateTime value)
		{
			this.WriteString(section, name, value.ToString());
		}

		public double ReadFloat(string section, string name, double Default)
		{
			string floatStr = this.ReadString(section, name, "");
			double result = Default;
			if (floatStr != "")
			{
				try
				{
					result = double.Parse(floatStr);
				}
				catch (Exception)
				{
					throw;
				}
			}
			return result;
		}

		public void WriteFloat(string section, string name, double value)
		{
			this.WriteString(section, name, value.ToString());
		}

		public string ReadString(string section, string ident, string Default)
		{
			StringBuilder buffer = new StringBuilder(2048);

		    uint res = Win32Native.GetPrivateProfileString(section, ident, Default, buffer, (uint) buffer.Capacity,
		                                                   this.FileName);

            string result = (res != 0u) ? buffer.ToString() : "";
			return result;
		}

		public void WriteString(string section, string ident, string value)
		{
            if (Win32Native.WritePrivateProfileString(section, ident, value, this.FileName) == false)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", this.FileName));
			}
		}

		public void EraseSection(string section)
		{
            if (Win32Native.WritePrivateProfileString(section, IntPtr.Zero, IntPtr.Zero, this.FileName) == false)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", this.FileName));
			}
		}

		public void DeleteKey(string section, string ident)
		{
            Win32Native.WritePrivateProfileString(section, ident, IntPtr.Zero, this.FileName);
		}

		public void UpdateFile()
		{
            Win32Native.WritePrivateProfileString(IntPtr.Zero, IntPtr.Zero, IntPtr.Zero, this.FileName);
		}
	}
}
