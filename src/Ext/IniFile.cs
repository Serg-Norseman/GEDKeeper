using System;
using System.Text;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
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

    //[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
	public sealed class IniFile : IDisposable
	{
		private string FFileName;
		private bool Disposed_;

		public string FileName
		{
			get { return this.FFileName; }
		}

		public IniFile(string FileName)
		{
			this.FFileName = FileName;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.UpdateFile();
				this.Disposed_ = true;
			}
		}

		public int ReadInteger(string Section, string Ident, int Default)
		{
			string IntStr = this.ReadString(Section, Ident, "");
			if (!string.IsNullOrEmpty(IntStr))
			{
				if (IntStr.Length > 2 && IntStr[0] == '0' && (IntStr[1] == 'X' || IntStr[1] == 'x'))
				{
					IntStr = "$" + IntStr.Substring(2);
				}
			}
			return SysUtils.ParseInt(IntStr, Default);
		}

		public void WriteInteger(string Section, string Ident, int Value)
		{
			this.WriteString(Section, Ident, Value.ToString());
		}

		public bool ReadBool(string Section, string Ident, bool Default)
		{
			return this.ReadInteger(Section, Ident, (int)(Default ? 1 : 0)) > 0;
		}

		public void WriteBool(string Section, string Ident, bool Value)
		{
			this.WriteInteger(Section, Ident, (Value ? 1 : 0));
		}

		public DateTime ReadDateTime(string Section, string Name, DateTime Default)
		{
			string DateStr = this.ReadString(Section, Name, "");
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

		public void WriteDateTime(string Section, string Name, DateTime Value)
		{
			this.WriteString(Section, Name, Value.ToString());
		}

		public double ReadFloat(string Section, string Name, double Default)
		{
			string FloatStr = this.ReadString(Section, Name, "");
			double Result = Default;
			if (FloatStr != "")
			{
				try
				{
					Result = double.Parse(FloatStr);
				}
				catch (Exception)
				{
					throw;
				}
			}
			return Result;
		}

		public void WriteFloat(string Section, string Name, double Value)
		{
			this.WriteString(Section, Name, Value.ToString());
		}

		public string ReadString(string Section, string Ident, string Default)
		{
			StringBuilder Buffer = new StringBuilder(2048);
			string Result;
            if (Win32Native.GetPrivateProfileString(Section, Ident, Default, Buffer, (uint)Buffer.Capacity, this.FileName) != 0u)
			{
				Result = Buffer.ToString();
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public void WriteString(string Section, string Ident, string Value)
		{
            if (Win32Native.WritePrivateProfileString(Section, Ident, Value, this.FileName) == false)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", this.FileName));
			}
		}

		public void EraseSection(string Section)
		{
            if (Win32Native.WritePrivateProfileString(Section, IntPtr.Zero, IntPtr.Zero, this.FileName) == false)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", this.FileName));
			}
		}

		public void DeleteKey(string Section, string Ident)
		{
            Win32Native.WritePrivateProfileString(Section, Ident, IntPtr.Zero, this.FileName);
		}

		public void UpdateFile()
		{
            Win32Native.WritePrivateProfileString(IntPtr.Zero, IntPtr.Zero, IntPtr.Zero, this.FileName);
		}
	}
}
