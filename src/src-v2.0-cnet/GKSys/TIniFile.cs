using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Text;

namespace GKSys
{

	public class EIniFileException : Exception
	{
		public EIniFileException()
		{
		}
		public EIniFileException(string message) : base(message)
		{
		}
		public EIniFileException(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	[SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=true)]
    [FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
	public class TIniFile : IDisposable
	{
		internal string FFileName;
		protected internal bool Disposed_;

		[Browsable(false)]
		public string FileName
		{
			get { return this.FFileName; }
		}

		public TIniFile([In] string FileName)
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

		public int ReadInteger([In] string Section, [In] string Ident, int Default)
		{
			string IntStr = this.ReadString(Section, Ident, "");
			if (((IntStr != null) ? IntStr.Length : 0) > 2 && IntStr[0] == '0' && (IntStr[1] == 'X' || IntStr[1] == 'x'))
			{
				IntStr = "$" + BDSSystem.WStrCopy(IntStr, 3, 2147483647);
			}
			return VCLUtils.StrToIntDef(IntStr, Default);
		}

		public void WriteInteger([In] string Section, [In] string Ident, int Value)
		{
			this.WriteString(Section, Ident, Value.ToString());
		}

		public bool ReadBool([In] string Section, [In] string Ident, bool Default)
		{
			return this.ReadInteger(Section, Ident, (int)(Default ? 1 : 0)) > 0;
		}

		public void WriteBool([In] string Section, [In] string Ident, bool Value)
		{
			this.WriteString(Section, Ident, VCLUtils.Values[Value ? 1 : 0]);
		}

		public int ReadBinaryStream([In] string Section, [In] string Name, TStream Value)
		{
			string Text = this.ReadString(Section, Name, "");
			int Result;
			if (Text != "")
			{
				TMemoryStream Stream;
				if (Value is TMemoryStream)
				{
					Stream = (Value as TMemoryStream);
				}
				else
				{
					Stream = new TMemoryStream();
				}
				try
				{
					int Pos = (int)Stream.Position;
					Stream.SetSize(Stream.Size + (long)(((Text != null) ? Text.Length : 0) / 2));
					VCLUtils.HexToBin(Encoding.Default.GetBytes(Text), 0, Stream.Memory, (int)Stream.Position, ((Text != null) ? Text.Length : 0) / 2);
					Stream.Position = (long)Pos;
					if (!object.Equals(Value, Stream))
					{
						Value.CopyFrom(Stream, (long)(((Text != null) ? Text.Length : 0) / 2));
					}
					Result = (int)(Stream.Size - (long)Pos);
				}
				finally
				{
					if (!object.Equals(Value, Stream))
					{
						Stream.Free();
					}
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}

		public void WriteBinaryStream([In] string Section, [In] string Name, TStream Value)
		{
			int num = (int)((Value.Size - Value.Position) * (long)((ulong)2));
			string Text = string.Empty;
			if (num > Text.Length)
			{
				Text = Text.PadRight(num);
			}
			else
			{
				Text = Text.Substring(0, num);
			}
			if (((Text != null) ? Text.Length : 0) > 0)
			{
				TMemoryStream Stream;
				if (Value is TMemoryStream)
				{
					Stream = (Value as TMemoryStream);
				}
				else
				{
					Stream = new TMemoryStream();
				}
				try
				{
					if (!object.Equals(Stream, Value))
					{
						Stream.CopyFrom(Value, Value.Size - Value.Position);
						Stream.Position = (long)((ulong)0);
					}
					byte[] Buffer = null;
					byte[] arg_93_0 = Buffer;
					int num2 = (int)(Stream.Size * (long)((ulong)2));
					byte[] array = arg_93_0;
					int arg_9D_0;
					if ((arg_9D_0 = num2) < 0)
					{
						arg_9D_0 = 0;
					}
					byte[] array2;
					byte[] expr_A2 = array2 = new byte[arg_9D_0];
					if (num2 > 0 && array != null)
					{
						int num3;
						if ((num3 = array.Length) > num2)
						{
							num3 = num2;
						}
						if (num3 > 0)
						{
							Array.Copy(array, array2, num3);
						}
					}
					Buffer = expr_A2;
					VCLUtils.BinToHex(Stream.Memory, (int)Stream.Position, ref Buffer, 0, (int)(Stream.Size - Stream.Position));
					Text = BDSSystem.StringOf(Buffer);
				}
				finally
				{
					if (!object.Equals(Value, Stream))
					{
						Stream.Free();
					}
				}
			}
			this.WriteString(Section, Name, Text);
		}

		public DateTime ReadDateTime([In] string Section, [In] string Name, DateTime Default)
		{
			string DateStr = this.ReadString(Section, Name, "");
			DateTime Result = Default;
			if (DateStr != "")
			{
				try
				{
					Result = DateTime.Parse(DateStr);
				}
				catch (GKSys.EConvertError E)
				{
				}
				catch (Exception E)
				{
					throw;
				}
			}
			return Result;
		}

		public void WriteDateTime([In] string Section, [In] string Name, DateTime Value)
		{
			this.WriteString(Section, Name, Value.ToString());
		}

		public double ReadFloat([In] string Section, [In] string Name, double Default)
		{
			string FloatStr = this.ReadString(Section, Name, "");
			double Result = Default;
			if (FloatStr != "")
			{
				try
				{
					Result = double.Parse(FloatStr);
				}
				catch (GKSys.EConvertError E)
				{
				}
				catch (Exception E)
				{
					throw;
				}
			}
			return Result;
		}

		public void WriteFloat([In] string Section, [In] string Name, double Value)
		{
			this.WriteString(Section, Name, Value.ToString());
		}

		public bool SectionExists([In] string Section)
		{
			TStrings S = new TStringList();
			this.ReadSection(Section, S);
			return S.Count > 0;
		}

		public void ReadSections([In] string Section, TStrings Strings)
		{
			this.ReadSections(Strings);
			int I = Strings.Count - 1;
			if (I >= 0)
			{
				do
				{
					if (!VCLUtils.SameText(Section, BDSSystem.WStrCopy(Strings[I], 1, (Section != null) ? Section.Length : 0)))
					{
						Strings.Delete(I);
					}
					I--;
				}
				while (I != -1);
			}
		}

		public bool ValueExists([In] string Section, [In] string Ident)
		{
			TStrings S = new TStringList();
			this.ReadSection(Section, S);
			return S.IndexOf(Ident) > -1;
		}

		public string ReadString([In] string Section, [In] string Ident, [In] string Default)
		{
			StringBuilder Buffer = new StringBuilder(2048);
			string Result;
			if (VCLUtils.GetPrivateProfileString(Section, Ident, Default, Buffer, (uint)Buffer.Capacity, this.FileName) != 0u)
			{
				Result = Buffer.ToString();
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public void WriteString([In] string Section, [In] string Ident, [In] string Value)
		{
			if (VCLUtils.WritePrivateProfileString(Section, Ident, Value, this.FileName) == (LongBool)0)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", new object[]
				{
					this.FileName
				}));
			}
		}

		public void ReadSection([In] string Section, TStrings Strings)
		{
			byte[] Buffer = null;
			byte[] array = Buffer;
			byte[] array2;
			byte[] expr_0D = array2 = new byte[16384];
			if (array != null)
			{
				int num;
				if ((num = array.Length) > 16384)
				{
					num = 16384;
				}
				if (num > 0)
				{
					Array.Copy(array, array2, num);
				}
			}
			Buffer = expr_0D;
			int Count = (int)VCLUtils.GetPrivateProfileString(Section, IntPtr.Zero, IntPtr.Zero, Buffer, (uint)((Buffer != null) ? ((uint)Buffer.Length) : 0u), this.FileName);
			if (Count != 0)
			{
				string S = BDSSystem.PlatformStringOf(Buffer);
				if (S == null)
				{
					S = string.Empty;
				}
				if (Count > S.Length)
				{
					S = S.PadRight(Count);
				}
				else
				{
					S = S.Substring(0, Count);
				}
				string LB = Strings.LineBreak;
				Strings.LineBreak = "\0";
				Strings.Text = S;
				Strings.LineBreak = LB;
			}
			else
			{
				Strings.Clear();
			}
		}

		public void ReadSections(TStrings Strings)
		{
			byte[] Buffer = null;
			byte[] array = Buffer;
			byte[] array2;
			byte[] expr_0D = array2 = new byte[16384];
			if (array != null)
			{
				int num;
				if ((num = array.Length) > 16384)
				{
					num = 16384;
				}
				if (num > 0)
				{
					Array.Copy(array, array2, num);
				}
			}
			Buffer = expr_0D;
			int Count = (int)VCLUtils.GetPrivateProfileString(IntPtr.Zero, IntPtr.Zero, IntPtr.Zero, Buffer, (uint)((Buffer != null) ? ((uint)Buffer.Length) : 0u), this.FileName);
			if (Count != 0)
			{
				string S = BDSSystem.PlatformStringOf(Buffer);
				if (S == null)
				{
					S = string.Empty;
				}
				if (Count > S.Length)
				{
					S = S.PadRight(Count);
				}
				else
				{
					S = S.Substring(0, Count);
				}
				string LB = Strings.LineBreak;
				Strings.LineBreak = "\0";
				Strings.Text = S;
				Strings.LineBreak = LB;
			}
			else
			{
				Strings.Clear();
			}
		}

		public void ReadSectionValues([In] string Section, TStrings Strings)
		{
			TStringList KeyList = new TStringList();
			this.ReadSection(Section, KeyList);
			Strings.BeginUpdate();
			try
			{
				Strings.Clear();
				int arg_24_0 = 0;
				int num = KeyList.Count - 1;
				int I = arg_24_0;
				if (num >= I)
				{
					num++;
					do
					{
						Strings.Add(KeyList[I] + "=" + this.ReadString(Section, KeyList[I], ""));
						I++;
					}
					while (I != num);
				}
			}
			finally
			{
				Strings.EndUpdate();
			}
		}
		public void EraseSection([In] string Section)
		{
			if (VCLUtils.WritePrivateProfileString(Section, IntPtr.Zero, IntPtr.Zero, this.FileName) == (LongBool)0)
			{
				throw new EIniFileException(string.Format("Unable to write to {0}", new object[]
				{
					this.FileName
				}));
			}
		}
		public void DeleteKey([In] string Section, [In] string Ident)
		{
			VCLUtils.WritePrivateProfileString(Section, Ident, IntPtr.Zero, this.FileName);
		}
		public void UpdateFile()
		{
			VCLUtils.WritePrivateProfileString(IntPtr.Zero, IntPtr.Zero, IntPtr.Zero, this.FileName);
		}
		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
