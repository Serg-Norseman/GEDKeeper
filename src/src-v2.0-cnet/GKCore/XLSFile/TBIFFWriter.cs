using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Text;

namespace XLSFile
{
	public class TBIFFWriter
	{
		public TStream Stream;

		public void WriteStr(string s)
		{
			int len = s.Length;
			this.WriteByte((byte)len);

			byte[] bytes = Encoding.GetEncoding(1251).GetBytes(s);
			
			for (int i = 0; i <= bytes.Length - 1; i++) {
				this.Stream.Write(bytes[i]);
			}
		}

		public void WriteByte(byte b)
		{
			this.Stream.Write(b, 1);
		}

		public void WriteDouble(double d)
		{
			this.Stream.Write(d, 8);
		}

		public void WriteInt(int i)
		{
			this.Stream.Write(i, 4);
		}

		public void WriteWord(ushort w)
		{
			this.Stream.Write(w, 2);
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
