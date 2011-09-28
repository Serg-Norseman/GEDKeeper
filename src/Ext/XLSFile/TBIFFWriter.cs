using System;
using System.IO;
using System.Text;

using GKCore.Sys;

namespace XLSFile
{
	public class TBIFFWriter
	{
		public Stream WStream;

		public void WriteByte(byte b)
		{
			this.WStream.Write(BitConverter.GetBytes(b), 0, 1);
		}

		public void WriteDouble(double d)
		{
			this.WStream.Write(BitConverter.GetBytes(d), 0, 8);
		}

		public void WriteInt(int i)
		{
			this.WStream.Write(BitConverter.GetBytes(i), 0, 4);
		}

		public void WriteWord(ushort w)
		{
			this.WStream.Write(BitConverter.GetBytes(w), 0, 2);
		}

		public void WriteStr(string s)
		{
			this.WriteByte((byte)s.Length);
			byte[] bytes = Encoding.GetEncoding(1251).GetBytes(s);
			this.WStream.Write(bytes, 0, bytes.Length);
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
