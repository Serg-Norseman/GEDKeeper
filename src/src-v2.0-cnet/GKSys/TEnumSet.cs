using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TEnumSet
	{
		internal uint FValue;

		public static TEnumSet Create()
		{
			TEnumSet Result = new TEnumSet();
			Result.FValue = 0u;
			return Result;
		}

		public static TEnumSet Create(params Enum[] e)
		{
			TEnumSet Result = TEnumSet.Create();
			Result.Include(e);
			return Result;
		}

		public void Include(params Enum[] e)
		{
			//e = (Enum[])e.Clone();
			for (int i = 0; i <= e.Length - 1; i++) {
				this.Include(e[i]);
			}
		}

		public void Include(Enum e)
		{
			byte pos = Convert.ToByte(e);
			this.FValue |= (uint)(1 << (int)pos);
		}

		public void Exclude(Enum e)
		{
			byte pos = Convert.ToByte(e);
			this.FValue &= (uint)(1 << (int)pos ^ -1);
		}

		public bool InSet(Enum e)
		{
			byte pos = Convert.ToByte(e);
			uint bt = (uint)(1 << (int)pos);
			return (bt & this.FValue) > 0u;
		}

		public bool IsEmpty()
		{
			return this.FValue == 0u;
		}

		public string ToString(byte B)
		{
			byte bt = 1;
			string s = "";
			int i = 1;
			do
			{
				if ((B & bt) > 0)
				{
					s = "1" + s;
				}
				else
				{
					s = "0" + s;
				}
				bt = (byte)((int)((uint)bt) << 1);
				i++;
			}
			while (i != 9);
			return s;
		}

	}
}
