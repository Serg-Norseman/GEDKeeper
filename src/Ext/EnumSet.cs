using System;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
{
	public struct EnumSet
	{
		private uint FValue;

		public static EnumSet Create()
		{
			EnumSet Result = new EnumSet();
			Result.FValue = 0u;
			return Result;
		}

		public static EnumSet Create(params Enum[] e)
		{
			EnumSet Result = EnumSet.Create();
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
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				this.FValue |= (uint)(1 << (int)pos);
			}
		}

		public void Exclude(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				this.FValue &= (uint)(1 << (int)pos ^ -1);
			}
		}

		public bool InSet(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				uint bt = (uint)(1 << (int)pos);
				return (bt & this.FValue) > 0u;
			}
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
