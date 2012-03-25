using System;

namespace Ext.Utils
{
	public sealed class CRC32
	{
		private static uint[] Ccitt32Table = new uint[256];

		private static void BuildCRCTable()
		{
			unchecked
			{
				uint i = 0u;
				do
				{
					uint val = i;
					uint j = 4294967288u;
					do
					{
						if ((val & 1u) != 0u)
						{
							val = (val >> 1 ^ 3988292384u);
						}
						else
						{
							val >>= 1;
						}
						j += 1u;
					}
					while (j != 0u);
					Ccitt32Table[(int)i] = val;
					i += 1u;
				}
				while (i != 256u);
			}
		}

		public static uint CrcStr(string Str)
		{
			uint crc = 0u;
			int num = (Str != null) ? Str.Length : 0;
			for (int i = 1; i <= num; i++)
			{
				byte c = (byte)Str[i - 1];
				crc = ((crc >> 8 & 16777215u) ^ Ccitt32Table[(int)((crc ^ (uint)c) & 255u)]);
			}
			return crc;
		}

		static CRC32()
		{
			BuildCRCTable();
		}

		public CRC32()
		{
		}
	}
}
