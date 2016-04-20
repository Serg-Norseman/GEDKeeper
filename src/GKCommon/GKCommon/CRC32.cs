namespace GKCommon
{
	public static class CRC32
	{
		private static readonly uint[] CCITT32_TABLE;

        static CRC32()
        {
            CCITT32_TABLE = new uint[256];
            BuildCRCTable();
        }

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
					CCITT32_TABLE[(int)i] = val;
					i += 1u;
				}
				while (i != 256u);
			}
		}

		public static uint CrcStr(string str)
		{
			uint crc = 0u;
            if (!string.IsNullOrEmpty(str))
            {
                for (int i = 0; i < str.Length; i++)
                {
                    byte c = (byte)str[i];
                    crc = ((crc >> 8 & 16777215u) ^ CCITT32_TABLE[(int)((crc ^ (uint)c) & 255u)]);
                }
            }
			return crc;
		}
	}
}
