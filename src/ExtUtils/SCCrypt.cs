using System;
using System.Runtime.InteropServices;
using System.Text;

/// <summary>
/// 
/// </summary>

namespace ExtUtils
{
	public static class SCCrypt
	{

		private static byte[] LStrConcat2([In] byte[] L, [In] byte[] R)
		{
			byte[] result = null;
			int num = ((L != null) ? L.Length : 0);
			int num2 = ((R != null) ? R.Length : 0);
			if (num + num2 > 0)
			{
				result = new byte[num + num2];
				if (num > 0) Array.Copy(L, 0, result, 0, num);
				if (num2 > 0) Array.Copy(R, 0, result, num, num2);
			}
			return result;
		}

		private static byte[] LStrCopy([In] byte[] S, int Index1, int Count)
		{
			byte[] result = null;
			if (Count > 0)
			{
				int num = ((S != null) ? S.Length : 0);
				if (num > 0 && Index1 <= num)
				{
					int idx = ((Index1 <= 0) ? 0 : Index1 - 1);

					if (Count > num - idx)
					{
						Count = num - idx;
					}

					if (Count > 0)
					{
						result = new byte[Count];
						Array.Copy(S, idx, result, 0, Count);
					}
				}
			}
			return result;
		}

		private static void LStrDelete(ref byte[] Dest, int Index1, int Count)
		{
			if (Count > 0)
			{
				int num = ((Dest != null) ? Dest.Length : 0);
				if (num > 0 && Index1 <= num)
				{
					int num2 = ((Index1 <= 0) ? 0 : Index1 - 1);

					if (Count > num - num2)
					{
						Count = num - num2;
					}
					if (Count > 0)
					{
						int num3 = num - Count;
						if (num3 < 0)
						{
							num3 = 0;
						}

						byte[] array = new byte[num3];
						if (num2 > 0)
						{
							Array.Copy(Dest, 0, array, 0, num2);
						}
						if (num2 + Count < num)
						{
							Array.Copy(Dest, num2 + Count, array, num2, num - Count - num2);
						}
						Dest = array;
					}
				}
			}
		}

		private static void MoveL2S([In] uint Source, ref byte[] Dest, int count)
		{
			byte[] bytes = new byte[4];

			unchecked
			{
				ushort wl = (ushort)(Source);
				ushort wh = (ushort)(Source >> 16);

				bytes[0] = (byte)wl;
				bytes[1] = (byte)(wl >> 8);
				bytes[2] = (byte)wh;
				bytes[3] = (byte)(wh >> 8);
			}

			if (Dest != null) {
				for (int I = 0; I < count; I++) Dest[I] = bytes[I];
			}
		}

		private static void MoveS2L([In] byte[] source, out int dest, int count)
		{
			byte[] bytes = new byte[4];
			for (int I = 1; I <= 4; I++) {
				if (I <= count) {
					bytes[I - 1] = source[I - 1];
				} else {
					bytes[I - 1] = 0;
				}
			}

			dest = (int)((bytes[0] | bytes[1] << 8) | (bytes[2] | bytes[3] << 8) << 16);
		}

		private static byte[] Decode([In] byte[] data)
		{
			int num = (data != null) ? data.Length : 0;
			byte[] result = null;
			uint I;

			switch (num) {
				case 2:
					I = (uint)(_Unnamed1_Map[data[0]] + (_Unnamed1_Map[data[1]] << 6));
					result = new byte[1];
					MoveL2S(I, ref result, 1);
					break;
				case 3:
					I = (uint)(_Unnamed1_Map[data[0]] + (_Unnamed1_Map[data[1]] << 6) + (_Unnamed1_Map[data[2]] << 12));
					result = new byte[2];
					MoveL2S(I, ref result, 2);
					break;
				case 4:
					I = (uint)(_Unnamed1_Map[data[0]] + (_Unnamed1_Map[data[1]] << 6) + (_Unnamed1_Map[data[2]] << 12) + (_Unnamed1_Map[data[3]] << 18));
					result = new byte[3];
					MoveL2S(I, ref result, 3);
					break;
			}
			
			return result;
		}

		private static byte[] Encode([In] byte[] data)
		{
			int I = 0;
			int num = (data != null) ? data.Length : 0;
            MoveS2L(data, out I, num);

			byte[] res = new byte[num + 1];

			switch (num) {
				case 1:
					res[0] = (byte)_Unnamed2_Map[I % 64];
					res[1] = (byte)_Unnamed2_Map[((uint)I >> 6) % 64];
					break;
				case 2:
					res[0] = (byte)_Unnamed2_Map[I % 64];
					res[1] = (byte)_Unnamed2_Map[((uint)I >> 6) % 64];
					res[2] = (byte)_Unnamed2_Map[((uint)I >> 12) % 64];
					break;
				case 3:
					res[0] = (byte)_Unnamed2_Map[I % 64];
					res[1] = (byte)_Unnamed2_Map[((uint)I >> 6) % 64];
					res[2] = (byte)_Unnamed2_Map[((uint)I >> 12) % 64];
					res[3] = (byte)_Unnamed2_Map[((uint)I >> 18) % 64];
					break;
			}
			
			return res;
		}

		public static string scDecrypt([In] string St, ushort Key)
		{
			string res = "";

			if (!string.IsNullOrEmpty(St))
			{
				byte[] SSD = Encoding.ASCII.GetBytes(St);
				byte[] ppd = null;
				while (SSD.Length != 0)
				{
					byte[] sd = LStrCopy(SSD, 1, 4);
					ppd = LStrConcat2(ppd, Decode(sd));
					LStrDelete(ref SSD, 1, 4);
				}

				byte[] tmp = (byte[])ppd.Clone();

				ushort Seed = Key;
				for (int I = 1; I <= ppd.Length; I++)
				{
					tmp[I - 1] = (byte)((uint)tmp[I - 1] ^ (uint)Seed >> 8);
					Seed = unchecked((ushort)(((uint)ppd[I - 1] + (uint)Seed) * 28732u + 28446u));
				}
				res = Encoding.ASCII.GetString(tmp);
			}

			return res;
		}

		public static string scEncrypt([In] string St, ushort Key)
		{
			string res = "";

			if (!string.IsNullOrEmpty(St))
			{
				ushort Seed = Key;
				byte[] idata = Encoding.ASCII.GetBytes(St);
				for (int I = 1; I <= idata.Length; I++)
				{
					idata[I - 1] = (byte)((uint)idata[I - 1] ^ (uint)Seed >> 8);
					Seed = unchecked((ushort)(((uint)idata[I - 1] + (uint)Seed) * 28732u + 28446u));
				}

				byte[] res_data = null;

				while (idata.Length != 0)
				{
					byte[] sd = LStrCopy(idata, 1, 3);
					res_data = LStrConcat2(res_data, Encode(sd));
					LStrDelete(ref idata, 1, 3);
				}
				res = Encoding.ASCII.GetString(res_data);
			}

			return res;
		}

		private static readonly byte[] _Unnamed1_Map;
		private static readonly char[] _Unnamed2_Map;

		static SCCrypt()
		{
			_Unnamed1_Map = new byte[]
			{
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				62,
				0,
				0,
				0,
				63,
				52,
				53,
				54,
				55,
				56,
				57,
				58,
				59,
				60,
				61,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				1,
				2,
				3,
				4,
				5,
				6,
				7,
				8,
				9,
				10,
				11,
				12,
				13,
				14,
				15,
				16,
				17,
				18,
				19,
				20,
				21,
				22,
				23,
				24,
				25,
				0,
				0,
				0,
				0,
				0,
				0,
				26,
				27,
				28,
				29,
				30,
				31,
				32,
				33,
				34,
				35,
				36,
				37,
				38,
				39,
				40,
				41,
				42,
				43,
				44,
				45,
				46,
				47,
				48,
				49,
				50,
				51,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0,
				0
			};

			_Unnamed2_Map = new char[]
			{
				'A',
				'B',
				'C',
				'D',
				'E',
				'F',
				'G',
				'H',
				'I',
				'J',
				'K',
				'L',
				'M',
				'N',
				'O',
				'P',
				'Q',
				'R',
				'S',
				'T',
				'U',
				'V',
				'W',
				'X',
				'Y',
				'Z',
				'a',
				'b',
				'c',
				'd',
				'e',
				'f',
				'g',
				'h',
				'i',
				'j',
				'k',
				'l',
				'm',
				'n',
				'o',
				'p',
				'q',
				'r',
				's',
				't',
				'u',
				'v',
				'w',
				'x',
				'y',
				'z',
				'0',
				'1',
				'2',
				'3',
				'4',
				'5',
				'6',
				'7',
				'8',
				'9',
				'+',
				'/'
			};
		}

	}
}
