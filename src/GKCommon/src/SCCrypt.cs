using System;
using System.Security.Cryptography;
using System.Text;

namespace GKCommon
{
    /// <summary>
    /// 
    /// </summary>
    public static class SCCrypt
	{
		public static byte[] CreateRandomSalt(int length)
		{
			// Create a buffer
			byte[] randBytes;

			if (length >= 1)
			{
				randBytes = new byte[length];
			}
			else
			{
				randBytes = new byte[1];
			}

			// Create a new RNGCryptoServiceProvider.
			RNGCryptoServiceProvider rand = new RNGCryptoServiceProvider();

			// Fill the buffer with random bytes.
			rand.GetBytes(randBytes);

			// return the bytes.
			return randBytes;
		}

		public static void ClearBytes(byte[] buffer)
		{
			// Check arguments.
			if (buffer == null)
			{
				throw new ArgumentException("buffer");
			}

			// Set each byte in the buffer to 0.
			for (int x = 0; x < buffer.Length; x++)
			{
				buffer[x] = 0;
			}
		}
		
		private static byte[] ArrConcat(byte[] L, byte[] R)
		{
			int num = ((L != null) ? L.Length : 0);
			int num2 = ((R != null) ? R.Length : 0);

			byte[] result = new byte[num + num2];
			if (num > 0) Array.Copy(L, 0, result, 0, num);
			if (num2 > 0) Array.Copy(R, 0, result, num, num2);
			return result;
		}

		private static byte[] ArrCopy(byte[] src, int index, int count)
		{
			byte[] result = null;
			if (count > 0)
			{
				int srcLen = ((src != null) ? src.Length : 0);
				index = (index < 0) ? 0 : index;

				if (srcLen > 0 && index < srcLen)
				{
					if (count > srcLen - index) {
						count = srcLen - index;
					}

					if (count > 0) {
						result = new byte[count];
						Array.Copy(src, index, result, 0, count);
					}
				}
			}
			return result;
		}

		private static byte[] MoveL2S(uint source, int count)
		{
			byte[] dest = new byte[count];

			unchecked
			{
				ushort wl = (ushort)(source);
				ushort wh = (ushort)(source >> 16);

				if (count >= 1) dest[0] = (byte)wl;
				if (count >= 2) dest[1] = (byte)(wl >> 8);
				if (count >= 3) dest[2] = (byte)wh;
				if (count >= 4) dest[3] = (byte)(wh >> 8);
			}

			return dest;
		}

		private static byte[] Decode(byte[] data)
		{
			int num = (data != null) ? data.Length : 0;
			byte[] result = null;
			uint I;

			switch (num) {
				case 2:
					I = (uint)(_Unnamed1_Map[data[0]] + (_Unnamed1_Map[data[1]] << 6));
					result = MoveL2S(I, 1);
					break;

				case 3:
					I = (uint)(_Unnamed1_Map[data[0]] + (_Unnamed1_Map[data[1]] << 6) + (_Unnamed1_Map[data[2]] << 12));
					result = MoveL2S(I, 2);
					break;

				case 4:
					I = (uint)(_Unnamed1_Map[data[0]] + (_Unnamed1_Map[data[1]] << 6) + (_Unnamed1_Map[data[2]] << 12) + (_Unnamed1_Map[data[3]] << 18));
					result = MoveL2S(I, 3);
					break;
			}
			
			return result;
		}

		private static int MoveS2L(byte[] source, int count)
		{
			byte[] bytes = new byte[4];
			bytes[0] = (byte)((count >= 1) ? source[0] : 0);
			bytes[1] = (byte)((count >= 2) ? source[1] : 0);
			bytes[2] = (byte)((count >= 3) ? source[2] : 0);
			bytes[3] = (byte)((count >= 4) ? source[3] : 0);
			
			int dest;
			dest = (int)((bytes[0] | bytes[1] << 8) | (bytes[2] | bytes[3] << 8) << 16);
			return dest;
		}

		private static byte[] Encode(byte[] data)
		{
			int num = (data != null) ? data.Length : 0;
            int I = MoveS2L(data, num);

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

		public static string scDecrypt(string str, ushort key)
		{
			string res = "";

			if (!string.IsNullOrEmpty(str))
			{
				byte[] SSD = Encoding.ASCII.GetBytes(str);
				byte[] ppd = null;
				
				int idx = 0;
				while (idx < SSD.Length)
				{
					byte[] sd = ArrCopy(SSD, idx, 4);
					ppd = ArrConcat(ppd, Decode(sd));
					idx += sd.Length;
				}

				byte[] tmp = (byte[])ppd.Clone();

				uint seed = key;
				for (int i = 0; i < ppd.Length; i++)
				{
					tmp[i] = (byte)((uint)tmp[i] ^ seed >> 8);
					seed = unchecked((ushort)(((uint)ppd[i] + seed) * 28732u + 28446u));
				}
				res = Encoding.ASCII.GetString(tmp);
			}

			return res;
		}

		public static string scEncrypt(string str, ushort key)
		{
			string res = "";

			if (!string.IsNullOrEmpty(str))
			{
				uint seed = key;
				byte[] idata = Encoding.ASCII.GetBytes(str);
				for (int i = 0; i < idata.Length; i++)
				{
					idata[i] = (byte)((uint)idata[i] ^ seed >> 8);
					seed = unchecked((ushort)(((uint)idata[i] + seed) * 28732u + 28446u));
				}

				byte[] res_data = null;

				int idx = 0;
				while (idx < idata.Length)
				{
					byte[] sd = ArrCopy(idata, idx, 3);
					res_data = ArrConcat(res_data, Encode(sd));
					idx += sd.Length;
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
