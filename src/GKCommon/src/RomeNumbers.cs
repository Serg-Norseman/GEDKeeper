namespace GKCommon
{
	public static class RomeNumbers
	{
		private static readonly int[] Rn_N;
		private static readonly string[] Rn_S;

		static RomeNumbers()
		{
			Rn_N = new int[]
			{
				1, 
				4, 
				5, 
				9, 
				10, 
				40, 
				50, 
				90, 
				100, 
				400, 
				500, 
				900, 
				1000
			};

			Rn_S = new string[]
			{
				"I", 
				"IV", 
				"V", 
				"IX", 
				"X", 
				"XL", 
				"L", 
				"XC", 
				"C", 
				"CD", 
				"D", 
				"CM", 
				"M"
			};
		}

		public static string GetRome(int N)
		{
			string S = "";
			byte T = 13;
			if (N > 0)
			{
				while (true)
				{
					if (N >= RomeNumbers.Rn_N[(int)T - 1])
					{
						while (N >= RomeNumbers.Rn_N[(int)T - 1])
						{
							N -= RomeNumbers.Rn_N[(int)T - 1];
							S += RomeNumbers.Rn_S[(int)T - 1];
						}
						if (N <= 0)
						{
							break;
						}
					}
					else
					{
						T -= 1;
					}
				}
			}
			return S;
		}

	}
}
