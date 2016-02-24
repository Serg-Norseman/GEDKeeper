namespace BSLib
{
	public static class RomeNumbers
	{
		private static readonly int[] RN_N;
		private static readonly string[] RN_S;

		static RomeNumbers()
		{
			RN_N = new int[]
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

			RN_S = new string[]
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

		public static string GetRome(int num)
		{
			string rome = "";
			int T = 12;

			if (num > 0)
			{
				while (true)
				{
					int rn = RN_N[T];
					
					if (num >= rn) {
						while (num >= rn) {
							num -= rn;
							rome += RN_S[T];
						}

						if (num <= 0) break;
					} else {
						T -= 1;
					}
				}
			}
			return rome;
		}

	}
}
