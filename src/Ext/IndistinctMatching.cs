using System;
using System.Collections.Generic;

namespace Ext.Utils
{
	public class IndistinctMatching
	{
		// quality checked
		private static int Minimum(int a, int b, int c)
		{
			int min = a;
			if (b < min) min = b;
			if (c < min) min = c;
			return min;
		}

		// quality checked
		public static int LevenshteinDistance(string source, string target)
		{
			if (string.IsNullOrEmpty(source)) {
				if (string.IsNullOrEmpty(target)) return 0;
				return target.Length;
			}

			if (string.IsNullOrEmpty(target)) {
				return source.Length;
			}

			if (source.Length > target.Length) {
				string temp = target;
				target = source;
				source = temp;
			}

			int m = target.Length;
			int n = source.Length;

			int[,] dist = new int[2, m + 1];
			for (int j = 1; j <= m; j++) dist[0, j] = j;

			int cur_row = 0;
			for (int i = 1; i <= n; ++i) {
				cur_row = i & 1;
				dist[cur_row, 0] = i;
				int prev_row = cur_row ^ 1;

				for (int j = 1; j <= m; j++) {
					int cost = (target[j - 1] == source[i - 1] ? 0 : 1);

					dist[cur_row, j] = Minimum(dist[prev_row, j] + 1, dist[cur_row, j - 1] + 1, dist[prev_row, j - 1] + cost);
				}
			}

			return dist[cur_row, m];
		}

		public static int DamerauLevenshteinDistance(string source, string target)
		{
			if (string.IsNullOrEmpty(source)) {
				if (string.IsNullOrEmpty(target)) {
					return 0;
				} else {
					return target.Length;
				}
			} else if (string.IsNullOrEmpty(target)) {
				return source.Length;
			}

			int m = source.Length;
			int n = target.Length;
			int[,] H = new int[m + 2, n + 2];

			int INF = m + n;
			H[0, 0] = INF;
			for (int i = 0; i <= m; i++) { H[i + 1, 1] = i; H[i + 1, 0] = INF; }
			for (int j = 0; j <= n; j++) { H[1, j + 1] = j; H[0, j + 1] = INF; }

			Dictionary<char, int> sd = new Dictionary<char, int>();
			foreach (char Letter in (source + target))
			{
				if (!sd.ContainsKey(Letter))
					sd.Add(Letter, 0);
			}

			for (int i = 1; i <= m; i++)
			{
				int DB = 0;
				for (int j = 1; j <= n; j++)
				{
					int i1 = sd[target[j - 1]];
					int j1 = DB;

					if (source[i - 1] == target[j - 1])
					{
						H[i + 1, j + 1] = H[i, j];
						DB = j;
					}
					else
					{
						H[i + 1, j + 1] = Math.Min(H[i, j], Math.Min(H[i + 1, j], H[i, j + 1])) + 1;
					}

					H[i + 1, j + 1] = Math.Min(H[i + 1, j + 1], H[i1, j1] + (i - i1 - 1) + 1 + (j - j1 - 1));
				}

				sd[source[i - 1]] = i;
			}

			return H[m + 1, n + 1];
		}

		public static double GetSimilarity(string firstString, string secondString)
		{
			if (firstString == null)
				throw new ArgumentNullException("firstString");
			if (secondString == null)
				throw new ArgumentNullException("secondString");

			if (firstString == secondString)
				return 1;

			int longestLenght = Math.Max(firstString.Length, secondString.Length);
			int distance = DamerauLevenshteinDistance(firstString, secondString);
			double percent = distance / (double)longestLenght;
			return 1.0 - percent;
		}

		/*void Button1Click(object sender, EventArgs e)
		{
			this.button1.Enabled = false;

			for (int i = 1; i <= 10000; i++) {
				int res1 = LevenshteinDistance(this.textBox1.Text, this.textBox2.Text);
				int res2 = DamerauLevenshteinDistance(this.textBox1.Text, this.textBox2.Text);

				this.Text = res1.ToString() + " / " + res2.ToString();
			}

			this.button1.Enabled = true;
		}*/
		
	}
}
