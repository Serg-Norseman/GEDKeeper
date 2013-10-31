using System;

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

		// quality checked, reduced memory useage
		public static int LevenshteinDistance(string source, string target)
		{
			// Return trivial case - where they are equal
			if (source.Equals(target)) return 0;

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

			// Return trivial case - where string1 is contained within string2
			if (target.Contains(source)) return target.Length - source.Length;

			int m = target.Length;
			int n = source.Length;

			int[,] dist = new int[2, m + 1];
			for (int j = 1; j <= m; j++) dist[0, j] = j;

			int cur_row = 0;
			int prev_row, cost;
			for (int i = 1; i <= n; ++i) {
				cur_row = i & 1;
				dist[cur_row, 0] = i;
				prev_row = cur_row ^ 1;

				for (int j = 1; j <= m; j++) {
					cost = (target[j - 1] == source[i - 1] ? 0 : 1);
					dist[cur_row, j] = Minimum(dist[prev_row, j] + 1, dist[cur_row, j - 1] + 1, dist[prev_row, j - 1] + cost);
				}
			}

			return dist[cur_row, m];
		}


		public static int DamerauLevenshteinDistance(string string1, string string2)
		{
			// Return trivial case - where they are equal
			if (string1.Equals(string2))
				return 0;

			// Return trivial case - where one is empty
			if (String.IsNullOrEmpty(string1) || String.IsNullOrEmpty(string2))
				return (string1 ?? "").Length + (string2 ?? "").Length;


			// Ensure string2 (inner cycle) is longer
			if (string1.Length > string2.Length)
			{
				var tmp = string1;
				string1 = string2;
				string2 = tmp;
			}

			// Return trivial case - where string1 is contained within string2
			if (string2.Contains(string1))
				return string2.Length - string1.Length;

			var length1 = string1.Length;
			var length2 = string2.Length;

			var d = new int[length1 + 1, length2 + 1];

			for (var i = 0; i <= d.GetUpperBound(0); i++)
				d[i, 0] = i;

			for (var i = 0; i <= d.GetUpperBound(1); i++)
				d[0, i] = i;

			for (var i = 1; i <= d.GetUpperBound(0); i++)
			{
				for (var j = 1; j <= d.GetUpperBound(1); j++)
				{
					var cost = string1[i - 1] == string2[j - 1] ? 0 : 1;

					var del = d[i - 1, j] + 1;
					var ins = d[i, j - 1] + 1;
					var sub = d[i - 1, j - 1] + cost;

					d[i, j] = Math.Min(del, Math.Min(ins, sub));

					if (i > 1 && j > 1 && string1[i - 1] == string2[j - 2] && string1[i - 2] == string2[j - 1])
						d[i, j] = Math.Min(d[i, j], d[i - 2, j - 2] + cost);
				}
			}

			return d[d.GetUpperBound(0), d.GetUpperBound(1)];
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
		
	}
}
