using System;
using System.Collections.Generic;

namespace BSLib
{
	public static class SortHelper
	{
        public static void QuickSort<T>(IList<T> list, Comparison<T> comparer)
		{
            if (list.Count > 1)
            {
                QuickSort(list, comparer, 0, list.Count - 1);
            }
		}

        private static void QuickSort<T>(IList<T> list, Comparison<T> comparer, int left, int right)
		{
			int I;
			do
			{
				I = left;
				int J = right;
				T itm = list[(int)((uint)(left + right) >> 1)];
				while (true)
				{
                    if (comparer(list[I], itm) >= 0)
					{
                        while (comparer(list[J], itm) > 0) J--;

						if (I <= J)
						{
							T tmp = list[I];
							list[I] = list[J];
							list[J] = tmp;

							I++;
							J--;
						}

						if (I > J)
						{
							break;
						}
					}
					else
					{
						I++;
					}
				}
                if (left < J) QuickSort(list, comparer, left, J);
				left = I;
			}
			while (I < right);
		}

		public static void MergeSort<T>(IList<T> list, Comparison<T> comparer)
		{
            if (list.Count > 1)
            {
				MergeSort(list, new T[list.Count], 0, list.Count - 1, comparer);
            }
		}

		private static void MergeSort<T>(IList<T> list, T[] tmp, int left, int right, Comparison<T> comparer)
		{
			if (left >= right) return;

			int mid = (left + right) >> 1;
			MergeSort(list, tmp, left, mid, comparer);
			MergeSort(list, tmp, mid + 1, right, comparer);

			int i = left, j = mid + 1, k = left;

			while (i <= mid && j <= right)
			{
				if (comparer(list[i], list[j]) < 0)
				{
					tmp[k++] = list[i++];
				}
				else
				{
					tmp[k++] = list[j++];
				}
			}
			while (i <= mid) tmp[k++] = list[i++];
			while (j <= right) tmp[k++] = list[j++];
			for (i = left; i <= right; ++i) list[i] = tmp[i];
		}
	}
}
