using System;
using System.Collections.Generic;

/// <summary>
/// Localization: clean
/// </summary>

namespace ExtUtils
{
    [Serializable]
	public class ListException : Exception
	{
		public ListException()
		{
		}
		public ListException(string message) : base(message)
		{
		}
	}

	public delegate int TListSortCompare(object Item1, object Item2);

	public enum ListNotification
	{
		lnAdded,
		lnExtracted,
		lnDeleted
	}

    public class ExtList<T> : BaseObject
	{
		private readonly List<T> fList;
		private bool fOwnsObjects;

		public IList<T> List
		{
			get { return this.fList; }
		}

		public int Count
		{
			get { return this.fList.Count; }
		}

		public bool OwnsObjects
		{
			get { return this.fOwnsObjects; }
			set { this.fOwnsObjects = value; }
		}

		public T this[int index]
		{
			get {
				return this.fList[index];
			}
			set {
				if (index < 0 || index >= this.Count)
				{
					ExtList<T>.Error("List index out of bounds ({0})", index);
				}

				if (!value.Equals(this.fList[index]))
				{
					T temp = this.fList[index];
					this.fList[index] = value;

					if (temp != null)
					{
						this.Notify(temp, ListNotification.lnDeleted);
					}

					if (value != null)
					{
						this.Notify(value, ListNotification.lnAdded);
					}
				}
			}
		}

		public ExtList()
		{
			this.fList = new List<T>();
			this.fOwnsObjects = false;
		}

		public ExtList(bool ownsObjects)
		{
			this.fList = new List<T>();
			this.fOwnsObjects = ownsObjects;
		}

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //if (this.fList != null) this.fList = null;
                this.Clear();
            }
            base.Dispose(disposing);
        }

		private void Notify(object instance, ListNotification action)
		{
			if (this.fOwnsObjects && action == ListNotification.lnDeleted)
			{
				SysUtils.Free(instance);
			}
		}

		public int Add(T item)
		{
			int result = this.fList.Count;
			this.fList.Add(item);
			if (item != null)
			{
				this.Notify(item, ListNotification.lnAdded);
			}
			return result;
		}

		public void Clear()
		{
			for (int i = this.fList.Count - 1; i >= 0; i--) this.Notify(fList[i], ListNotification.lnDeleted);
			this.fList.Clear();
		}

		public void Delete(int index)
		{
			object temp = this.fList[index];

			this.fList.RemoveAt(index);

			if (temp != null)
			{
				this.Notify(temp, ListNotification.lnDeleted);
			}
		}

		public static void Error(string msg, int data)
		{
			throw new ListException(string.Format(msg, data));
		}

		public void Exchange(int index1, int index2)
		{
			T item = this.fList[index1];
			this.fList[index1] = this.fList[index2];
			this.fList[index2] = item;
		}

		public object Extract(T item)
		{
			object result = null;
			int I = this.IndexOf(item);
			if (I >= 0)
			{
				result = item;
				this.fList.RemoveAt(I);
				this.Notify(result, ListNotification.lnExtracted);
			}
			return result;
		}

		public int IndexOf(T item)
		{
			return this.fList.IndexOf(item);
		}

		public void Insert(int index, T item)
		{
			this.fList.Insert(index, item);
			if (item != null)
			{
				this.Notify(item, ListNotification.lnAdded);
			}
		}

		public int Remove(T item)
		{
			int result = this.IndexOf(item);
			if (result >= 0)
			{
				this.Delete(result);
			}
			return result;
		}

		public void Pack()
		{
			for (int I = this.Count - 1; I >= 0; I--)
			{
				if (this[I] == null)
					this.Delete(I);
			}
		}

        private void QuickSort(TListSortCompare comparer, int L, int R)
		{
			int I;
			do
			{
				I = L;
				int J = R;
				object P = fList[(int)((uint)(L + R) >> 1)];
				while (true)
				{
                    if (comparer(fList[I], P) >= 0)
					{
                        while (comparer(fList[J], P) > 0) J--;

						if (I <= J)
						{
							T tmp = fList[I];
							fList[I] = fList[J];
							fList[J] = tmp;

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
                if (L < J) QuickSort(comparer, L, J);
				L = I;
			}
			while (I < R);
		}

        public void QuickSort(TListSortCompare comparer)
		{
            if (this.Count > 0) {
                QuickSort(comparer, 0, this.Count - 1);
            }
		}

		public void MergeSort(TListSortCompare comparer)
		{
			MergeSort(new T[fList.Count], 0, fList.Count - 1, comparer);
		}

		private void MergeSort(T[] tmp, int left, int right, TListSortCompare comparer)
		{
			if (left >= right) return;

			int mid = (left + right) / 2;
			MergeSort(tmp, left, mid, comparer);
			MergeSort(tmp, mid + 1, right, comparer);

			int i = left, j = mid + 1, k = left;

			while (i <= mid && j <= right)
			{
				if (comparer(fList[i], fList[j]) < 0)
				{
					tmp[k++] = fList[i++];
				}
				else
				{
					tmp[k++] = fList[j++];
				}
			}
			while (i <= mid) tmp[k++] = fList[i++];
			while (j <= right) tmp[k++] = fList[j++];
			for (i = left; i <= right; ++i) fList[i] = tmp[i];
		}

	}
}
