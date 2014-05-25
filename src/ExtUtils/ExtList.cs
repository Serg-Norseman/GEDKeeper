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

    public class ExtList : BaseObject
	{
		private readonly List<object> fList;
		private bool fOwnsObjects;

		public IList<object> List
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

		public object this[int Index]
		{
			get {
				return this.fList[Index];
			}
			set {
				if (Index < 0 || Index >= this.Count)
				{
					ExtList.Error("List index out of bounds ({0})", Index);
				}

				if (value != this.fList[Index])
				{
					object temp = this.fList[Index];
					this.fList[Index] = value;

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
			this.fList = new List<object>();
			this.fOwnsObjects = false;
		}

		public ExtList(bool ownsObjects)
		{
			this.fList = new List<object>();
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

		private void Notify(object Instance, ListNotification Action)
		{
			if (this.fOwnsObjects && Action == ListNotification.lnDeleted)
			{
				SysUtils.Free(Instance);
			}
		}

		public int Add(object Item)
		{
			int Result = this.fList.Count;
			this.fList.Add(Item);
			if (Item != null)
			{
				this.Notify(Item, ListNotification.lnAdded);
			}
			return Result;
		}

		public void Clear()
		{
			for (int i = this.fList.Count - 1; i >= 0; i--) this.Notify(fList[i], ListNotification.lnDeleted);
			this.fList.Clear();
		}

		public void Delete(int Index)
		{
			object Temp = this.fList[Index];

			this.fList.RemoveAt(Index);

			if (Temp != null)
			{
				this.Notify(Temp, ListNotification.lnDeleted);
			}
		}

		public static void Error(string Msg, int Data)
		{
			throw new ListException(string.Format(Msg, Data));
		}

		public void Exchange(int Index1, int Index2)
		{
			object Item = this.fList[Index1];
			this.fList[Index1] = this.fList[Index2];
			this.fList[Index2] = Item;
		}

		public object Extract(object Item)
		{
			object Result = null;
			int I = this.IndexOf(Item);
			if (I >= 0)
			{
				Result = Item;
				this.fList.RemoveAt(I);
				this.Notify(Result, ListNotification.lnExtracted);
			}
			return Result;
		}

		public int IndexOf(object Item)
		{
			return this.fList.IndexOf(Item);
		}

		public void Insert(int Index, object Item)
		{
			this.fList.Insert(Index, Item);
			if (Item != null)
			{
				this.Notify(Item, ListNotification.lnAdded);
			}
		}

		public int Remove(object Item)
		{
			int Result = this.IndexOf(Item);
			if (Result >= 0)
			{
				this.Delete(Result);
			}
			return Result;
		}

		public void Pack()
		{
			for (int I = this.Count - 1; I >= 0; I--)
			{
				if (this[I] == null)
					this.Delete(I);
			}
		}

		private void IQuickSort(TListSortCompare SCompare, int L, int R)
		{
			int I;
			do
			{
				I = L;
				int J = R;
				object P = fList[(int)((uint)(L + R) >> 1)];
				while (true)
				{
					if (SCompare(fList[I], P) >= 0)
					{
						while (SCompare(fList[J], P) > 0) J--;

						if (I <= J)
						{
							object T = fList[I];
							fList[I] = fList[J];
							fList[J] = T;

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
				if (L < J) IQuickSort(SCompare, L, J);
				L = I;
			}
			while (I < R);
		}

		public void Sort(TListSortCompare SCompare)
		{
			if (this.Count > 0) IQuickSort(SCompare, 0, this.Count - 1);
		}

		public void MergeSort(TListSortCompare comparer)
		{
			MergeSort(new object[fList.Count], 0, fList.Count - 1, comparer);
		}

		private void MergeSort(object[] tmp, int left, int right, TListSortCompare comparer)
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
