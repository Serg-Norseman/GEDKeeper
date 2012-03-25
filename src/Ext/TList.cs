using System;
using System.Collections.Generic;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
{
	public class EListError : Exception
	{
		public EListError()
		{
		}
		public EListError(string message) : base(message)
		{
		}
	}

	public delegate int TListSortCompare(object Item1, object Item2);

	public enum TListNotification : byte
	{
		lnAdded,
		lnExtracted,
		lnDeleted
	}

	public class TList : IDisposable
	{
		private List<object> FList;
		protected bool FOwnsObjects;
		private bool Disposed_;

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		public IList<object> List
		{
			get { return FList; }
		}

		public int Count
		{
			get { return this.FList.Count; }
		}

		public object this[int Index]
		{
			get { return this.Get(Index); }
			set { this.Put(Index, value); }
		}

		public bool OwnsObjects
		{
			get { return this.FOwnsObjects; }
			set { this.FOwnsObjects = value; }
		}

		protected object Get(int Index)
		{
			return this.FList[Index];
		}

		protected void Put(int Index, object Item)
		{
			if (Index < 0 || Index >= this.Count)
			{
				TList.Error("List index out of bounds (%d)", Index); // FIXME: таких включений много; исправить!
			}
			if (!object.Equals(Item, this.FList[Index]))
			{
				object Temp = this.FList[Index];
				this.FList[Index] = Item;
				if (Temp != null)
				{
					this.Notify(Temp, TListNotification.lnDeleted);
				}
				if (Item != null)
				{
					this.Notify(Item, TListNotification.lnAdded);
				}
			}
		}

		private void Notify(object Instance, TListNotification Action)
		{
			if (this.FOwnsObjects && Action == TListNotification.lnDeleted)
			{
				SysUtils.Free(Instance);
			}
		}

		public TList()
		{
			this.FList = new List<object>();
			this.FOwnsObjects = false;
		}

		public TList(bool AOwnsObjects)
		{
			this.FList = new List<object>();
			this.FOwnsObjects = AOwnsObjects;
		}

		public int Add(object Item)
		{
			int Result = this.FList.Count;
			this.FList.Add(Item);
			if (Item != null)
			{
				this.Notify(Item, TListNotification.lnAdded);
			}
			return Result;
		}

		public void Clear()
		{
			for (int i = this.FList.Count - 1; i >= 0; i--) this.Notify(FList[i], TListNotification.lnDeleted);
			this.FList.Clear();
		}

		public void Delete(int Index)
		{
			object Temp = this.FList[Index];
			this.FList.RemoveAt(Index);
			if (Temp != null)
			{
				this.Notify(Temp, TListNotification.lnDeleted);
			}
		}

		public static void Error(string Msg, int Data)
		{
			throw new EListError(string.Format(Msg, Data));
		}

		public void Exchange(int Index1, int Index2)
		{
			object Item = this.FList[Index1];
			this.FList[Index1] = this.FList[Index2];
			this.FList[Index2] = Item;
		}

		public object Extract(object Item)
		{
			object Result = null;
			int I = this.IndexOf(Item);
			if (I >= 0)
			{
				Result = Item;
				this.FList.RemoveAt(I);
				this.Notify(Result, TListNotification.lnExtracted);
			}
			return Result;
		}

		public int IndexOf(object Item)
		{
			return this.FList.IndexOf(Item);
		}

		public void Insert(int Index, object Item)
		{
			this.FList.Insert(Index, Item);
			if (Item != null)
			{
				this.Notify(Item, TListNotification.lnAdded);
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
				object P = FList[(int)((uint)(L + R) >> 1)];
				while (true)
				{
					if (SCompare(FList[I], P) >= 0)
					{
						while (SCompare(FList[J], P) > 0) J--;

						if (I <= J)
						{
							object T = FList[I];
							FList[I] = FList[J];
							FList[J] = T;

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
			MergeSort(new object[FList.Count], 0, FList.Count - 1, comparer);
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
				if (comparer(FList[i], FList[j]) < 0)
				{
					tmp[k++] = FList[i++];
				}
				else
				{
					tmp[k++] = FList[j++];
				}
			}
			while (i <= mid) tmp[k++] = FList[i++];
			while (j <= right) tmp[k++] = FList[j++];
			for (i = left; i <= right; ++i) FList[i] = tmp[i];
		}

	}
}
