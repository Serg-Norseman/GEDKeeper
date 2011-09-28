using System;
using System.Collections;
using System.Runtime.InteropServices;

namespace GKCore.Sys
{
	public class EListError : Exception
	{
		public EListError()
		{
		}
		public EListError(string message) : base(message)
		{
		}
		public EListError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public class TListComparer : IComparer
	{
		private TListSortCompare FCompare;

		int IComparer.Compare(object O1, object O2)
		{
			return this.FCompare(O1, O2);
		}

		public TListComparer(TListSortCompare Compare)
		{
			this.FCompare = Compare;
		}
	}


	public class TListEnumerator
	{
		private int FIndex;
		private TList FList;

		public object Current
		{
			get { return this.GetCurrent(); }
		}

		public TListEnumerator(TList AList)
		{
			this.FIndex = -1;
			this.FList = AList;
		}

		public object GetCurrent()
		{
			return this.FList[this.FIndex];
		}

		public bool MoveNext()
		{
			bool Result = this.FIndex < this.FList.Count - 1;
			if (Result)
			{
				this.FIndex++;
			}
			return Result;
		}
	}


	public delegate int TListSortCompare(object Item1, object Item2);

	public enum TListNotification : byte
	{
		lnAdded,
		lnExtracted,
		lnDeleted
	}


	public class TList
	{
		private ArrayList FList;

		public int Capacity
		{
			get { return this.FList.Capacity; }
			set { this.SetCapacity(value); }
		}

		public int Count
		{
			get { return this.FList.Count; }
			set { this.SetCount(value); }
		}

		public object this[int Index]
		{
			get { return this.Get(Index); }
			set { this.Put(Index, value); }
		}

		protected object Get(int Index)
		{
			return this.FList[Index];
		}

		protected virtual void Grow()
		{
			int LCapacity = this.FList.Capacity;
			int Delta;
			if (LCapacity > 64)
			{
				Delta = LCapacity / 4;
			}
			else
			{
				if (LCapacity > 8)
				{
					Delta = 16;
				}
				else
				{
					Delta = 4;
				}
			}
			this.SetCapacity(LCapacity + Delta);
		}

		protected void Put(int Index, object Item)
		{
			if (Index < 0 || Index >= this.Count)
			{
				TList.Error("List index out of bounds (%d)", Index);
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

		protected virtual void Notify(object Instance, TListNotification Action)
		{
		}

		protected void SetCapacity(int NewCapacity)
		{
			if (NewCapacity < this.Count)
			{
				TList.Error("List capacity out of bounds (%d)", NewCapacity);
			}
			this.FList.Capacity = NewCapacity;
		}

		protected void SetCount(int NewCount)
		{
			if (NewCount < 0)
			{
				TList.Error("List count out of bounds (%d)", NewCount);
			}
			int C = this.FList.Count;
			if (NewCount > C)
			{
				object[] TempArray = null;
				object[] arg_30_0 = TempArray;
				int num = NewCount - C;
				object[] array = arg_30_0;
				int arg_3A_0;
				if ((arg_3A_0 = num) < 0)
				{
					arg_3A_0 = 0;
				}
				object[] array2;
				object[] expr_3F = array2 = new object[arg_3A_0];
				if (num > 0 && array != null)
				{
					int num2;
					if ((num2 = array.Length) > num)
					{
						num2 = num;
					}
					if (num2 > 0)
					{
						Array.Copy(array, array2, num2);
					}
				}
				TempArray = expr_3F;
				this.FList.AddRange((ICollection)TempArray);
			}
			else
			{
				object[] TempArray = null;
				object[] arg_86_0 = TempArray;
				int num3 = C - NewCount;
				object[] array3 = arg_86_0;
				int arg_90_0;
				if ((arg_90_0 = num3) < 0)
				{
					arg_90_0 = 0;
				}
				object[] array4;
				object[] expr_95 = array4 = new object[arg_90_0];
				if (num3 > 0 && array3 != null)
				{
					int num4;
					if ((num4 = array3.Length) > num3)
					{
						num4 = num3;
					}
					if (num4 > 0)
					{
						Array.Copy(array3, array4, num4);
					}
				}
				TempArray = expr_95;
				this.FList.CopyTo(NewCount, TempArray, 0, C - NewCount);
				this.FList.RemoveRange(NewCount, C - NewCount);
				int arg_ED_0 = 0;
				int num5 = ((TempArray != null) ? TempArray.Length : 0) - 1;
				int I = arg_ED_0;
				if (num5 >= I)
				{
					num5++;
					do
					{
						this.Notify(TempArray[I], TListNotification.lnDeleted);
						I++;
					}
					while (I != num5);
				}
			}
		}

		public TList()
		{
			this.FList = new ArrayList();
		}

		public int Add(object Item)
		{
			int Result = this.FList.Add(Item);
			if (Item != null)
			{
				this.Notify(Item, TListNotification.lnAdded);
			}
			return Result;
		}

		public virtual void Clear()
		{
			this.SetCount(0);
			this.SetCapacity(0);
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

		public static void Error([In] string Msg, int Data)
		{
			throw new EListError(string.Format(Msg, new object[] { Data }));
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

		public object First()
		{
			return this.Get(0);
		}

		public TListEnumerator GetEnumerator()
		{
			return new TListEnumerator(this);
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

		public object Last()
		{
			return this.Get(this.Count - 1);
		}

		public void Move(int CurIndex, int NewIndex)
		{
			if (CurIndex != NewIndex)
			{
				if (NewIndex < 0 || NewIndex >= this.Count)
				{
					TList.Error("List index out of bounds (%d)", NewIndex);
				}
				object Item = this.Get(CurIndex);
				this.FList.RemoveAt(CurIndex);
				this.FList.Insert(NewIndex, Item);
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

		public void Sort(TListSortCompare Compare)
		{
			this.FList.Sort(new TListComparer(Compare));
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
