using System;
using System.Collections;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
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

	internal class TListComparer : IComparer
	{
		internal TListSortCompare FCompare;

		int IComparer.Compare(object O1, object O2)
		{
			return this.FCompare(O1, O2);
		}

		public TListComparer(TListSortCompare Compare)
		{
			this.FCompare = Compare;
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}


	public class TListEnumerator
	{
		internal int FIndex;
		internal TList FList;

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

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}


	public delegate int TListSortCompare(object Item1, object Item2);


	public enum TListNotification : byte
	{
		lnAdded,
		lnExtracted,
		lnDeleted
	}


	public enum TListAssignOp : byte
	{
		laCopy,
		laAnd,
		laOr,
		laXor,
		laSrcUnique,
		laDestUnique
	}


	public class TList
	{
		internal ArrayList FList;

		public int Capacity
		{
			get { return this.GetCapacity(); }
			set { this.SetCapacity(value); }
		}

		public int Count
		{
			get { return this.GetCount(); }
			set { this.SetCount(value); }
		}

		public object this[int Index]
		{
			get { return this.Get(Index); }
			set { this.Put(Index, value); }
		}

		public ArrayList List
		{
			get { return this.FList; }
		}

		protected internal object Get(int Index)
		{
			return this.FList[Index];
		}

		protected internal int GetCount()
		{
			return this.FList.Count;
		}

		protected internal int GetCapacity()
		{
			return this.FList.Capacity;
		}

		protected internal virtual void Grow()
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

		protected internal void Put(int Index, object Item)
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

		protected internal virtual void Notify(object Instance, TListNotification Action)
		{
		}

		protected internal void SetCapacity(int NewCapacity)
		{
			if (NewCapacity < this.Count)
			{
				TList.Error("List capacity out of bounds (%d)", NewCapacity);
			}
			this.FList.Capacity = NewCapacity;
		}

		protected internal void SetCount(int NewCount)
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
			throw new EListError(string.Format(Msg, new object[]
			{
				Data
			}));
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
			int I = this.Count - 1;
			if (I >= 0)
			{
				do
				{
					if (this[I] == null)
					{
						this.Delete(I);
					}
					I--;
				}
				while (I != -1);
			}
		}
		public void Sort(TListSortCompare Compare)
		{
			this.FList.Sort(new TListComparer(Compare));
		}
		public void Assign(TList ListA, TListAssignOp AOperator, TList ListB)
		{
			TList LSource;
			if (ListB != null)
			{
				LSource = ListB;
				this.Assign(ListA, TListAssignOp.laCopy, null);
			}
			else
			{
				LSource = ListA;
			}
			switch (AOperator)
			{
				case TListAssignOp.laCopy:
				{
					this.Clear();
					this.Capacity = LSource.Capacity;
					int arg_51_0 = 0;
					int num = LSource.Count - 1;
					int I = arg_51_0;
					if (num >= I)
					{
						num++;
						do
						{
							this.Add(LSource[I]);
							I++;
						}
						while (I != num);
					}
					break;
				}
				case TListAssignOp.laAnd:
				{
					int I = this.Count - 1;
					if (I >= 0)
					{
						do
						{
							if (LSource.IndexOf(this[I]) == -1)
							{
								this.Delete(I);
							}
							I--;
						}
						while (I != -1);
					}
					break;
				}
				case TListAssignOp.laOr:
				{
					int arg_B7_0 = 0;
					int num2 = LSource.Count - 1;
					int I = arg_B7_0;
					if (num2 >= I)
					{
						num2++;
						do
						{
							if (this.IndexOf(LSource[I]) == -1)
							{
								this.Add(LSource[I]);
							}
							I++;
						}
						while (I != num2);
					}
					break;
				}
				case TListAssignOp.laXor:
				{
					TList LTemp = new TList();
					LTemp.Capacity = LSource.Count;
					int arg_10F_0 = 0;
					int num3 = LSource.Count - 1;
					int I = arg_10F_0;
					if (num3 >= I)
					{
						num3++;
						do
						{
							if (this.IndexOf(LSource[I]) == -1)
							{
								LTemp.Add(LSource[I]);
							}
							I++;
						}
						while (I != num3);
					}
					I = this.Count - 1;
					if (I >= 0)
					{
						do
						{
							if (LSource.IndexOf(this[I]) != -1)
							{
								this.Delete(I);
							}
							I--;
						}
						while (I != -1);
					}
					I = this.Count + LTemp.Count;
					if (this.Capacity < I)
					{
						this.Capacity = I;
					}
					int arg_197_0 = 0;
					int num4 = LTemp.Count - 1;
					I = arg_197_0;
					if (num4 >= I)
					{
						num4++;
						do
						{
							this.Add(LTemp[I]);
							I++;
						}
						while (I != num4);
					}
					break;
				}
				case TListAssignOp.laSrcUnique:
				{
					int I = this.Count - 1;
					if (I >= 0)
					{
						do
						{
							if (LSource.IndexOf(this[I]) != -1)
							{
								this.Delete(I);
							}
							I--;
						}
						while (I != -1);
					}
					break;
				}
				case TListAssignOp.laDestUnique:
				{
					TList LTemp = new TList();
					LTemp.Capacity = LSource.Count;
					int I = LSource.Count - 1;
					if (I >= 0)
					{
						do
						{
							if (this.IndexOf(LSource[I]) == -1)
							{
								LTemp.Add(LSource[I]);
							}
							I--;
						}
						while (I != -1);
					}
					this.Assign(LTemp, TListAssignOp.laCopy, null);
					break;
				}
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
