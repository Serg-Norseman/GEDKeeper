using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore.Sys
{
	public class EStringListError : Exception
	{
		public EStringListError()
		{
		}
		public EStringListError(string message) : base(message)
		{
		}
		public EStringListError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public class TStringList : TStrings
	{
		private struct TStringItem
		{
			public string FString;
			public object FObject;
		}

		public enum TDuplicates : byte
		{
			dupIgnore,
			dupAccept,
			dupError
		}

		private TStringList.TStringItem[] FList;
		private int FCount;
		private bool FSorted;
		private TStringList.TDuplicates FDuplicates;
		private bool FCaseSensitive;
		private TNotifyEvent FOnChange;
		private TNotifyEvent FOnChanging;

		public event TNotifyEvent OnChange
		{
			add { this.FOnChange = value; }
			remove { if (this.FOnChange == value) this.FOnChange = null; }
		}

		public event TNotifyEvent OnChanging
		{
			add { this.FOnChanging = value; }
			remove { if (this.FOnChanging == value) this.FOnChanging = null; }
		}

		public TStringList.TDuplicates Duplicates
		{
			get { return this.FDuplicates; }
			set { this.FDuplicates = value; }
		}

		public bool Sorted
		{
			get { return this.FSorted; }
			set { this.SetSorted(value); }
		}

		public bool CaseSensitive
		{
			get { return this.FCaseSensitive; }
			set { this.SetCaseSensitive(value); }
		}

		public void ExchangeItems(int Index1, int Index2)
		{
			TStringList.TStringItem Temp = this.FList[Index1];
			this.FList[Index1] = this.FList[Index2];
			this.FList[Index2] = Temp;
		}

		private void Grow()
		{
			TStringList.TStringItem[] fList = this.FList;
			int C = (fList != null) ? fList.Length : 0;
			int Delta;
			if (C > 64)
			{
				Delta = C / 4;
			}
			else
			{
				if (C > 8)
				{
					Delta = 16;
				}
				else
				{
					Delta = 4;
				}
			}
			this.SetCapacity(C + Delta);
		}

		private void QuickSort(int L, int R)
		{
			int I;
			do
			{
				I = L;
				int J = R;
				int P = (int)((uint)(L + R) >> 1);
				while (true)
				{
					if (SCompare(I, P) >= 0)
					{
						while (SCompare(J, P) > 0)
						{
							J--;
						}
						if (I <= J)
						{
							this.ExchangeItems(I, J);
							if (P == I)
							{
								P = J;
							}
							else
							{
								if (P == J)
								{
									P = I;
								}
							}
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
				if (L < J)
				{
					this.QuickSort(L, J);
				}
				L = I;
			}
			while (I < R);
		}

		private void SetSorted(bool Value)
		{
			if (this.FSorted != Value)
			{
				if (Value) this.Sort();
				this.FSorted = Value;
			}
		}

		private void SetCaseSensitive([In] bool Value)
		{
			if (Value != this.FCaseSensitive)
			{
				this.FCaseSensitive = Value;
				if (this.Sorted) this.Sort();
			}
		}

		protected virtual void Changed()
		{
			if (this.FUpdateCount == 0 && this.FOnChange != null)
			{
				this.FOnChange(this);
			}
		}

		protected virtual void Changing()
		{
			if (this.FUpdateCount == 0 && this.FOnChanging != null)
			{
				this.FOnChanging(this);
			}
		}

		protected override string Get(int Index)
		{
			if (Index < 0 || Index >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", Index);
			}
			return this.FList[Index].FString;
		}

		protected override int GetCapacity()
		{
			TStringList.TStringItem[] fList = this.FList;
			return (fList != null) ? fList.Length : 0;
		}

		protected override int GetCount()
		{
			return this.FCount;
		}

		public override object GetObject(int index)
		{
			if (index < 0 || index >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", index);
			}
			return this.FList[index].FObject;
		}

		protected override void Put(int Index, [In] string S)
		{
			if (this.Sorted)
			{
				base.Error("Operation not allowed on sorted list", 0);
			}
			if (Index < 0 || Index >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", Index);
			}
			this.Changing();
			this.FList[Index].FString = S;
			this.Changed();
		}

		public override void PutObject(int Index, object AObject)
		{
			if (Index < 0 || Index >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", Index);
			}
			this.Changing();
			this.FList[Index].FObject = AObject;
			this.Changed();
		}

		protected override void SetCapacity(int NewCapacity)
		{
			TStringList.TStringItem[] fList = this.FList;
			int arg_0F_0 = NewCapacity;
			if (NewCapacity < 0)
			{
				arg_0F_0 = 0;
			}
			TStringList.TStringItem[] array;
			TStringList.TStringItem[] expr_14 = array = new TStringList.TStringItem[arg_0F_0];
			if (NewCapacity > 0 && fList != null)
			{
				int num;
				if ((num = fList.Length) > NewCapacity)
				{
					num = NewCapacity;
				}
				if (num > 0)
				{
					Array.Copy(fList, array, num);
				}
			}
			this.FList = expr_14;
		}

		protected override void SetUpdateState(bool Updating)
		{
			if (Updating)
			{
				this.Changing();
			}
			else
			{
				this.Changed();
			}
		}

		protected virtual void InsertItem(int Index, [In] string S, object AObject)
		{
			this.Changing();
			int arg_1C_0 = this.FCount;
			TStringList.TStringItem[] fList = this.FList;
			if (arg_1C_0 == ((fList != null) ? fList.Length : 0))
			{
				this.Grow();
			}
			if (Index < this.FCount)
			{
				Array.Copy(this.FList, Index, this.FList, Index + 1, this.FCount - Index);
			}
			TStringList.TStringItem[] var_0_cp_0 = this.FList;
			var_0_cp_0[Index].FObject = AObject;
			var_0_cp_0[Index].FString = S;
			this.FCount++;
			this.Changed();
		}

		public override int Add([In] string S)
		{
			return this.AddObject(S, null);
		}

		public override int AddObject([In] string S, object AObject)
		{
			int Result = -1;
			if (!this.Sorted)
			{
				Result = this.FCount;
			}
			else
			{
				if (this.Find(S, ref Result))
				{
					TStringList.TDuplicates duplicates = this.Duplicates;
					if (duplicates == TStringList.TDuplicates.dupIgnore)
					{
						return Result;
					}
					if (duplicates == TStringList.TDuplicates.dupError)
					{
						base.Error("String list does not allow duplicates", 0);
					}
				}
			}
			this.InsertItem(Result, S, AObject);
			return Result;
		}

		public override void Clear()
		{
			if (this.FCount != 0)
			{
				this.Changing();
				this.FCount = 0;
				this.SetCapacity(0);
				this.Changed();
			}
		}

		public override void Delete(int Index)
		{
			if (Index < 0 || Index >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", Index);
			}
			this.Changing();
			this.FCount--;
			if (Index < this.FCount)
			{
				Array.Copy(this.FList, Index + 1, this.FList, Index, this.FCount - Index);
			}
			this.Changed();
		}

		public override void Exchange(int Index1, int Index2)
		{
			if (Index1 < 0 || Index1 >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", Index1);
			}
			if (Index2 < 0 || Index2 >= this.FCount)
			{
				base.Error("List index out of bounds (%d)", Index2);
			}
			this.Changing();
			this.ExchangeItems(Index1, Index2);
			this.Changed();
		}

		public virtual bool Find([In] string S, ref int Index)
		{
			bool Result = false;
			int L = 0;
			int H = this.FCount - 1;
			if (L <= H)
			{
				do
				{
					int I = (int)((uint)(L + H) >> 1);
					int C = this.CompareStrings(this.FList[I].FString, S);
					if (C < 0)
					{
						L = I + 1;
					}
					else
					{
						H = I - 1;
						if (C == 0)
						{
							Result = true;
							if (this.Duplicates != TStringList.TDuplicates.dupAccept)
							{
								L = I;
							}
						}
					}
				}
				while (L <= H);
			}
			Index = L;
			return Result;
		}

		public override int IndexOf([In] string S)
		{
			int Result = -1;
			if (!this.Sorted)
			{
				Result = base.IndexOf(S);
			}
			else
			{
				if (!this.Find(S, ref Result))
				{
					Result = -1;
				}
			}
			return Result;
		}

		public override void Insert(int Index, [In] string S)
		{
			this.InsertObject(Index, S, null);
		}

		public override void InsertObject(int Index, [In] string S, object AObject)
		{
			if (this.Sorted)
			{
				base.Error("Operation not allowed on sorted list", 0);
			}
			if (Index < 0 || Index > base.Count)
			{
				base.Error("List index out of bounds (%d)", Index);
			}
			this.InsertItem(Index, S, AObject);
		}

		public override int CompareStrings([In] string S1, [In] string S2)
		{
			return string.Compare(S1, S2, !this.FCaseSensitive);
		}

		public int SCompare(int Index1, int Index2)
		{
			return string.Compare(FList[Index1].FString, FList[Index2].FString, !this.FCaseSensitive);
		}

		public virtual void Sort()
		{
			if (!this.FSorted && this.FCount > 1)
			{
				this.Changing();
				this.QuickSort(0, this.FCount - 1);
				this.Changed();
			}
		}
	}
}
