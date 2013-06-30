using System;
using System.Collections.Generic;
using System.Text;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
{
    [Serializable]
	public class EStringListError : Exception
	{
		public EStringListError()
		{
		}
		public EStringListError(string message) : base(message)
		{
		}
	}

	public delegate void TNotifyEvent(object sender);

	public class StringList
	{
		private struct StringItem
		{
			public string FString;
			public object FObject;
			
			public StringItem(string S, object Obj)
			{
				this.FString = S;
				this.FObject = Obj;
			}
		}

		public enum TDuplicates : byte
		{
			dupIgnore,
			dupAccept,
			dupError
		}

		private bool FCaseSensitive;
		private TDuplicates FDuplicates;
		private List<StringItem> FList = new List<StringItem>();
		private TNotifyEvent FOnChange;
		private TNotifyEvent FOnChanging;
		private bool FSorted;
		private int FUpdateCount;

		private static string LineBreak = "\r\n";

		public int Count
		{
			get { return this.FList.Count; }
		}

		public string this[int Index]
		{
			get {
				if (Index < 0 || Index >= this.FList.Count)
				{
					this.Error("List index out of bounds (%d)", Index);
				}

				return this.FList[Index].FString;
			}
			set {
				if (this.Sorted)
				{
					this.Error("Operation not allowed on sorted list", 0);
				}
				if (Index < 0 || Index >= this.FList.Count)
				{
					this.Error("List index out of bounds (%d)", Index);
				}

				this.Changing();
				StringItem item = this.FList[Index];
				item.FString = value;
				this.FList[Index] = item;
				this.Changed();
			}
		}

		public string Text
		{
			get {
				return this.GetTextStr();
			}
			set {
				this.SetTextStr(value);
			}
		}

		public StringList()
		{
		}

		public StringList(string str)
		{
			SetTextStr(str);
		}

		public void Free()
		{
			SysUtils.Free(this);
		}

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

		public TDuplicates Duplicates
		{
			get { return this.FDuplicates; }
			set { this.FDuplicates = value; }
		}

		public bool Sorted
		{
			get {
				return this.FSorted;
			}
			set {
				if (this.FSorted != value) {
					if (value) this.Sort();
					this.FSorted = value;
				}
			}
		}

		public bool CaseSensitive
		{
			get {
				return this.FCaseSensitive;
			}
			set {
				if (value != this.FCaseSensitive) {
					this.FCaseSensitive = value;
					if (this.FSorted) this.Sort();
				}
			}
		}

		private void Error(string Msg, int Data)
		{
			throw new EStringListError(string.Format(Msg, Data));
		}

		public object GetObject(int index)
		{
			if (index < 0 || index >= this.FList.Count)
			{
				this.Error("List index out of bounds (%d)", index);
			}
			return this.FList[index].FObject;
		}

		public void SetObject(int Index, object AObject)
		{
			if (Index < 0 || Index >= this.FList.Count)
			{
				this.Error("List index out of bounds (%d)", Index);
			}
			this.Changing();
			StringItem item = this.FList[Index];
			item.FObject = AObject;
			this.FList[Index] = item;
			this.Changed();
		}

		private string GetTextStr()
		{
			int Count = this.FList.Count;
			StringBuilder Buffer = new StringBuilder();

			int num = Count - 1;
			for (int I = 0; I <= num; I++)
			{
				Buffer.Append(this[I]);
				Buffer.Append(StringList.LineBreak);
			}

			return Buffer.ToString();
		}

		// FIXME: тщательно многократно проверить на разных данных
		private void SetTextStr(string Value)
		{
			this.BeginUpdate();
			try
			{
				this.Clear();

				int Start = 0;
				int L = StringList.LineBreak.Length;
				int P = Value.IndexOf(StringList.LineBreak);
				if (P >= 0)
				{
					do
					{
						string s = Value.Substring(Start, P - Start);
						this.Add(s);
						Start = P + L;
						P = Value.IndexOf(StringList.LineBreak, Start);
					}
					while (P >= 0);
				}

				if (Start <= Value.Length)
				{
					string s = Value.Substring(Start, (Value.Length - Start));
					this.Add(s);
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public int Add(string S)
		{
			return this.AddObject(S, null);
		}

		public int AddObject(string S, object AObject)
		{
			int Result = -1;
			if (!this.Sorted)
			{
				Result = this.FList.Count;
			}
			else
			{
				if (this.Find(S, ref Result))
				{
					StringList.TDuplicates duplicates = this.Duplicates;
					if (duplicates == StringList.TDuplicates.dupIgnore)
					{
						return Result;
					}
					if (duplicates == StringList.TDuplicates.dupError)
					{
						this.Error("String list does not allow duplicates", 0);
					}
				}
			}
			this.InsertItem(Result, S, AObject);
			return Result;
		}

		public void AddStrings(StringList strings)
		{
			this.BeginUpdate();
			try
			{
				int num = strings.Count - 1;
				for (int I = 0; I <= num; I++)
				{
					this.AddObject(strings[I], strings.GetObject(I));
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public void Assign(StringList Source)
		{
			if (Source is StringList)
			{
				this.BeginUpdate();
				try
				{
					this.Clear();
					this.AddStrings(Source);
				}
				finally
				{
					this.EndUpdate();
				}
			}
		}

		public void Clear()
		{
			if (this.FList.Count != 0)
			{
				this.Changing();
				this.FList.Clear();
				this.Changed();
			}
		}

		public void Delete(int Index)
		{
			if (Index < 0 || Index >= this.FList.Count)
			{
				this.Error("List index out of bounds (%d)", Index);
			}
			this.Changing();
			if (Index < this.FList.Count)
			{
				this.FList.RemoveAt(Index);
			}
			this.Changed();
		}

		private void SetUpdateState(bool Updating)
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

		public void BeginUpdate()
		{
			if (this.FUpdateCount == 0)
			{
				this.SetUpdateState(true);
			}
			this.FUpdateCount++;
		}

		public void EndUpdate()
		{
			this.FUpdateCount--;
			if (this.FUpdateCount == 0)
			{
				this.SetUpdateState(false);
			}
		}

		public void Exchange(int Index1, int Index2)
		{
			if (Index1 < 0 || Index1 >= this.FList.Count)
			{
				this.Error("List index out of bounds (%d)", Index1);
			}
			if (Index2 < 0 || Index2 >= this.FList.Count)
			{
				this.Error("List index out of bounds (%d)", Index2);
			}
			this.Changing();
			this.ExchangeItems(Index1, Index2);
			this.Changed();
		}

		public int xIndexOf(string S)
		{
			int num = this.FList.Count - 1;
			for (int res = 0; res <= num; res++)
			{
				if (this.CompareStrings(this.FList[res].FString, S) == 0)
				{
					return res;
				}
			}

			return -1;
		}

		public int IndexOf(string S)
		{
			int Result = -1;
			if (!this.Sorted)
			{
				Result = xIndexOf(S);
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

		public int IndexOfObject(object AObject)
		{
			int res;

			if (AObject == null)
			{
				int num = this.FList.Count - 1;
				res = 0;
				if (num >= res)
				{
					num++;
					while (this.FList[res].FObject != null)
					{
						res++;
						if (res == num)
						{
							goto IL_53;
						}
					}
					return res;
				}
			}
			else
			{
				int num2 = this.FList.Count - 1;
				res = 0;
				if (num2 >= res)
				{
					num2++;
					while (!AObject.Equals(this.FList[res].FObject))
					{
						res++;
						if (res == num2)
						{
							goto IL_53;
						}
					}
					return res;
				}
			}

			IL_53:
			res = -1;
			return res;
		}

		public void Insert(int Index, string S)
		{
			this.InsertObject(Index, S, null);
		}

		public void InsertObject(int Index, string S, object AObject)
		{
			if (this.Sorted)
			{
				this.Error("Operation not allowed on sorted list", 0);
			}
			if (Index < 0 || Index > this.Count)
			{
				this.Error("List index out of bounds (%d)", Index);
			}
			this.InsertItem(Index, S, AObject);
		}

		private void InsertItem(int Index, string S, object AObject)
		{
			this.Changing();
			this.FList.Insert(Index, new StringItem(S, AObject));
			this.Changed();
		}

		public void ExchangeItems(int Index1, int Index2)
		{
			StringItem temp = this.FList[Index1];
			this.FList[Index1] = this.FList[Index2];
			this.FList[Index2] = temp;
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

		private void Changed()
		{
			if (this.FUpdateCount == 0 && this.FOnChange != null)
			{
				this.FOnChange(this);
			}
		}

		private void Changing()
		{
			if (this.FUpdateCount == 0 && this.FOnChanging != null)
			{
				this.FOnChanging(this);
			}
		}

		public bool Find(string S, ref int Index)
		{
			bool Result = false;
			int L = 0;
			int H = this.FList.Count - 1;
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
							if (this.Duplicates != StringList.TDuplicates.dupAccept)
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

		public int CompareStrings(string S1, string S2)
		{
			return string.Compare(S1, S2, !this.FCaseSensitive);
		}

		public int SCompare(int Index1, int Index2)
		{
			return string.Compare(FList[Index1].FString, FList[Index2].FString, !this.FCaseSensitive);
		}

		public void Sort()
		{
			if (!this.FSorted && this.FList.Count > 1)
			{
				this.Changing();
				this.QuickSort(0, this.FList.Count - 1);
				this.Changed();
			}
		}

		public object[] ToArray()
		{
			object[] result = new object[this.Count];
			for (int i = 0; i <= this.Count - 1; i++) {
				result[i] = this[i];
			}
			return result;
		}

	}
}
