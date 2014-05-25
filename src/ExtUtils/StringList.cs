using System;
using System.Collections.Generic;
using System.Text;

/// <summary>
/// Localization: clean
/// </summary>

namespace ExtUtils
{
    [Serializable]
	public class StringListException : Exception
	{
		public StringListException()
		{
		}
		public StringListException(string message) : base(message)
		{
		}
	}

	public delegate void NotifyEventHandler(object sender);

    public class StringList : BaseObject
	{
		private struct StringItem
		{
			public string FString;
			public object FObject;
			
			public StringItem(string str, object obj)
			{
				this.FString = str;
				this.FObject = obj;
			}
		}

		public enum TDuplicates
		{
			dupIgnore,
			dupAccept,
			dupError
		}

        private readonly List<StringItem> fList;
        private bool fCaseSensitive;
		private TDuplicates fDuplicates;
		private NotifyEventHandler fOnChange;
		private NotifyEventHandler fOnChanging;
		private bool fSorted;
		private int fUpdateCount;

	    private const string LineBreak = "\r\n";

	    public int Count
		{
			get { return this.fList.Count; }
		}

		public string this[int index]
		{
			get {
				if (index < 0 || index >= this.fList.Count)
				{
					RaiseError("List index out of bounds ({0})", index);
				}

				return this.fList[index].FString;
			}
			set {
				if (this.Sorted)
				{
					RaiseError("Operation not allowed on sorted list", 0);
				}
				if (index < 0 || index >= this.fList.Count)
				{
					RaiseError("List index out of bounds ({0})", index);
				}

				this.Changing();
				StringItem item = this.fList[index];
				item.FString = value;
				this.fList[index] = item;
				this.Changed();
			}
		}

		public string Text
		{
			get { return this.GetTextStr(); }
			set { this.SetTextStr(value); }
		}

		public StringList()
		{
			this.fList = new List<StringItem>();
		}

		public StringList(string str) : this()
		{
			this.SetTextStr(str);
		}

		public StringList(string[] list) : this()
		{
			for (int i = 0; i < list.Length; i++)
			{
				this.AddObject(list[i], null);
			}
		}

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //this.fList = null;
            }
            base.Dispose(disposing);
        }

		public event NotifyEventHandler OnChange
		{
			add { this.fOnChange = value; }
			remove { if (this.fOnChange == value) this.fOnChange = null; }
		}

		public event NotifyEventHandler OnChanging
		{
			add { this.fOnChanging = value; }
			remove { if (this.fOnChanging == value) this.fOnChanging = null; }
		}

		public TDuplicates Duplicates
		{
			get { return this.fDuplicates; }
			set { this.fDuplicates = value; }
		}

		public bool Sorted
		{
			get {
				return this.fSorted;
			}
			set {
				if (this.fSorted != value) {
					if (value) this.Sort();
					this.fSorted = value;
				}
			}
		}

		public bool CaseSensitive
		{
			get {
				return this.fCaseSensitive;
			}
			set {
				if (value != this.fCaseSensitive) {
					this.fCaseSensitive = value;
					if (this.fSorted) this.Sort();
				}
			}
		}

		private static void RaiseError(string msg, int data)
		{
			throw new StringListException(string.Format(msg, data));
		}

		public object GetObject(int index)
		{
			if (index < 0 || index >= this.fList.Count)
			{
				RaiseError("List index out of bounds ({0})", index);
			}
			return this.fList[index].FObject;
		}

		public void SetObject(int index, object obj)
		{
			if (index < 0 || index >= this.fList.Count)
			{
				RaiseError("List index out of bounds ({0})", index);
			}
			this.Changing();
			StringItem item = this.fList[index];
			item.FObject = obj;
			this.fList[index] = item;
			this.Changed();
		}

		private string GetTextStr()
		{
			StringBuilder buffer = new StringBuilder();

			int num = this.fList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				buffer.Append(this[i]);
				buffer.Append(StringList.LineBreak);
			}

			return buffer.ToString();
		}

		private void SetTextStr(string value)
		{
			this.BeginUpdate();
			try
			{
				this.Clear();

				int Start = 0;
				int L = StringList.LineBreak.Length;
				int P = value.IndexOf(StringList.LineBreak);
				if (P >= 0)
				{
					do
					{
						string s = value.Substring(Start, P - Start);
						this.Add(s);
						Start = P + L;
						P = value.IndexOf(StringList.LineBreak, Start);
					}
					while (P >= 0);
				}

				if (Start <= value.Length)
				{
					string s = value.Substring(Start, (value.Length - Start));
					this.Add(s);
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public int Add(string str)
		{
			return this.AddObject(str, null);
		}

		public int AddObject(string str, object obj)
		{
			int result = -1;

            if (!this.Sorted)
			{
				result = this.fList.Count;
			}
			else
			{
				if (this.Find(str, ref result))
				{
					StringList.TDuplicates duplicates = this.Duplicates;
					if (duplicates == StringList.TDuplicates.dupIgnore)
					{
						return result;
					}
					if (duplicates == StringList.TDuplicates.dupError)
					{
						RaiseError("String list does not allow duplicates", 0);
					}
				}
			}
			this.InsertItem(result, str, obj);

            return result;
		}

		public void AddStrings(StringList strList)
		{
		    if (strList == null) return;

			this.BeginUpdate();
			try
			{
				int num = strList.Count - 1;
				for (int I = 0; I <= num; I++)
				{
					this.AddObject(strList[I], strList.GetObject(I));
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public void Assign(StringList source)
		{
			if (source != null)
			{
				this.BeginUpdate();
				try
				{
					this.Clear();
					this.AddStrings(source);
				}
				finally
				{
					this.EndUpdate();
				}
			}
		}

		public void Clear()
		{
			if (this.fList.Count != 0)
			{
				this.Changing();
				this.fList.Clear();
				this.Changed();
			}
		}

		public void Delete(int index)
		{
			if (index < 0 || index >= this.fList.Count)
			{
				RaiseError("List index out of bounds ({0})", index);
			}

            this.Changing();
			if (index < this.fList.Count)
			{
				this.fList.RemoveAt(index);
			}
			this.Changed();
		}

		public void Exchange(int index1, int index2)
		{
			if (index1 < 0 || index1 >= this.fList.Count)
			{
				RaiseError("List index out of bounds ({0})", index1);
			}
			if (index2 < 0 || index2 >= this.fList.Count)
			{
				RaiseError("List index out of bounds ({0})", index2);
			}

            this.Changing();
			this.ExchangeItems(index1, index2);
			this.Changed();
		}

		public void Insert(int index, string str)
		{
			this.InsertObject(index, str, null);
		}

		public void InsertObject(int index, string str, object obj)
		{
			if (this.Sorted)
			{
				RaiseError("Operation not allowed on sorted list", 0);
			}
			if (index < 0 || index > this.Count)
			{
				RaiseError("List index out of bounds ({0})", index);
			}
			this.InsertItem(index, str, obj);
		}

		private void InsertItem(int index, string str, object obj)
		{
			this.Changing();
			this.fList.Insert(index, new StringItem(str, obj));
			this.Changed();
		}

		public void ExchangeItems(int index1, int index2)
		{
			StringItem temp = this.fList[index1];
			this.fList[index1] = this.fList[index2];
			this.fList[index2] = temp;
		}

		#region Updating

		private void SetUpdateState(bool updating)
		{
			if (updating)
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
			if (this.fUpdateCount == 0)
			{
				this.SetUpdateState(true);
			}
			this.fUpdateCount++;
		}

		public void EndUpdate()
		{
			this.fUpdateCount--;
			if (this.fUpdateCount == 0)
			{
				this.SetUpdateState(false);
			}
		}

		private void Changed()
		{
			if (this.fUpdateCount == 0 && this.fOnChange != null)
			{
				this.fOnChange(this);
			}
		}

		private void Changing()
		{
			if (this.fUpdateCount == 0 && this.fOnChanging != null)
			{
				this.fOnChanging(this);
			}
		}

		#endregion

		#region Search

		public int xIndexOf(string str)
		{
			int num = this.fList.Count - 1;
			for (int res = 0; res <= num; res++)
			{
				if (this.CompareStrings(this.fList[res].FString, str) == 0)
				{
					return res;
				}
			}

			return -1;
		}

		public int IndexOf(string str)
		{
			int result = -1;

			if (!this.Sorted)
			{
				result = xIndexOf(str);
			}
			else
			{
				if (!this.Find(str, ref result))
				{
					result = -1;
				}
			}

			return result;
		}

		public int IndexOfObject(object obj)
		{
			int num = this.fList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fList[i].FObject == obj)
				{
					return i;
				}
			}

			return -1;
		}

		public bool Find(string str, ref int index)
		{
			bool result = false;

            int L = 0;
			int H = this.fList.Count - 1;
			if (L <= H)
			{
				do
				{
					int I = (int)((uint)(L + H) >> 1);
					int C = this.CompareStrings(this.fList[I].FString, str);
					if (C < 0)
					{
						L = I + 1;
					}
					else
					{
						H = I - 1;
						if (C == 0)
						{
							result = true;
							if (this.Duplicates != StringList.TDuplicates.dupAccept)
							{
								L = I;
							}
						}
					}
				}
				while (L <= H);
			}
			index = L;

            return result;
		}

		#endregion

		#region Sorting

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

		public int CompareStrings(string s1, string s2)
		{
			return string.Compare(s1, s2, !this.fCaseSensitive);
		}

		public int SCompare(int index1, int index2)
		{
			return string.Compare(fList[index1].FString, fList[index2].FString, !this.fCaseSensitive);
		}

		public void Sort()
		{
			if (!this.fSorted && this.fList.Count > 1)
			{
				this.Changing();
				this.QuickSort(0, this.fList.Count - 1);
				this.Changed();
			}
		}

		#endregion

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
