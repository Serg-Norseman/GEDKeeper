using System;
using System.Runtime.InteropServices;
using System.Text;

namespace GKCore.Sys
{
	public class TStringsEnumerator
	{
		private int FIndex;
		private TStrings FStrings;

		public string Current
		{
			get { return this.GetCurrent(); }
		}

		public TStringsEnumerator(TStrings AStrings)
		{
			this.FIndex = -1;
			this.FStrings = AStrings;
		}

		public string GetCurrent()
		{
			return this.FStrings[this.FIndex];
		}

		public bool MoveNext()
		{
			bool Result = this.FIndex < this.FStrings.Count - 1;
			if (Result) this.FIndex++;
			return Result;
		}
	}

	public abstract class TStrings : MarshalByRefObject
	{
		public enum E21 : byte
		{
			sdDelimiter,
			sdQuoteChar,
			sdNameValueSeparator,
			sdLineBreak,
			sdStrictDelimiter
		}

		[Flags, TSetElementType(typeof(E21))]
		public enum TStringsDefined : byte
		{
			sdDelimiter = 1,
			sdQuoteChar = 2,
			sdNameValueSeparator = 4,
			sdLineBreak = 8,
			sdStrictDelimiter = 16
		}

		private TStrings.TStringsDefined FDefined;
		private string FLineBreak;
		private char FNameValueSeparator;
		private bool FStrictDelimiter;
		protected int FUpdateCount;

		protected int UpdateCount
		{
			get { return this.FUpdateCount; }
		}

		public int Capacity
		{
			get { return this.GetCapacity(); }
			set { this.SetCapacity(value); }
		}

		public int Count
		{
			get { return this.GetCount(); }
		}

		public string LineBreak
		{
			get { return this.GetLineBreak(); }
			set { this.SetLineBreak(value); }
		}

		/*public object Objects//[int Index]
		{
			get { return this.GetObject(Index); }
			set { this.PutObject(Index, value); }
		}*/

		public bool StrictDelimiter
		{
			get { return this.GetStrictDelimiter(); }
			set { this.SetStrictDelimiter(value); }
		}

		public string this[int Index]
		{
			get { return this.Get(Index); }
			set { this.Put(Index, value); }
		}

		public string Text
		{
			get { return this.GetTextStr(); }
			set { this.SetTextStr(value); }
		}

		protected string GetLineBreak()
		{
			if ((this.FDefined & TStrings.TStringsDefined.sdLineBreak) == (TStrings.TStringsDefined)0)
			{
				this.LineBreak = "\r\n";
			}
			return this.FLineBreak;
		}

		protected void SetLineBreak([In] string Value)
		{
			if (this.FLineBreak != Value || (this.FDefined & TStrings.TStringsDefined.sdLineBreak) == (TStrings.TStringsDefined)0)
			{
				this.FDefined |= TStrings.TStringsDefined.sdLineBreak;
				this.FLineBreak = Value;
			}
		}

		protected bool GetStrictDelimiter()
		{
			if ((this.FDefined & TStrings.TStringsDefined.sdStrictDelimiter) == (TStrings.TStringsDefined)0)
			{
				this.StrictDelimiter = false;
			}
			return this.FStrictDelimiter;
		}

		protected void SetStrictDelimiter([In] bool Value)
		{
			if (this.FStrictDelimiter != Value || (this.FDefined & TStrings.TStringsDefined.sdStrictDelimiter) == (TStrings.TStringsDefined)0)
			{
				this.FDefined |= TStrings.TStringsDefined.sdStrictDelimiter;
				this.FStrictDelimiter = Value;
			}
		}

		protected void Error([In] string Msg, int Data)
		{
			throw new EStringListError(string.Format(Msg, new object[] { Data }));
		}

		protected abstract string Get(int Index);

		protected virtual int GetCapacity()
		{
			return this.Count;
		}

		protected abstract int GetCount();

		public virtual object GetObject(int Index)
		{
			return null;
		}

		protected virtual string GetTextStr()
		{
			int Count = this.GetCount();
			StringBuilder Buffer = new StringBuilder();

			int num = Count - 1;
			for (int I = 0; I <= num; I++)
			{
				Buffer.Append(this.Get(I));
				Buffer.Append(this.LineBreak);
			}
			return Buffer.ToString();
		}

		protected virtual void Put(int Index, [In] string S)
		{
			object TempObject = this.GetObject(Index);
			this.Delete(Index);
			this.InsertObject(Index, S, TempObject);
		}

		public virtual void PutObject(int Index, object AObject)
		{
		}

		protected virtual void SetCapacity(int NewCapacity)
		{
		}

		protected virtual void SetTextStr([In] string Value)
		{
			this.BeginUpdate();
			try
			{
				this.Clear();
				int Start = 1;
				string lineBreak = this.LineBreak;
				int L = (lineBreak != null) ? lineBreak.Length : 0;
				int P = SysUtils.Pos(this.LineBreak, Value);
				if (P > 0)
				{
					do
					{
						this.Add(SysUtils.WStrCopy(Value, Start, P - Start));
						Start = P + L;
						P = SysUtils.PosEx(this.LineBreak, Value, Start);
					}
					while (P > 0);
				}
				if (Start <= ((Value != null) ? Value.Length : 0))
				{
					this.Add(SysUtils.WStrCopy(Value, Start, ((Value != null) ? Value.Length : 0) - Start + 1));
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		protected virtual void SetUpdateState(bool Updating)
		{
		}

		public virtual int CompareStrings([In] string S1, [In] string S2)
		{
			return string.Compare(S1, S2, true);
		}

		public virtual int Add([In] string S)
		{
			int Result = this.GetCount();
			this.Insert(Result, S);
			return Result;
		}

		public virtual int AddObject([In] string S, object AObject)
		{
			int Result = this.Add(S);
			this.PutObject(Result, AObject);
			return Result;
		}

		public virtual void AddStrings(TStrings Strings)
		{
			this.BeginUpdate();
			try
			{
				int num = Strings.Count - 1;
				for (int I = 0; I <= num; I++)
				{
					this.AddObject(Strings[I], Strings.GetObject(I));
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public void Assign(TStrings Source)
		{
			if (Source is TStrings)
			{
				this.BeginUpdate();
				try
				{
					this.Clear();
					this.FDefined = (Source as TStrings).FDefined;
					this.FNameValueSeparator = (Source as TStrings).FNameValueSeparator;
					this.FLineBreak = (Source as TStrings).FLineBreak;
					this.FStrictDelimiter = (Source as TStrings).FStrictDelimiter;
					this.AddStrings(Source as TStrings);
				}
				finally
				{
					this.EndUpdate();
				}
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

		public abstract void Clear();
		public abstract void Delete(int Index);

		public void EndUpdate()
		{
			this.FUpdateCount--;
			if (this.FUpdateCount == 0)
			{
				this.SetUpdateState(false);
			}
		}

		public virtual void Exchange(int Index1, int Index2)
		{
			this.BeginUpdate();
			try
			{
				string TempString = this[Index1];
				object TempObject = this.GetObject(Index1);
				this[Index1] = this[Index2];
				this.PutObject(Index1, this.GetObject(Index2));
				this[Index2] = TempString;
				this.PutObject(Index2, TempObject);
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public TStringsEnumerator GetEnumerator()
		{
			return new TStringsEnumerator(this);
		}

		public virtual int IndexOf([In] string S)
		{
			int num = this.GetCount() - 1;
			int Result = 0;
			if (num >= Result)
			{
				num++;
				while (this.CompareStrings(this.Get(Result), S) != 0)
				{
					Result++;
					if (Result == num)
					{
						goto IL_2B;
					}
				}
				return Result;
			}
			IL_2B:
			Result = -1;
			return Result;
		}

		public virtual int IndexOfObject(object AObject)
		{
			int res;

			if (AObject == null)
			{
				int num = this.GetCount() - 1;
				res = 0;
				if (num >= res)
				{
					num++;
					while (this.GetObject(res) != null)
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
				int num2 = this.GetCount() - 1;
				res = 0;
				if (num2 >= res)
				{
					num2++;
					while (!AObject.Equals(this.GetObject(res)))
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

		public abstract void Insert(int Index, [In] string S);

		public virtual void InsertObject(int Index, [In] string S, object AObject)
		{
			this.Insert(Index, S);
			this.PutObject(Index, AObject);
		}

		public virtual void Move(int CurIndex, int NewIndex)
		{
			if (CurIndex != NewIndex)
			{
				this.BeginUpdate();
				try
				{
					string TempString = this.Get(CurIndex);
					object TempObject = this.GetObject(CurIndex);
					this.Delete(CurIndex);
					this.InsertObject(NewIndex, TempString, TempObject);
				}
				finally
				{
					this.EndUpdate();
				}
			}
		}

		public TStrings()
		{
			// dummy
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
