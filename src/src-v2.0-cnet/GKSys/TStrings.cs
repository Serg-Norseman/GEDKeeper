using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace GKSys
{
	public class TStringsEnumerator
	{
		internal int FIndex;
		internal TStrings FStrings;

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

	public abstract class TStrings : MarshalByRefObject//, _TPersistentHelper
	{
		internal enum E21 : byte
		{
			sdDelimiter,
			sdQuoteChar,
			sdNameValueSeparator,
			sdLineBreak,
			sdStrictDelimiter
		}

		[Flags, TSetElementType(typeof(E21))]
		[Serializable]
		public enum TStringsDefined : byte
		{
			sdDelimiter = 1,
			sdQuoteChar = 2,
			sdNameValueSeparator = 4,
			sdLineBreak = 8,
			sdStrictDelimiter = 16
		}

		internal TStrings.TStringsDefined FDefined;
		internal string FLineBreak;
		internal char FNameValueSeparator;
		internal bool FStrictDelimiter;
		internal int FUpdateCount;

		protected internal int UpdateCount
		{
			get
			{
				return this.FUpdateCount;
			}
		}

		[Browsable(false)]
		public int Capacity
		{
			get
			{
				return this.GetCapacity();
			}
			set
			{
				this.SetCapacity(value);
			}
		}

		[Browsable(false)]
		public int Count
		{
			get
			{
				return this.GetCount();
			}
		}
		[Browsable(false)]
		public string LineBreak
		{
			get
			{
				return this.GetLineBreak();
			}
			set
			{
				this.SetLineBreak(value);
			}
		}

		/*[Browsable(false)]
		public string Names
		{
			get
			{
				return this.GetName(Index);
			}
		}*/

		/*[Browsable(false)]
		public object Objects//[int Index]
		{
			get
			{
				return this.GetObject(Index);
			}
			set
			{
				this.PutObject(Index, value);
			}
		}*/

		/*[Browsable(false)]
		public string Values
		{
			get
			{
				return this.GetValue(Name);
			}
			set
			{
				this.SetValue(Name, Value);
			}
		}
		[Browsable(false)]
		public string ValueFromIndex
		{
			get
			{
				return this.GetValueFromIndex(Index);
			}
			set
			{
				this.SetValueFromIndex(Index, Value);
			}
		}*/

		[Browsable(false)]
		public char NameValueSeparator
		{
			get
			{
				return this.GetNameValueSeparator();
			}
			set
			{
				this.SetNameValueSeparator(value);
			}
		}
		[Browsable(false)]
		public bool StrictDelimiter
		{
			get
			{
				return this.GetStrictDelimiter();
			}
			set
			{
				this.SetStrictDelimiter(value);
			}
		}

		[Browsable(false)]
		public string this[int Index]
		{
			get
			{
				return this.Get(Index);
			}
			set
			{
				this.Put(Index, value);
			}
		}
		[Browsable(false)]
		public string Text
		{
			get
			{
				return this.GetTextStr();
			}
			set
			{
				this.SetTextStr(value);
			}
		}
		internal string GetName(int Index)
		{
			return this.ExtractName(this.Get(Index));
		}
		internal string GetValue([In] string Name)
		{
			int I = this.IndexOfName(Name);
			string Result;
			if (I >= 0)
			{
				Result = BDSSystem.WStrCopy(this.Get(I), ((Name != null) ? Name.Length : 0) + 2, 2147483647);
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		internal void SetValue([In] string Name, [In] string Value)
		{
			int I = this.IndexOfName(Name);
			if (Value != "")
			{
				if (I < 0) I = this.Add("");
				this.Put(I, Name + this.NameValueSeparator + Value);
			}
			else
			{
				if (I >= 0) this.Delete(I);
			}
		}

		internal string GetLineBreak()
		{
			if ((this.FDefined & TStrings.TStringsDefined.sdLineBreak) == (TStrings.TStringsDefined)0)
			{
				this.LineBreak = "\r\n";
			}
			return this.FLineBreak;
		}
		internal void SetLineBreak([In] string Value)
		{
			if (BDSSystem.WStrCmp(this.FLineBreak, Value) != 0 || (this.FDefined & TStrings.TStringsDefined.sdLineBreak) == (TStrings.TStringsDefined)0)
			{
				this.FDefined |= TStrings.TStringsDefined.sdLineBreak;
				this.FLineBreak = Value;
			}
		}
		internal char GetNameValueSeparator()
		{
			if ((this.FDefined & TStrings.TStringsDefined.sdNameValueSeparator) == (TStrings.TStringsDefined)0)
			{
				this.NameValueSeparator = '=';
			}
			return this.FNameValueSeparator;
		}
		internal void SetNameValueSeparator([In] char Value)
		{
			if (this.FNameValueSeparator != Value || (this.FDefined & TStrings.TStringsDefined.sdNameValueSeparator) == (TStrings.TStringsDefined)0)
			{
				this.FDefined |= TStrings.TStringsDefined.sdNameValueSeparator;
				this.FNameValueSeparator = Value;
			}
		}
		internal bool GetStrictDelimiter()
		{
			if ((this.FDefined & TStrings.TStringsDefined.sdStrictDelimiter) == (TStrings.TStringsDefined)0)
			{
				this.StrictDelimiter = false;
			}
			return this.FStrictDelimiter;
		}
		internal void SetStrictDelimiter([In] bool Value)
		{
			if (this.FStrictDelimiter != Value || (this.FDefined & TStrings.TStringsDefined.sdStrictDelimiter) == (TStrings.TStringsDefined)0)
			{
				this.FDefined |= TStrings.TStringsDefined.sdStrictDelimiter;
				this.FStrictDelimiter = Value;
			}
		}
		internal string GetValueFromIndex(int Index)
		{
			string Result;
			if (Index >= 0)
			{
				string arg_26_0 = this.Get(Index);
				string text = this.GetName(Index);
				Result = BDSSystem.WStrCopy(arg_26_0, ((text != null) ? text.Length : 0) + 2, 2147483647);
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		internal void SetValueFromIndex(int Index, [In] string Value)
		{
			if (Value != "")
			{
				if (Index < 0) Index = this.Add("");
				this.Put(Index, this.GetName(Index) + this.NameValueSeparator + Value);
			}
			else
			{
				if (Index >= 0) this.Delete(Index);
			}
		}

		protected internal void Error([In] string Msg, int Data)
		{
			throw new EStringListError(string.Format(Msg, new object[]
			{
				Data
			}));
		}
		protected internal string ExtractName([In] string S)
		{
			string Result = S;
			int P = BDSSystem.Pos(BDSSystem.WStrFromWChar(this.NameValueSeparator), Result);
			if (P != 0)
			{
				int num = P - 1;
				if (Result == null)
				{
					Result = string.Empty;
				}
				if (num > Result.Length)
				{
					Result = Result.PadRight(num);
				}
				else
				{
					Result = Result.Substring(0, num);
				}
			}
			else
			{
				if (Result == null)
				{
					Result = string.Empty;
				}
				if (0 > Result.Length)
				{
					Result = Result.PadRight(0);
				}
				else
				{
					Result = Result.Substring(0, 0);
				}
			}
			return Result;
		}
		protected internal abstract string Get(int Index);
		protected internal virtual int GetCapacity()
		{
			return this.Count;
		}
		protected internal abstract int GetCount();

		public virtual object GetObject(int Index)
		{
			return null;
		}

		protected internal virtual string GetTextStr()
		{
			int Count = this.GetCount();
			StringBuilder Buffer = new StringBuilder();
			int arg_12_0 = 0;
			int num = Count - 1;
			int I = arg_12_0;
			if (num >= I)
			{
				num++;
				do
				{
					Buffer.Append(this.Get(I));
					Buffer.Append(this.LineBreak);
					I++;
				}
				while (I != num);
			}
			return Buffer.ToString();
		}
		protected internal virtual void Put(int Index, [In] string S)
		{
			object TempObject = this.GetObject(Index);
			this.Delete(Index);
			this.InsertObject(Index, S, TempObject);
		}
		protected internal virtual void PutObject(int Index, object AObject)
		{
		}
		protected internal virtual void SetCapacity(int NewCapacity)
		{
		}
		protected internal virtual void SetTextStr([In] string Value)
		{
			this.BeginUpdate();
			try
			{
				this.Clear();
				int Start = 1;
				string lineBreak = this.LineBreak;
				int L = (lineBreak != null) ? lineBreak.Length : 0;
				int P = BDSSystem.Pos(this.LineBreak, Value);
				if (P > 0)
				{
					do
					{
						this.Add(BDSSystem.WStrCopy(Value, Start, P - Start));
						Start = P + L;
						P = VCLUtils.PosEx(this.LineBreak, Value, Start);
					}
					while (P > 0);
				}
				if (Start <= ((Value != null) ? Value.Length : 0))
				{
					this.Add(BDSSystem.WStrCopy(Value, Start, ((Value != null) ? Value.Length : 0) - Start + 1));
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}
		protected internal virtual void SetUpdateState(bool Updating)
		{
		}
		protected internal virtual int CompareStrings([In] string S1, [In] string S2)
		{
			return VCLUtils.CompareText(S1, S2);
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
		public void Append([In] string S)
		{
			this.Add(S);
		}
		public virtual void AddStrings(TStrings Strings)
		{
			this.BeginUpdate();
			try
			{
				int arg_10_0 = 0;
				int num = Strings.Count - 1;
				int I = arg_10_0;
				if (num >= I)
				{
					num++;
					do
					{
						this.AddObject(Strings[I], Strings.GetObject(I));
						I++;
					}
					while (I != num);
				}
			}
			finally
			{
				this.EndUpdate();
			}
		}

		public void /*_TPersistentHelper.*/Assign(/*MarshalByRefObject*/ TStrings Source)
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
			else
			{
				//TPersistentHelper.Assign(this, Source);
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
		public bool Equals(TStrings Strings)
		{
			bool Result = false;
			int Count = this.GetCount();
			if (Count == Strings.GetCount())
			{
				int arg_17_0 = 0;
				int num = Count - 1;
				int I = arg_17_0;
				if (num >= I)
				{
					num++;
					while (BDSSystem.WStrCmp(this.Get(I), Strings.Get(I)) == 0)
					{
						I++;
						if (I == num)
						{
							goto IL_3D;
						}
					}
					return Result;
				}
				IL_3D:
				Result = true;
			}
			return Result;
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
			int arg_0A_0 = 0;
			int num = this.GetCount() - 1;
			int Result = arg_0A_0;
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
		public virtual int IndexOfName([In] string Name)
		{
			int arg_0A_0 = 0;
			int num = this.GetCount() - 1;
			int Result = arg_0A_0;
			if (num >= Result)
			{
				num++;
				do
				{
					string S = this.Get(Result);
					int P = BDSSystem.Pos(BDSSystem.WStrFromWChar(this.NameValueSeparator), S);
					if (P != 0 && this.CompareStrings(BDSSystem.WStrCopy(S, 1, P - 1), Name) == 0)
					{
						return Result;
					}
					Result++;
				}
				while (Result != num);
			}
			Result = -1;
			return Result;
		}
		public virtual int IndexOfObject(object AObject)
		{
			int Result;
			if (AObject == null)
			{
				int arg_0D_0 = 0;
				int num = this.GetCount() - 1;
				Result = arg_0D_0;
				if (num >= Result)
				{
					num++;
					while (this.GetObject(Result) != null)
					{
						Result++;
						if (Result == num)
						{
							goto IL_53;
						}
					}
					return Result;
				}
			}
			else
			{
				int arg_33_0 = 0;
				int num2 = this.GetCount() - 1;
				Result = arg_33_0;
				if (num2 >= Result)
				{
					num2++;
					while (!AObject.Equals(this.GetObject(Result)))
					{
						Result++;
						if (Result == num2)
						{
							goto IL_53;
						}
					}
					return Result;
				}
			}
			IL_53:
			Result = -1;
			return Result;
		}
		public abstract void Insert(int Index, [In] string S);
		public virtual void InsertObject(int Index, [In] string S, object AObject)
		{
			this.Insert(Index, S);
			this.PutObject(Index, AObject);
		}
		public virtual void LoadFromFile([In] string FileName)
		{
			this.LoadFromFile(FileName, null);
		}
		public virtual void LoadFromFile([In] string FileName, Encoding Encoding)
		{
			TStream Stream = new TFileStream(FileName, 32);
			try
			{
				this.LoadFromStream(Stream, Encoding);
			}
			finally
			{
				Stream.Free();
			}
		}
		public virtual void LoadFromStream(TStream Stream)
		{
			this.LoadFromStream(Stream, null);
		}
		public virtual void LoadFromStream(TStream Stream, Encoding Encoding)
		{
			this.BeginUpdate();
			try
			{
				int Size = (int)(Stream.Size - Stream.Position);
				byte[] Buffer = null;
				byte[] array = Buffer;
				int arg_1E_0;
				if ((arg_1E_0 = Size) < 0)
				{
					arg_1E_0 = 0;
				}
				byte[] array2;
				byte[] expr_23 = array2 = new byte[arg_1E_0];
				if (Size > 0 && array != null)
				{
					int num;
					if ((num = array.Length) > Size)
					{
						num = Size;
					}
					if (num > 0)
					{
						Array.Copy(array, array2, num);
					}
				}
				Buffer = expr_23;
				Stream.Read(ref Buffer, Size);
				Size = 0;
				if (Encoding == null)
				{
					if (TStrings._LoadFromStream_ContainsPreamble(Buffer, Encoding.Unicode.GetPreamble()))
					{
						Encoding = Encoding.Unicode;
					}
					else
					{
						if (TStrings._LoadFromStream_ContainsPreamble(Buffer, Encoding.BigEndianUnicode.GetPreamble()))
						{
							Encoding = Encoding.BigEndianUnicode;
						}
						else
						{
							if (TStrings._LoadFromStream_ContainsPreamble(Buffer, Encoding.UTF8.GetPreamble()))
							{
								Encoding = Encoding.UTF8;
							}
							else
							{
								Encoding = Encoding.Default;
							}
						}
					}
					byte[] preamble = Encoding.GetPreamble();
					Size = ((preamble != null) ? preamble.Length : 0);
				}
				else
				{
					byte[] Preamble = Encoding.GetPreamble();
					if (TStrings._LoadFromStream_ContainsPreamble(Buffer, Preamble))
					{
						Size = ((Preamble != null) ? Preamble.Length : 0);
					}
				}
				this.SetTextStr(Encoding.GetString(Buffer, Size, ((Buffer != null) ? Buffer.Length : 0) - Size));
			}
			finally
			{
				this.EndUpdate();
			}
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
		public virtual void SaveToFile([In] string FileName)
		{
			this.SaveToFile(FileName, null);
		}
		public virtual void SaveToFile([In] string FileName, Encoding Encoding)
		{
			TStream Stream = new TFileStream(FileName, 65535);
			try
			{
				this.SaveToStream(Stream, Encoding);
			}
			finally
			{
				Stream.Free();
			}
		}
		public virtual void SaveToStream(TStream Stream)
		{
			this.SaveToStream(Stream, null);
		}
		public virtual void SaveToStream(TStream Stream, Encoding Encoding)
		{
			if (Encoding == null)
			{
				Encoding = Encoding.Default;
			}
			byte[] Buffer = Encoding.GetBytes(this.GetTextStr());
			byte[] Preamble = Encoding.GetPreamble();
			if (((Preamble != null) ? Preamble.Length : 0) > 0)
			{
				Stream.WriteBuffer(Preamble, (Preamble != null) ? Preamble.Length : 0);
			}
			Stream.WriteBuffer(Buffer, (Buffer != null) ? Buffer.Length : 0);
		}

		/*void _TPersistentHelper.AssignTo(MarshalByRefObject Dest)
		{
			TPersistentHelper.AssignTo(this, Dest);
		}

		MarshalByRefObject _TPersistentHelper.GetOwner()
		{
			return TPersistentHelper.GetOwner(this);
		}*/

		public TStrings()
		{
			TPersistentHelper.Create(this);
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		private static bool _LoadFromStream_ContainsPreamble([In] byte[] Buffer, [In] byte[] Signature)
		{
			bool Result = true;
			if (((Buffer != null) ? Buffer.Length : 0) >= ((Signature != null) ? Signature.Length : 0))
			{
				int arg_21_0 = 1;
				int num = (Signature != null) ? Signature.Length : 0;
				int I = arg_21_0;
				if (num >= I)
				{
					num++;
					while (Buffer[I - 1] == Signature[I - 1])
					{
						I++;
						if (I == num)
						{
							return Result;
						}
					}
					Result = false;
				}
			}
			else
			{
				Result = false;
			}
			return Result;
		}
	}
}
