using System;
using System.Globalization;
using System.IO;

using Ext.Utils;

namespace GedCom551
{
	public class TGEDCOMTag : TGEDCOMObject, IDisposable
	{
		private struct TTagPropsRec
		{
			public string Name;
			public bool EmptySkip;

			public TTagPropsRec(string aName, bool aEmptySkip) {
				this.Name = aName;
				this.EmptySkip = aEmptySkip;
			}
		}

		private static readonly TGEDCOMTag.TTagPropsRec[] TagBase;

		protected int FLevel;
		protected TGEDCOMTree FOwner;
		protected string FName;
		protected TGEDCOMObject FParent;
		protected string FStringValue;
		protected GEDCOMList<TGEDCOMTag> FTags;
		protected bool Disposed_;

		protected TGEDCOMCustomRecord ParentRecord
		{
			get	{ 
				TGEDCOMCustomRecord Result = null;
				TGEDCOMObject O = this.Parent;
				while (O != null && O is TGEDCOMTag)
				{
					if (O is TGEDCOMCustomRecord)
					{
						Result = (O as TGEDCOMCustomRecord);
						break;
					}
					O = (O as TGEDCOMTag).Parent;
				}
				return Result;
			}
		}

		public int Count
		{
			get { return this.FTags.Count; }
		}

		public TGEDCOMTag this[int Index]
		{
			get { return this.FTags[Index]; }
		}

		public int Level
		{
			get { return this.FLevel; }
		}

		public string Name
		{
			get { return this.FName; }
			set { this.FName = value; }
		}

		public TGEDCOMTree Owner
		{
			get { return this.FOwner; }
		}

		public TGEDCOMObject Parent
		{
			get { return this.FParent; }
		}

		public string StringValue
		{
			get { return this.GetStringValue(); }
			set { this.SetStringValue(value); }
		}

		private TTagPropsRec GetTagProps(string aName)
		{
			TTagPropsRec result;

			int num = TagBase.Length - 1;
			for (int i = 1; i <= num; i++)
			{
				if (TagBase[i].Name == aName)
				{
					result = TagBase[i];
					return result;
				}
			}

			result = TagBase[0];
			return result;
		}

		protected TGEDCOMRecord FindRecord(string xref)
		{
			return ((this.FOwner == null) ? null : this.FOwner.XRefIndex_Find(xref));
		}

		protected TGEDCOMTag InsertTag(TGEDCOMTag tag)
		{
			this.FTags.Add(tag);
			return tag;
		}

		public bool IsEmptySkip()
		{
			return this.GetTagProps(this.Name).EmptySkip;
		}

		public void SetLevel(int Value)
		{
			this.FLevel = Value;
		}

		protected virtual string GetStringValue()
		{
			return this.FStringValue;
		}

		protected virtual void SetStringValue(string S)
		{
			this.ParseString(S);
		}

		protected virtual void SaveTagsToStream(StreamWriter AStream)
		{
			if (this.Count > 0)
			{
				StringList SavedTags = new StringList();
				try
				{
					SavedTags.Duplicates = StringList.TDuplicates.dupIgnore;
					SavedTags.Sorted = true;

					int num = this.Count - 1;
					for (int I = 0; I <= num; I++)
					{
						SavedTags.Add(this[I].Name);
					}

					if (SavedTags.IndexOf("CONC") >= 0 || SavedTags.IndexOf("CONT") >= 0)
					{
						int num2 = this.Count - 1;
						for (int I = 0; I <= num2; I++)
						{
							if (this[I].Name == "CONC" || this[I].Name == "CONT")
							{
								this[I].SaveToStream(AStream);
							}
						}
						if (SavedTags.IndexOf("CONC") >= 0)
						{
							SavedTags.Delete(SavedTags.IndexOf("CONC"));
						}
						if (SavedTags.IndexOf("CONT") >= 0)
						{
							SavedTags.Delete(SavedTags.IndexOf("CONT"));
						}
					}

					int num3 = this.Count - 1;
					for (int I = 0; I <= num3; I++) {
						if (this[I].Name != "CONT" && this[I].Name != "CONC") {
							this[I].SaveToStream(AStream);
						}
					}
				}
				finally
				{
					SavedTags.Free();
				}
			}
		}

		protected virtual void SaveValueToStream(StreamWriter stream)
		{
			string S = this.FLevel.ToString() + " " + this.FName;

			string Val = this.StringValue;
			if (!string.IsNullOrEmpty(Val)) {
				S = S + " " + Val;
			}

			stream.WriteLine(S);
		}

		public virtual void SaveToStream(StreamWriter stream)
		{
			this.SaveValueToStream(stream);
			this.SaveTagsToStream(stream);
		}

		protected virtual void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			this.FOwner = owner;
			this.FParent = parent;
			this.FTags = new GEDCOMList<TGEDCOMTag>(this);
			this.FStringValue = "";

			if (parent != null && parent is TGEDCOMTag) {
				this.FLevel = (parent as TGEDCOMTag).Level + 1;
			} else {
				this.FLevel = 0;
			}
		}

		public TGEDCOMTag(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			this.CreateObj(owner, parent);

			if (tagName != "" || tagValue != "")
			{
				this.Name = tagName;
				this.SetStringValue(tagValue);
			}
		}

		public static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
            return new TGEDCOMTag(owner, parent, tagName, tagValue);
		}

		public virtual void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FTags != null) {
					this.FTags.Dispose();
					this.FTags = null;
				}
				this.Disposed_ = true;
			}
		}

		public virtual TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag tag = null;
			try
			{
				if (tagConstructor != null) {
					tag = tagConstructor(this.FOwner, this, tagName, tagValue);
				} else {
					tag = GEDCOMFactory.GetInstance().CreateTag(this.FOwner, this, tagName, tagValue);
					if (tag == null) {
						tag = new TGEDCOMTag(this.FOwner, this, tagName, tagValue);
					}
				}

				this.InsertTag(tag);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGEDCOMTag.InternalCreateTag(): " + E.Message);
			}
			return tag;
		}

		public virtual void Assign(TGEDCOMTag source)
		{
			if (source != null)
			{
				this.Name = source.Name;
				this.StringValue = source.StringValue;

				int num = source.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMTag sourceTag = source[i];
					TGEDCOMTag copy = Activator.CreateInstance(sourceTag.GetType(), new object[] { this.Owner, this, "", "" }) as TGEDCOMTag;
					copy.Assign(sourceTag);
					this.InsertTag(copy);
				}
			}
		}

		protected void AssignList(GEDCOMList<TGEDCOMTag> srcList, GEDCOMList<TGEDCOMTag> destList)
		{
			int num = srcList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMTag sourceTag = srcList[i];
				TGEDCOMTag copy = Activator.CreateInstance(sourceTag.GetType(), new object[] { this.Owner, this, "", "" }) as TGEDCOMTag;
				copy.Assign(sourceTag);
				destList.Add(copy);
			}
		}

		public virtual void Clear()
		{
			this.FTags.Clear();
			this.FStringValue = "";
		}

		public void Delete(int index)
		{
			this.FTags.Delete(index);
		}

		//FIXME: некачественна€ реализаци€, перепроверить
		public void DeleteTag(string tagName)
		{
			TGEDCOMTag tag = this.FindTag(tagName, 0);
			if (tag != null) {
				do
				{
					int idx = this.FTags.IndexOfObject(tag);
					this.FTags.Delete(idx);
					tag = this.FindTag(tagName, idx);
				}
				while (tag != null);
			}
		}

		public TGEDCOMTag FindTag(string ATag, int StartIndex)
		{
			string SU = ATag.ToUpperInvariant();

			int pos = SU.IndexOf('\\');
			string S = ((pos >= 0) ? SU.Substring(0, pos) : SU);

			TGEDCOMTag O = this;
			TGEDCOMTag Result;

			while (true)
			{
				int Index = ((S == SU) ? StartIndex : 0);

				while (Index < O.Count && O[Index].Name != S) Index++;

				if (Index >= O.Count) break;

				Result = O[Index];
				O = Result;

				pos = SU.IndexOf('\\');
				if (pos >= 0)
				{
					SU = SU.Substring(pos + 1);

					pos = SU.IndexOf('\\');
					S = ((pos >= 0) ? SU.Substring(0, pos) : SU);
				}
				else
				{
					SU = "";
				}

				if (SU == "") return Result;
			}

			Result = null;
			return Result;
		}

		public TGEDCOMTag TagClass(string ATag, Type ATagClass, TagConstructor ATagConstructor)
		{
			TGEDCOMTag result = this.FindTag(ATag, 0);

			if (result == null)
			{
				result = this.AddTag(ATag, "", ATagConstructor);
			}
			else
			{
				if (!ATagClass.IsInstanceOfType(result))
				{
					throw new EGEDCOMException(string.Format("The tag {0} is of type {1}, but type {2} was expected", new object[]
					{
						ATag, result.GetType().Name, ATagClass.Name
					}));
				}
			}

			return result;
		}

		public int GetTagIntegerValue(string ATag, int ADefault)
		{
			string S = this.GetTagStringValue(ATag);
			int Result = ((S == "") ? ADefault : SysUtils.ParseInt(S, ADefault));
			return Result;
		}

		public double GetTagFloatValue(string ATag, double ADefault)
		{
			string S = this.GetTagStringValue(ATag);
			double Result = ((S == "") ? ADefault : SysUtils.ParseFloat(S, ADefault));
			return Result;
		}

		public string GetTagStringValue(string ATag)
		{
			TGEDCOMTag Tag = this.FindTag(ATag, 0);
			string Result = ((Tag == null) ? "" : Tag.StringValue);
			return Result;
		}

		public StringList GetTagStrings(TGEDCOMTag ATag, ref StringList AStrings)
		{
			if (AStrings == null)
			{
				AStrings = new StringList();
			}
			else
			{
				AStrings.Clear();
			}

			if (ATag != null)
			{
				if (ATag.StringValue != "")
				{
					AStrings.Add(ATag.StringValue);
				}

				int num = ATag.Count - 1;
				for (int I = 0; I <= num; I++)
				{
					TGEDCOMTag Tag = ATag[I];
					if (Tag.Name == "CONC")
					{
						AStrings[AStrings.Count - 1] = AStrings[AStrings.Count - 1] + Tag.StringValue;
					}
					else
					{
						if (Tag.Name == "CONT")
						{
							AStrings.Add(Tag.StringValue);
						}
					}
				}
			}

			return AStrings;
		}

		public int IndexOfTag(TGEDCOMTag tag)
		{
			return this.FTags.IndexOfObject(tag);
		}

		public virtual bool IsEmpty()
		{
			return ((this.FStringValue == "") && (this.FTags.Count == 0));
		}

		public virtual string ParseString(string AString)
		{
			this.FStringValue = AString;
			string Result = "";
			return Result;
		}

		public void SetTagIntegerValue(string ATag, int AValue)
		{
			this.SetTagStringValue(ATag, AValue.ToString());
		}

		public void SetTagFloatValue(string ATag, double AValue)
		{
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";
			this.SetTagStringValue(ATag, AValue.ToString(nfi));
		}

		public void SetTagStringValue(string ATag, string AValue)
		{
			string SU = ATag;
			TGEDCOMTag P = this.FindTag(SU, 0);
			if (P != null)
			{
				P.StringValue = AValue;
			}
			else
			{
				TGEDCOMTag O = this;
				while (SU != "")
				{
					string S;

					int Index = SU.IndexOf('\\');
					if (Index >= 0)
					{
						S = SU.Substring(0, Index);
						SU = SU.Substring(Index + 1);
					}
					else
					{
						S = SU;
						SU = "";
					}

					P = O.FindTag(S, 0);
					if (P == null)
					{
						if (SU == "")
						{
							P = O.AddTag(S, AValue, null);
						}
						else
						{
							P = O.AddTag(S, "", null);
						}
					}
					else
					{
						if (SU == "")
						{
							(P as TGEDCOMTag).StringValue = AValue;
						}
					}
					O = P;
				}
			}
		}

		public void SetTagStrings(TGEDCOMTag ATag, StringList Value)
		{
			if (ATag != null)
			{
				ATag.StringValue = "";
				for (int I = ATag.Count - 1; I >= 0; I--)
				{
					if (ATag[I].Name == "CONT" || ATag[I].Name == "CONC")
					{
						ATag.Delete(I);
					}
				}

				if (Value != null)
				{
					int num = Value.Count - 1;
					for (int I = 0; I <= num; I++)
					{
						string S = Value[I];

						int len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
						string sub = S.Substring(0, len);
						S = S.Remove(0, len);

						if (I == 0 && !(ATag is TGEDCOMRecord))
						{
							ATag.StringValue = sub;
						}
						else
						{
							ATag.AddTag("CONT", sub, null);
						}

						while (((S != null) ? S.Length : 0) > 0)
						{
							len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
							ATag.AddTag("CONC", S.Substring(0, len), null);
							S = S.Remove(0, len);
						}
					}
				}
			}
		}

        public void SetTagStrings(TGEDCOMTag ATag, params string[] Value)
		{
			Value = (string[])Value.Clone();

			if (ATag != null)
			{
				ATag.StringValue = "";
				for (int I = ATag.Count - 1; I >= 0; I--)
				{
					if (ATag[I].Name == "CONT" || ATag[I].Name == "CONC")
					{
						ATag.Delete(I);
					}
				}

				for (int I = 0; I <= ((Value != null) ? Value.Length : 0) - 1; I++)
				{
					string S = Value[I];

					int len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
					string sub = S.Substring(0, len);
					S = S.Remove(0, len);

					if (I == 0 && !(ATag is TGEDCOMRecord))
					{
						ATag.StringValue = sub;
					}
					else
					{
						ATag.AddTag("CONT", sub, null);
					}

					while (((S != null) ? S.Length : 0) > 0)
					{
						len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
						ATag.AddTag("CONC", S.Substring(0, len), null);
						S = S.Remove(0, len);
					}
				}
			}
		}

		public virtual void Pack()
		{
			this.FTags.Pack();
		}

		public virtual void ReplaceXRefs(TXRefReplaceMap map)
		{
			this.FTags.ReplaceXRefs(map);
		}

		public virtual void ResetOwner(TGEDCOMTree owner)
		{
			this.FOwner = owner;
			this.FTags.ResetOwner(owner);
		}

		public void ResetParent(TGEDCOMObject parent)
		{
			this.FParent = parent;
		}

		static TGEDCOMTag()
		{
            TGEDCOMTag.TTagPropsRec[] array = new TGEDCOMTag.TTagPropsRec[26];

			array[0] = new TTagPropsRec("", false);
			array[1] = new TTagPropsRec("ADDR", true);
			array[2] = new TTagPropsRec("AGNC", true);
			array[3] = new TTagPropsRec("AUTH", true);
			array[4] = new TTagPropsRec("CAUS", true);
			array[5] = new TTagPropsRec("CHAN", true);
			array[6] = new TTagPropsRec("CITY", true);
			array[7] = new TTagPropsRec("CTRY", true);
			array[8] = new TTagPropsRec("DATE", true);
			array[9] = new TTagPropsRec("PAGE", true);
			array[10] = new TTagPropsRec("PLAC", true);
			array[11] = new TTagPropsRec("POST", true);
			array[12] = new TTagPropsRec("PUBL", true);
			array[13] = new TTagPropsRec("RESN", true);
			array[14] = new TTagPropsRec("STAE", true);
			array[15] = new TTagPropsRec("TEXT", true);
			array[16] = new TTagPropsRec("TIME", true);
			array[17] = new TTagPropsRec("TYPE", true);
			array[18] = new TTagPropsRec("SUBM", true);
			array[19] = new TTagPropsRec("NPFX", true);
			array[20] = new TTagPropsRec("GIVN", true);
			array[21] = new TTagPropsRec("NICK", true);
			array[22] = new TTagPropsRec("SPFX", true);
			array[23] = new TTagPropsRec("SURN", true);
			array[24] = new TTagPropsRec("NSFX", true);
			array[25] = new TTagPropsRec("_LOC", true);

            TGEDCOMTag.TagBase = array;
		}
	}
}
