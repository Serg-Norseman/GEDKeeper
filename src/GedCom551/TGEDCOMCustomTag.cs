using System;
using System.IO;
using System.Runtime.InteropServices;

using GKSys;

namespace GedCom551
{
	public class TGEDCOMCustomTag : TGEDCOMObject, IDisposable
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

		private static readonly TGEDCOMCustomTag.TTagPropsRec[] TagBase;

		protected int FLevel;
		protected TGEDCOMTree FOwner;
		protected string FName;
		protected TGEDCOMObject FParent;
		protected string FStringValue;
		protected TGEDCOMListEx<TGEDCOMTag> FTags;
		protected bool Disposed_;

		protected TGEDCOMCustomRecord ParentRecord
		{
			get	{ return this.GetParentRecord(); }
		}

		public int Count
		{
			get { return ((this.FTags == null) ? 0 : this.FTags.Count); }
		}

		public TGEDCOMTag this[int Index]
		{
			get { return ((this.FTags == null) ? null : (this.FTags[Index] as TGEDCOMTag)); }
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

		private TTagPropsRec GetTagProps([In] string aName)
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

		protected TGEDCOMCustomRecord GetParentRecord()
		{
			TGEDCOMCustomRecord Result = null;
			TGEDCOMObject O = this.Parent;
			while (O != null && O is TGEDCOMCustomTag)
			{
				if (O is TGEDCOMCustomRecord)
				{
					Result = (O as TGEDCOMCustomRecord);
					break;
				}
				O = (O as TGEDCOMCustomTag).Parent;
			}
			return Result;
		}

		protected virtual void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			this.FOwner = AOwner;
			this.FParent = AParent;
			this.FTags = null;
			this.FStringValue = "";

			if (AParent != null && AParent is TGEDCOMCustomTag)
			{
				this.FLevel = (AParent as TGEDCOMCustomTag).Level + 1;
			}
			else
			{
				this.FLevel = 0;
			}
		}

		protected TGEDCOMRecord FindRecord([In] string XRef)
		{
			return ((this.FOwner == null) ? null : this.FOwner.XRefIndex_Find(XRef));
		}

		public TGEDCOMTag InsertTag(TGEDCOMTag ATag)
		{
			if (this.FTags == null)
			{
				this.FTags = new TGEDCOMListEx<TGEDCOMTag>(this);
			}

			this.FTags.Add(ATag);
			return ATag;
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

		protected virtual void SetStringValue([In] string S)
		{
			this.ParseString(S);
		}

		protected virtual void SaveTagToStream(StreamWriter AStream, TGEDCOMTag ATag)
		{
			if (ATag != null)
			{
				ATag.SaveToStream(AStream);
			}
		}

		protected virtual void SaveTagToStream(StreamWriter AStream, [In] string ATag)
		{
			TGEDCOMTag Tag = this.FindTag(ATag, 0);
			if (Tag != null)
			{
				do
				{
					int Index = this.IndexOfTag(Tag);
					Tag.SaveToStream(AStream);
					Tag = this.FindTag(ATag, Index + 1);
				}
				while (Tag != null);
			}
		}

		protected virtual void SaveTagsToStream(StreamWriter AStream, [In] params string[] ATagSorting)
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
								this.SaveTagToStream(AStream, this[I]);
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

					if (ATagSorting == null || ATagSorting.Length == 0)
					{
						int num3 = this.Count - 1;
						for (int I = 0; I <= num3; I++)
						{
							if (this[I].Name != "CONT" && this[I].Name != "CONC")
							{
								this.SaveTagToStream(AStream, this[I]);
							}
						}
					}
					else
					{
						int num4 = ((ATagSorting != null) ? ATagSorting.Length : 0) - 1;
						for (int I = 0; I <= num4; I++)
						{
							int Index = SavedTags.IndexOf(ATagSorting[I]);
							if (Index >= 0)
							{
								SavedTags.Delete(Index);
							}
							this.SaveTagToStream(AStream, ATagSorting[I]);
						}

						int num5 = SavedTags.Count - 1;
						for (int I = 0; I <= num5; I++)
						{
							this.SaveTagToStream(AStream, SavedTags[I]);
						}
					}
				}
				finally
				{
					SavedTags.Free();
				}
			}
		}

		protected virtual void SaveValueToStream(StreamWriter AStream)
		{
			string S = this.Level.ToString() + " " + this.Name;
			string Val = this.StringValue;

			if (Val != "") S = S + " " + Val;

			AStream.WriteLine(S);
		}

		public TGEDCOMCustomTag(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			this.CreateObj(AOwner, AParent);

			if (AName != "" || AValue != "")
			{
				this.Name = AName;
				this.SetStringValue(AValue);
			}
		}

		public static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return null;
		}

		public virtual void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FTags != null)
				{
					this.FTags.Dispose();
					this.FTags = null;
				}
				this.Disposed_ = true;
			}
		}

		public virtual TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result = null;

			try
			{
				if (this.ParentRecord != null)
				{
					Result = this.ParentRecord.AddSubTag(this, ATag, AValue, ATagConstructor);
				}
				else
				{
					TGEDCOMTag tag;
					if (ATagConstructor != null) {
						tag = (TGEDCOMTag)ATagConstructor(this.Owner, this, ATag, AValue);
					} else {
						tag = this.CreateGEDCOMTag(this.Owner, this, ATag, AValue);
					}
					Result = this.InsertTag(tag);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGEDCOMCustomTag.AddTag(): " + E.Message);
			}

			return Result;
		}

		public virtual void Assign(TGEDCOMCustomTag Source)
		{
			if (Source != null)
			{
				this.Name = Source.Name;
				this.StringValue = Source.StringValue;

				int num = Source.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMTag tag = Source[i];
					//TGEDCOMTag copy = (tag.ClassType() as TGEDCOMTag.MetaTGEDCOMTag).Create(this.Owner, this, "", "") as TGEDCOMTag;
					TGEDCOMTag copy = (TGEDCOMTag)Activator.CreateInstance(tag.GetType(), new object[] { this.Owner, this, "", "" });
					copy.Assign(tag);
					this.InsertTag(copy);
				}
			}
		}

		public virtual void Clear()
		{
			if (this.FTags != null)
			{
				this.FTags.Dispose();
				this.FTags = null;
			}
			this.FStringValue = "";
		}

		public void Delete(int Index)
		{
			this.FTags.Delete(Index);
		}

		public void DeleteTag([In] string ATag)
		{
			if (this.FTags != null)
			{
				TGEDCOMTag Tag = this.FindTag(ATag, 0);
				int Index = this.FTags.IndexOfObject(Tag);
				if (Tag != null)
				{
					do
					{
						this.FTags.DeleteObject(Tag);
						Tag = this.FindTag(ATag, Index);
					}
					while (Tag != null);
				}
			}
		}

		public TGEDCOMTag FindTag([In] string ATag, int StartIndex)
		{
			string SU = ATag.ToUpper();

			int pos = SU.IndexOf('\\');
			string S = ((pos >= 0) ? SU.Substring(0, pos) : SU);

			TGEDCOMCustomTag O = this;
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

		public TGEDCOMTag TagClass([In] string ATag, Type ATagClass, TagConstructor ATagConstructor)
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

		public int GetTagIntegerValue([In] string ATag, int ADefault)
		{
			string S = this.GetTagStringValue(ATag);
			int Result = ((S == "") ? ADefault : SysUtils.StrToIntDef(S, ADefault));
			return Result;
		}

		public string GetTagStringValue([In] string ATag)
		{
			TGEDCOMTag Tag = this.FindTag(ATag, 0);
			string Result = ((Tag == null) ? "" : Tag.StringValue);
			return Result;
		}

		public StringList GetTagStrings(TGEDCOMCustomTag ATag, ref StringList AStrings)
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

		public int IndexOfTag(TGEDCOMTag ATag)
		{
			return ((this.FTags == null) ? -1 : this.FTags.IndexOfObject(ATag));
		}

		public virtual bool IsEmpty()
		{
			return (this.FStringValue == "" && (this.FTags == null || this.FTags.Count == 0));
		}

		public virtual string ParseString([In] string AString)
		{
			this.FStringValue = AString;
			string Result = "";
			return Result;
		}

		public virtual void SaveToStream(StreamWriter AStream)
		{
			this.SaveValueToStream(AStream);
			string[] aTagSorting = new string[0];
			this.SaveTagsToStream(AStream, aTagSorting);
		}

		public void SetTagIntegerValue([In] string ATag, int AValue)
		{
			this.SetTagStringValue(ATag, AValue.ToString());
		}

		public void SetTagStringValue([In] string ATag, [In] string AValue)
		{
			string SU = ATag;
			TGEDCOMCustomTag P = this.FindTag(SU, 0);
			if (P != null)
			{
				P.StringValue = AValue;
			}
			else
			{
				TGEDCOMCustomTag O = this;
				while (SU != "")
				{
					int Index = SysUtils.Pos("\\", SU);
					string S;
					if (Index > 0)
					{
						S = SysUtils.WStrCopy(SU, 1, Index - 1);
						SU = SysUtils.WStrCopy(SU, Index + 1, 2147483647);
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

		public void SetTagStrings(TGEDCOMCustomTag ATag, StringList Value)
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
						if (I == 0 && !(ATag is TGEDCOMRecord))
						{
							ATag.StringValue = SysUtils.WStrCopy(S, 1, 248);
						}
						else
						{
							ATag.AddTag("CONT", SysUtils.WStrCopy(S, 1, 248), null);
						}

						S = S.Remove(0, ((S.Length > 248) ? 248 : S.Length) /*248*/);

						while (((S != null) ? S.Length : 0) > 0)
						{
							ATag.AddTag("CONC", SysUtils.WStrCopy(S, 1, 248), null);
							S = S.Remove(0, ((S.Length > 248) ? 248 : S.Length) /*248*/);
						}
					}
				}
			}
		}

		public void SetTagStrings(TGEDCOMCustomTag ATag, params string[] Value)
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
					if (I == 0 && !(ATag is TGEDCOMRecord))
					{
						ATag.StringValue = SysUtils.WStrCopy(S, 1, 248);
					}
					else
					{
						ATag.AddTag("CONT", SysUtils.WStrCopy(S, 1, 248), null);
					}

					S = S.Remove(0, ((S.Length > 248) ? 248 : S.Length) /*248*/);

					while (((S != null) ? S.Length : 0) > 0)
					{
						ATag.AddTag("CONC", SysUtils.WStrCopy(S, 1, 248), null);
						S = S.Remove(0, ((S.Length > 248) ? 248 : S.Length) /*248*/);
					}
				}
			}
		}

		public virtual void Pack()
		{
			if (this.FTags != null) this.FTags.Pack();
		}

		public virtual void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			if (this.FTags != null) this.FTags.ReplaceXRefs(aMap);
		}

		public virtual void ResetOwner(TGEDCOMTree AOwner)
		{
			this.FOwner = AOwner;

			if (this.FTags != null) this.FTags.ResetOwner(AOwner);
		}

		public void ResetParent(TGEDCOMObject AParent)
		{
			this.FParent = AParent;
		}

		protected TGEDCOMTag CreateGEDCOMTag(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string ATag, [In] string AValue)
		{
			TGEDCOMTag result = (TGEDCOMTag)GEDCOMFactory.GetInstance().Create(AOwner, AParent, ATag, AValue);

			if (result == null) {
				result = new TGEDCOMTag(AOwner, AParent, ATag, AValue);
			}

			return result;
		}

		static TGEDCOMCustomTag()
		{
			TGEDCOMCustomTag.TTagPropsRec[] array = new TGEDCOMCustomTag.TTagPropsRec[26];

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

			TGEDCOMCustomTag.TagBase = array;
		}
	}
}
