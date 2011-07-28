using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace GedCom551
{
	public class TGEDCOMCustomTag : TGEDCOMObject, IDisposable
	{
		[StructLayout(LayoutKind.Auto)]
		private struct TTagPropsRec
		{
			public string Name;
			public bool EmptySkip;
		}

		private static readonly TGEDCOMCustomTag.TTagPropsRec[] TagBase;
		internal int FLevel;
		internal TGEDCOMObject FOwner;
		internal string FName;
		internal TGEDCOMObject FParent;
		internal string FStringValue;
		internal TGEDCOMList FTags;
		protected internal bool Disposed_;

		protected internal TGEDCOMCustomRecord ParentRecord
		{
			get	{ return this.GetParentRecord(); }
		}

		public int Count
		{
			get { return this.GetCount(); }
		}

		//[System.Runtime.CompilerServices.IndexerName("Tags")]
		/*public TGEDCOMTag this[int Index]
		{
			get { return this.GetTag(Index); }
		}*/

		public int Level
		{
			get { return this.FLevel; }
		}

		public string Name
		{
			get { return this.FName; }
			set { this.FName = value; }
		}

		public TGEDCOMObject Owner
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

		private TGEDCOMCustomTag.TTagPropsRec GetTagProps([In] string aName)
		{
			int i = 1;
			TGEDCOMCustomTag.TTagPropsRec Result;
			while (BDSSystem.WStrCmp(TGEDCOMCustomTag.TagBase[i].Name, aName) != 0)
			{
				i++;
				if (i == 26)
				{
					Result = TGEDCOMCustomTag.TagBase[0];
					return Result;
				}
			}
			Result = TGEDCOMCustomTag.TagBase[i];
			return Result;
		}

		internal int GetCount()
		{
			int Result;
			if (this.FTags == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FTags.Count;
			}
			return Result;
		}

		internal TGEDCOMCustomRecord GetParentRecord()
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

		public TGEDCOMTag GetTag(int Index)
		{
			TGEDCOMTag Result;
			if (this.FTags != null)
			{
				Result = (this.FTags[Index] as TGEDCOMTag);
			}
			else
			{
				Result = null;
			}
			return Result;
		}

		protected internal virtual void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
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

		protected internal TGEDCOMRecord FindRecord([In] string XRef)
		{
			TGEDCOMRecord Result;
			if (this.FOwner != null)
			{
				Result = (this.FOwner as TGEDCOMTree).XRefIndex_Find(XRef);
			}
			else
			{
				Result = null;
			}
			return Result;
		}

		protected internal virtual string GetStringValue()
		{
			return this.FStringValue;
		}

		protected internal TGEDCOMTag InsertTag(TGEDCOMTag ATag)
		{
			if (this.FTags == null)
			{
				this.FTags = new TGEDCOMList(this);
			}
			this.FTags.Add(ATag);
			return ATag;
		}

		protected internal bool IsEmptySkip()
		{
			return this.GetTagProps(this.Name).EmptySkip;
		}

		protected internal void SetLevel(int Value)
		{
			this.FLevel = Value;
		}

		protected internal virtual void SetStringValue([In] string S)
		{
			string temp = S;
			if ((this.FOwner as TGEDCOMTree).Header.CharacterSet == TGEDCOMObject.TGEDCOMCharacterSet.csUTF8)
			{
				temp = BDSSystem.StrToUtf8(S);
			}
			this.ParseString(temp);
		}

		protected internal virtual void SaveTagToStream(StreamWriter AStream, TGEDCOMTag ATag)
		{
			if (ATag != null)
			{
				ATag.SaveToStream(AStream);
			}
		}

		protected internal virtual void SaveTagToStream(StreamWriter AStream, [In] string ATag)
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

		protected internal virtual void SaveTagsToStream(StreamWriter AStream, [In] params string[] ATagSorting)
		{
			if (this.Count > 0)
			{
				TStringList SavedTags = new TStringList();
				try
				{
					SavedTags.Duplicates = TStringList.TDuplicates.dupIgnore;
					SavedTags.Sorted = true;
					int arg_2A_0 = 0;
					int num = this.Count - 1;
					int I = arg_2A_0;
					if (num >= I)
					{
						num++;
						do
						{
							SavedTags.Add(this.GetTag(I).Name);
							I++;
						}
						while (I != num);
					}
					if (SavedTags.IndexOf("CONC") >= 0 || SavedTags.IndexOf("CONT") >= 0)
					{
						int arg_78_0 = 0;
						int num2 = this.Count - 1;
						I = arg_78_0;
						if (num2 >= I)
						{
							num2++;
							do
							{
								if (BDSSystem.WStrCmp(this.GetTag(I).Name, "CONC") == 0 || BDSSystem.WStrCmp(this.GetTag(I).Name, "CONT") == 0)
								{
									this.SaveTagToStream(AStream, this.GetTag(I));
								}
								I++;
							}
							while (I != num2);
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
						int arg_11F_0 = 0;
						int num3 = this.Count - 1;
						I = arg_11F_0;
						if (num3 >= I)
						{
							num3++;
							do
							{
								if (BDSSystem.WStrCmp(this.GetTag(I).Name, "CONT") != 0 && BDSSystem.WStrCmp(this.GetTag(I).Name, "CONC") != 0)
								{
									this.SaveTagToStream(AStream, this.GetTag(I));
								}
								I++;
							}
							while (I != num3);
						}
					}
					else
					{
						int arg_185_0 = 0;
						int num4 = ((ATagSorting != null) ? ATagSorting.Length : 0) - 1;
						I = arg_185_0;
						if (num4 >= I)
						{
							num4++;
							do
							{
								int Index = SavedTags.IndexOf(ATagSorting[I]);
								if (Index >= 0)
								{
									SavedTags.Delete(Index);
								}
								this.SaveTagToStream(AStream, ATagSorting[I]);
								I++;
							}
							while (I != num4);
						}
						int arg_1C4_0 = 0;
						int num5 = SavedTags.Count - 1;
						I = arg_1C4_0;
						if (num5 >= I)
						{
							num5++;
							do
							{
								this.SaveTagToStream(AStream, SavedTags[I]);
								I++;
							}
							while (I != num5);
						}
					}
				}
				finally
				{
					SavedTags.Free();
				}
			}
		}

		protected internal virtual void SaveValueToStream(StreamWriter AStream)
		{
			string S = this.Level.ToString() + " " + this.Name;
			string Val = this.StringValue;

			if ((this.FOwner as TGEDCOMTree).Header.CharacterSet == TGEDCOMObject.TGEDCOMCharacterSet.csUTF8)
			{
				AnsiString s;
				s.Data = BDSSystem.LStrFromWStr(Val);
				Val = BDSSystem.WStrFromLStr(BDSSystem.AnsiToUtf8(s).Data);
			}

			if (Val != "") S = S + " " + Val;

			AStream.WriteLine(S);
		}

		public TGEDCOMCustomTag(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			this.CreateObj(AOwner, AParent);

			if (AName != "" || AValue != "")
			{
				this.Name = AName;
				this.SetStringValue(AValue);
			}
		}

		public virtual void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FTags != null)
				{
					object fTags = this.FTags;
					VCLUtils.FreeAndNil(ref fTags);
					this.FTags = (fTags as TGEDCOMList);
				}
				this.Disposed_ = true;
			}
		}

		public virtual TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (this.ParentRecord != null)
			{
				Result = this.ParentRecord.AddSubTag(this, ATag, AValue, AClass);
			}
			else
			{
				if (AClass != null)
				{
					TGEDCOMTag tag = (TGEDCOMTag)Activator.CreateInstance(AClass, new object[] { this.Owner, this, ATag, AValue });
					/*AClass.Create(this.Owner, this, ATag, AValue) as TGEDCOMTag*/
					Result = this.InsertTag(tag);
				}
				else
				{
					Result = this.InsertTag(base.CreateGEDCOMTag(this.Owner, this, ATag, AValue));
				}
			}
			return Result;
		}

		public virtual void Assign(TGEDCOMCustomTag Source)
		{
			if (Source != null)
			{
				this.StringValue = Source.StringValue;

				int num = Source.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMTag tag = Source.GetTag(i);
						//TGEDCOMTag copy = (tag.ClassType() as TGEDCOMTag.MetaTGEDCOMTag).Create(this.Owner, this, "", "") as TGEDCOMTag;						
						TGEDCOMTag copy = (TGEDCOMTag)Activator.CreateInstance(tag.GetType(), new object[] { this.Owner, this, "", "" });
						copy.Name = tag.Name;
						copy.Assign(tag);
						this.InsertTag(copy);
						i++;
					}
					while (i != num);
				}
			}
		}

		public virtual void Clear()
		{
			if (this.FTags != null)
			{
				object fTags = this.FTags;
				VCLUtils.FreeAndNil(ref fTags);
				this.FTags = (fTags as TGEDCOMList);
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
			string S;
			if (BDSSystem.Pos("\\", SU) > 0)
			{
				S = BDSSystem.WStrCopy(SU, 1, BDSSystem.Pos("\\", SU) - 1);
			}
			else
			{
				S = SU;
			}

			TGEDCOMCustomTag O = this;
			TGEDCOMTag Result;
			while (true)
			{
				int Index;
				if (BDSSystem.WStrCmp(S, SU) == 0)
				{
					Index = StartIndex;
				}
				else
				{
					Index = 0;
				}
				while (Index < O.Count && BDSSystem.WStrCmp(O.GetTag(Index).Name, S) != 0)
				{
					Index++;
				}
				if (Index >= O.Count)
				{
					break;
				}
				Result = O.GetTag(Index);
				O = Result;
				if (BDSSystem.Pos("\\", SU) > 0)
				{
					SU = BDSSystem.WStrCopy(SU, BDSSystem.Pos("\\", SU) + 1, 2147483647);
					if (BDSSystem.Pos("\\", SU) > 0)
					{
						S = BDSSystem.WStrCopy(SU, 1, BDSSystem.Pos("\\", SU) - 1);
					}
					else
					{
						S = SU;
					}
				}
				else
				{
					SU = "";
				}
				if (SU == "")
				{
					return Result;
				}
			}
			Result = null;
			return Result;
		}

		public int GetTagIntegerValue([In] string ATag, int ADefault)
		{
			string S = this.GetTagStringValue(ATag);
			int Result;
			if (S == "")
			{
				Result = ADefault;
			}
			else
			{
				Result = VCLUtils.StrToIntDef(S, ADefault);
			}
			return Result;
		}

		public string GetTagStringValue([In] string ATag)
		{
			TGEDCOMTag Tag = this.FindTag(ATag, 0);
			string Result;
			if (Tag != null)
			{
				Result = Tag.StringValue;
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public TStrings GetTagStrings(TGEDCOMCustomTag ATag, ref TStrings AStrings)
		{
			if (AStrings == null)
			{
				AStrings = new TStringList();
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
				int I = 0;
				if (num >= I)
				{
					num++;
					do
					{
						TGEDCOMTag Tag = ATag.GetTag(I);
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
						I++;
					}
					while (I != num);
				}
			}
			return AStrings;
		}

		public int IndexOfTag(TGEDCOMTag ATag)
		{
			int Result;
			if (this.FTags != null)
			{
				Result = this.FTags.IndexOfObject(ATag);
			}
			else
			{
				Result = -1;
			}
			return Result;
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
					int Index = BDSSystem.Pos("\\", SU);
					string S;
					if (Index > 0)
					{
						S = BDSSystem.WStrCopy(SU, 1, Index - 1);
						SU = BDSSystem.WStrCopy(SU, Index + 1, 2147483647);
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

		public void SetTagStrings(TGEDCOMCustomTag ATag, TStrings Value)
		{
			if (ATag != null)
			{
				ATag.StringValue = "";
				int I = ATag.Count - 1;
				if (I >= 0)
				{
					do
					{
						if (BDSSystem.WStrCmp(ATag.GetTag(I).Name, "CONT") == 0 || BDSSystem.WStrCmp(ATag.GetTag(I).Name, "CONC") == 0)
						{
							ATag.Delete(I);
						}
						I--;
					}
					while (I != -1);
				}
				if (Value != null)
				{
					int num = Value.Count - 1;
					I = 0;
					if (num >= I)
					{
						num++;
						do
						{
							string S = Value[I];
							if (I == 0 && !(ATag is TGEDCOMRecord))
							{
								ATag.StringValue = BDSSystem.WStrCopy(S, 1, 248);
							}
							else
							{
								ATag.AddTag("CONT", BDSSystem.WStrCopy(S, 1, 248), null);
							}
							S = S.Remove(0, 248);
							while (((S != null) ? S.Length : 0) > 0)
							{
								ATag.AddTag("CONC", BDSSystem.WStrCopy(S, 1, 248), null);
								S = S.Remove(0, 248);
							}
							I++;
						}
						while (I != num);
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
				int I = ATag.Count - 1;
				if (I >= 0)
				{
					do
					{
						if (ATag.GetTag(I).Name == "CONT" || ATag.GetTag(I).Name == "CONC")
						{
							ATag.Delete(I);
						}
						I--;
					}
					while (I != -1);
				}

				int num = ((Value != null) ? Value.Length : 0) - 1;
				I = 0;
				if (num >= I)
				{
					num++;
					do
					{
						string S = Value[I];
						if (I == 0 && !(ATag is TGEDCOMRecord))
						{
							ATag.StringValue = BDSSystem.WStrCopy(S, 1, 248);
						}
						else
						{
							ATag.AddTag("CONT", BDSSystem.WStrCopy(S, 1, 248), null);
						}
						S = S.Remove(0, 248);
						while (((S != null) ? S.Length : 0) > 0)
						{
							ATag.AddTag("CONC", BDSSystem.WStrCopy(S, 1, 248), null);
							S = S.Remove(0, 248);
						}
						I++;
					}
					while (I != num);
				}
			}
		}

		public TGEDCOMTag TagClass([In] string ATag, Type AClass)
		{
			TGEDCOMTag Tag = this.FindTag(ATag, 0);
			TGEDCOMTag Result;
			if (Tag == null)
			{
				Result = this.AddTag(ATag, "", AClass);
			}
			else
			{
				/*if (!AClass.InstanceType().IsInstanceOfType(Tag))
				{
					throw new EGEDCOMException(string.Format("The tag {0} is of type {1}, but type {2} was expected", new object[]
					{
						ATag, 
						TGEDCOMObject.ClassName(BDSSystem.GetMetaFromObject(Tag) as TGEDCOMTag.MetaTGEDCOMTag), 
						TGEDCOMObject.ClassName(AClass)
					}));
				}*/
				//alert!!! restore code above!
				Result = Tag;
			}
			return Result;
		}

		public virtual void Pack()
		{
			if (this.FTags != null)
			{
				this.FTags.Pack();
			}
		}

		public virtual void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			if (this.FTags != null)
			{
				this.FTags.ReplaceXRefs(aMap);
			}
		}

		public virtual void ResetOwner(TGEDCOMObject AOwner)
		{
			this.FOwner = AOwner;
			if (this.FTags != null)
			{
				this.FTags.ResetOwner(AOwner);
			}
		}

		public void ResetParent(TGEDCOMObject AParent)
		{
			this.FParent = AParent;
		}

		static TGEDCOMCustomTag()
		{
			TGEDCOMCustomTag.TTagPropsRec[] array = new TGEDCOMCustomTag.TTagPropsRec[26];
			TGEDCOMCustomTag.TTagPropsRec[] arg_30_0_cp_0 = array;
			int arg_30_0_cp_1 = 0;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec;
			tTagPropsRec.Name = "";
			tTagPropsRec.EmptySkip = false;
			arg_30_0_cp_0[arg_30_0_cp_1] = tTagPropsRec;
			TGEDCOMCustomTag.TTagPropsRec[] arg_52_0_cp_0 = array;
			int arg_52_0_cp_1 = 1;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec2;
			tTagPropsRec2.Name = "ADDR";
			tTagPropsRec2.EmptySkip = true;
			arg_52_0_cp_0[arg_52_0_cp_1] = tTagPropsRec2;
			TGEDCOMCustomTag.TTagPropsRec[] arg_74_0_cp_0 = array;
			int arg_74_0_cp_1 = 2;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec3;
			tTagPropsRec3.Name = "AGNC";
			tTagPropsRec3.EmptySkip = true;
			arg_74_0_cp_0[arg_74_0_cp_1] = tTagPropsRec3;
			TGEDCOMCustomTag.TTagPropsRec[] arg_96_0_cp_0 = array;
			int arg_96_0_cp_1 = 3;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec4;
			tTagPropsRec4.Name = "AUTH";
			tTagPropsRec4.EmptySkip = true;
			arg_96_0_cp_0[arg_96_0_cp_1] = tTagPropsRec4;
			TGEDCOMCustomTag.TTagPropsRec[] arg_B9_0_cp_0 = array;
			int arg_B9_0_cp_1 = 4;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec5;
			tTagPropsRec5.Name = "CAUS";
			tTagPropsRec5.EmptySkip = true;
			arg_B9_0_cp_0[arg_B9_0_cp_1] = tTagPropsRec5;
			TGEDCOMCustomTag.TTagPropsRec[] arg_DC_0_cp_0 = array;
			int arg_DC_0_cp_1 = 5;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec6;
			tTagPropsRec6.Name = "CHAN";
			tTagPropsRec6.EmptySkip = true;
			arg_DC_0_cp_0[arg_DC_0_cp_1] = tTagPropsRec6;
			TGEDCOMCustomTag.TTagPropsRec[] arg_FF_0_cp_0 = array;
			int arg_FF_0_cp_1 = 6;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec7;
			tTagPropsRec7.Name = "CITY";
			tTagPropsRec7.EmptySkip = true;
			arg_FF_0_cp_0[arg_FF_0_cp_1] = tTagPropsRec7;
			TGEDCOMCustomTag.TTagPropsRec[] arg_122_0_cp_0 = array;
			int arg_122_0_cp_1 = 7;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec8;
			tTagPropsRec8.Name = "CTRY";
			tTagPropsRec8.EmptySkip = true;
			arg_122_0_cp_0[arg_122_0_cp_1] = tTagPropsRec8;
			TGEDCOMCustomTag.TTagPropsRec[] arg_145_0_cp_0 = array;
			int arg_145_0_cp_1 = 8;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec9;
			tTagPropsRec9.Name = "DATE";
			tTagPropsRec9.EmptySkip = true;
			arg_145_0_cp_0[arg_145_0_cp_1] = tTagPropsRec9;
			TGEDCOMCustomTag.TTagPropsRec[] arg_169_0_cp_0 = array;
			int arg_169_0_cp_1 = 9;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec10;
			tTagPropsRec10.Name = "PAGE";
			tTagPropsRec10.EmptySkip = true;
			arg_169_0_cp_0[arg_169_0_cp_1] = tTagPropsRec10;
			TGEDCOMCustomTag.TTagPropsRec[] arg_18D_0_cp_0 = array;
			int arg_18D_0_cp_1 = 10;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec11;
			tTagPropsRec11.Name = "PLAC";
			tTagPropsRec11.EmptySkip = true;
			arg_18D_0_cp_0[arg_18D_0_cp_1] = tTagPropsRec11;
			TGEDCOMCustomTag.TTagPropsRec[] arg_1B1_0_cp_0 = array;
			int arg_1B1_0_cp_1 = 11;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec12;
			tTagPropsRec12.Name = "POST";
			tTagPropsRec12.EmptySkip = true;
			arg_1B1_0_cp_0[arg_1B1_0_cp_1] = tTagPropsRec12;
			TGEDCOMCustomTag.TTagPropsRec[] arg_1D5_0_cp_0 = array;
			int arg_1D5_0_cp_1 = 12;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec13;
			tTagPropsRec13.Name = "PUBL";
			tTagPropsRec13.EmptySkip = true;
			arg_1D5_0_cp_0[arg_1D5_0_cp_1] = tTagPropsRec13;
			TGEDCOMCustomTag.TTagPropsRec[] arg_1F9_0_cp_0 = array;
			int arg_1F9_0_cp_1 = 13;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec14;
			tTagPropsRec14.Name = "RESN";
			tTagPropsRec14.EmptySkip = true;
			arg_1F9_0_cp_0[arg_1F9_0_cp_1] = tTagPropsRec14;
			TGEDCOMCustomTag.TTagPropsRec[] arg_21D_0_cp_0 = array;
			int arg_21D_0_cp_1 = 14;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec15;
			tTagPropsRec15.Name = "STAE";
			tTagPropsRec15.EmptySkip = true;
			arg_21D_0_cp_0[arg_21D_0_cp_1] = tTagPropsRec15;
			TGEDCOMCustomTag.TTagPropsRec[] arg_241_0_cp_0 = array;
			int arg_241_0_cp_1 = 15;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec16;
			tTagPropsRec16.Name = "TEXT";
			tTagPropsRec16.EmptySkip = true;
			arg_241_0_cp_0[arg_241_0_cp_1] = tTagPropsRec16;
			TGEDCOMCustomTag.TTagPropsRec[] arg_265_0_cp_0 = array;
			int arg_265_0_cp_1 = 16;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec17;
			tTagPropsRec17.Name = "TIME";
			tTagPropsRec17.EmptySkip = true;
			arg_265_0_cp_0[arg_265_0_cp_1] = tTagPropsRec17;
			TGEDCOMCustomTag.TTagPropsRec[] arg_289_0_cp_0 = array;
			int arg_289_0_cp_1 = 17;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec18;
			tTagPropsRec18.Name = "TYPE";
			tTagPropsRec18.EmptySkip = true;
			arg_289_0_cp_0[arg_289_0_cp_1] = tTagPropsRec18;
			TGEDCOMCustomTag.TTagPropsRec[] arg_2AD_0_cp_0 = array;
			int arg_2AD_0_cp_1 = 18;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec19;
			tTagPropsRec19.Name = "SUBM";
			tTagPropsRec19.EmptySkip = true;
			arg_2AD_0_cp_0[arg_2AD_0_cp_1] = tTagPropsRec19;
			TGEDCOMCustomTag.TTagPropsRec[] arg_2D1_0_cp_0 = array;
			int arg_2D1_0_cp_1 = 19;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec20;
			tTagPropsRec20.Name = "NPFX";
			tTagPropsRec20.EmptySkip = true;
			arg_2D1_0_cp_0[arg_2D1_0_cp_1] = tTagPropsRec20;
			TGEDCOMCustomTag.TTagPropsRec[] arg_2F5_0_cp_0 = array;
			int arg_2F5_0_cp_1 = 20;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec21;
			tTagPropsRec21.Name = "GIVN";
			tTagPropsRec21.EmptySkip = true;
			arg_2F5_0_cp_0[arg_2F5_0_cp_1] = tTagPropsRec21;
			TGEDCOMCustomTag.TTagPropsRec[] arg_319_0_cp_0 = array;
			int arg_319_0_cp_1 = 21;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec22;
			tTagPropsRec22.Name = "NICK";
			tTagPropsRec22.EmptySkip = true;
			arg_319_0_cp_0[arg_319_0_cp_1] = tTagPropsRec22;
			TGEDCOMCustomTag.TTagPropsRec[] arg_33D_0_cp_0 = array;
			int arg_33D_0_cp_1 = 22;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec23;
			tTagPropsRec23.Name = "SPFX";
			tTagPropsRec23.EmptySkip = true;
			arg_33D_0_cp_0[arg_33D_0_cp_1] = tTagPropsRec23;
			TGEDCOMCustomTag.TTagPropsRec[] arg_361_0_cp_0 = array;
			int arg_361_0_cp_1 = 23;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec24;
			tTagPropsRec24.Name = "SURN";
			tTagPropsRec24.EmptySkip = true;
			arg_361_0_cp_0[arg_361_0_cp_1] = tTagPropsRec24;
			TGEDCOMCustomTag.TTagPropsRec[] arg_385_0_cp_0 = array;
			int arg_385_0_cp_1 = 24;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec25;
			tTagPropsRec25.Name = "NSFX";
			tTagPropsRec25.EmptySkip = true;
			arg_385_0_cp_0[arg_385_0_cp_1] = tTagPropsRec25;
			TGEDCOMCustomTag.TTagPropsRec[] arg_3A9_0_cp_0 = array;
			int arg_3A9_0_cp_1 = 25;
			TGEDCOMCustomTag.TTagPropsRec tTagPropsRec26;
			tTagPropsRec26.Name = "_LOC";
			tTagPropsRec26.EmptySkip = true;
			arg_3A9_0_cp_0[arg_3A9_0_cp_1] = tTagPropsRec26;
			TGEDCOMCustomTag.TagBase = array;
		}
	}
}
