using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMSubmissionRecord : TGEDCOMRecord
	{
		public enum TGEDCOMOrdinanceProcessFlag : byte { opNone, opYes, opNo }

		public string AutomatedRecordID
		{
			get { return this.GetStringTag(3); }
			set { this.SetStringTag(3, value); }
		}

		public string FamilyFileName
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		public int GenerationsOfAncestors
		{
			get { return this.GetIntegerTag(1); }
			set { this.SetIntegerTag(1, value); }
		}

		public int GenerationsOfDescendants
		{
			get { return this.GetIntegerTag(2); }
			set { this.SetIntegerTag(2, value); }
		}

		public TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag OrdinanceProcessFlag
		{
			get { return this.GetOrdinanceProcessFlag(); }
			set { this.SetOrdinanceProcessFlag(value); }
		}

		public TGEDCOMPointer Submitter
		{
			get { return this.GetSubmitter(); }
		}

		public string TempleCode
		{
			get { return this.GetStringTag(2); }
			set { this.SetStringTag(2, value); }
		}

		internal TGEDCOMPointer GetSubmitter()
		{
			return base.TagClass("SUBM", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index != 2)
				{
					if (Index == 3)
					{
						Result = base.GetTagStringValue("RIN");
					}
				}
				else
				{
					Result = base.GetTagStringValue("TEMP");
				}
			}
			else
			{
				Result = base.GetTagStringValue("FAMF");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index != 2)
				{
					if (Index == 3)
					{
						base.SetTagStringValue("RIN", Value);
					}
				}
				else
				{
					base.SetTagStringValue("TEMP", Value);
				}
			}
			else
			{
				base.SetTagStringValue("FAMF", Value);
			}
		}
		internal int GetIntegerTag(int Index)
		{
			int Result;
			if (Index != 1)
			{
				if (Index != 2)
				{
					Result = 0;
				}
				else
				{
					Result = base.GetTagIntegerValue("DESC", 0);
				}
			}
			else
			{
				Result = base.GetTagIntegerValue("ANCE", 0);
			}
			return Result;
		}
		internal void SetIntegerTag(int Index, int Value)
		{
			if (Index != 1)
			{
				if (Index == 2)
				{
					base.SetTagIntegerValue("DESC", Value);
				}
			}
			else
			{
				base.SetTagIntegerValue("ANCE", Value);
			}
		}
		internal TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlag()
		{
			string S = base.GetTagStringValue("ORDI").Trim().ToUpper();
			TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag Result;
			if (BDSSystem.WStrCmp(S, "YES") == 0)
			{
				Result = TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag.opYes;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "NO") == 0)
				{
					Result = TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag.opNo;
				}
				else
				{
					Result = TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag.opNone;
				}
			}
			return Result;
		}
		internal void SetOrdinanceProcessFlag([In] TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag Value)
		{
			string S;
			if (Value != TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag.opNone)
			{
				if (Value != TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag.opYes)
				{
					if (Value != TGEDCOMSubmissionRecord.TGEDCOMOrdinanceProcessFlag.opNo)
					{
						S = "";
					}
					else
					{
						S = "no";
					}
				}
				else
				{
					S = "yes";
				}
			}
			else
			{
				S = "";
			}
			base.SetTagStringValue("ORDI", S);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtSubmission;
			this.FName = "SUBN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "SUBM") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public TGEDCOMSubmissionRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
