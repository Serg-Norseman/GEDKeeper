using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMSubmissionRecord : TGEDCOMRecord
	{
		public string FamilyFileName
		{
			get { return base.GetTagStringValue("FAMF"); }
			set { base.SetTagStringValue("FAMF", value); }
		}

		public string TempleCode
		{
			get { return base.GetTagStringValue("TEMP"); }
			set { base.SetTagStringValue("TEMP", value); }
		}

		public int GenerationsOfAncestors
		{
			get { return base.GetTagIntegerValue("ANCE", 0); }
			set { base.SetTagIntegerValue("ANCE", value); }
		}

		public int GenerationsOfDescendants
		{
			get { return base.GetTagIntegerValue("DESC", 0); }
			set { base.SetTagIntegerValue("DESC", value); }
		}

		public TGEDCOMOrdinanceProcessFlag OrdinanceProcessFlag
		{
			get { return this.GetOrdinanceProcessFlag(); }
			set { this.SetOrdinanceProcessFlag(value); }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", typeof(TGEDCOMPointer)) as TGEDCOMPointer; }
		}

		private TGEDCOMOrdinanceProcessFlag GetOrdinanceProcessFlag()
		{
			string S = base.GetTagStringValue("ORDI").Trim().ToUpper();

			TGEDCOMOrdinanceProcessFlag Result;
			if (S == "YES")
			{
				Result = TGEDCOMOrdinanceProcessFlag.opYes;
			}
			else
			{
				if (S == "NO")
				{
					Result = TGEDCOMOrdinanceProcessFlag.opNo;
				}
				else
				{
					Result = TGEDCOMOrdinanceProcessFlag.opNone;
				}
			}
			return Result;
		}

		private void SetOrdinanceProcessFlag([In] TGEDCOMOrdinanceProcessFlag Value)
		{
			string S;
			if (Value != TGEDCOMOrdinanceProcessFlag.opNone)
			{
				if (Value != TGEDCOMOrdinanceProcessFlag.opYes)
				{
					if (Value != TGEDCOMOrdinanceProcessFlag.opNo)
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

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecordType.rtSubmission;
			this.FName = "SUBN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "SUBM")
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
