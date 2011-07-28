using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMCommunicationRecord : TGEDCOMRecord
	{

		public enum TCommunicationType : byte
		{
			ctCall,
			ctEMail,
			ctFax,
			ctLetter,
			ctTape,
			ctVisit
		}
		public enum TCommunicationDir : byte
		{
			cdFrom,
			cdTo
		}
		public static readonly string[] CommunicationTags;
		[Browsable(false)]
		public TGEDCOMDateExact Date
		{
			get
			{
				return this.GetDate();
			}
		}
		[Browsable(false)]
		public new string Name
		{
			get
			{
				return this.GetName();
			}
			set
			{
				this.SetName(value);
			}
		}
		[Browsable(false)]
		public TGEDCOMCommunicationRecord.TCommunicationType CommunicationType
		{
			get
			{
				return this.GetCommunicationType();
			}
			set
			{
				this.SetCommunicationType(value);
			}
		}

		/*[Browsable(false)]
		public new TGEDCOMNotes Notes
		{
			get
			{
				return base.GetNote(Index);
			}
		}
		[Browsable(false)]
		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		internal string GetName()
		{
			return base.GetTagStringValue("NAME");
		}
		internal void SetName([In] string Value)
		{
			base.SetTagStringValue("NAME", Value);
		}
		internal TGEDCOMDateExact GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}
		internal TGEDCOMCommunicationRecord.TCommunicationType GetCommunicationType()
		{
			string S = base.GetTagStringValue("TYPE").Trim().ToLower();
			TGEDCOMCommunicationRecord.TCommunicationType Result;
			if (BDSSystem.WStrCmp(S, "call") == 0)
			{
				Result = TGEDCOMCommunicationRecord.TCommunicationType.ctCall;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "email") == 0)
				{
					Result = TGEDCOMCommunicationRecord.TCommunicationType.ctEMail;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "fax") == 0)
					{
						Result = TGEDCOMCommunicationRecord.TCommunicationType.ctFax;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "letter") == 0)
						{
							Result = TGEDCOMCommunicationRecord.TCommunicationType.ctLetter;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "tape") == 0)
							{
								Result = TGEDCOMCommunicationRecord.TCommunicationType.ctTape;
							}
							else
							{
								if (BDSSystem.WStrCmp(S, "visit") == 0)
								{
									Result = TGEDCOMCommunicationRecord.TCommunicationType.ctVisit;
								}
								else
								{
									Result = TGEDCOMCommunicationRecord.TCommunicationType.ctVisit;
								}
							}
						}
					}
				}
			}
			return Result;
		}
		internal void SetCommunicationType([In] TGEDCOMCommunicationRecord.TCommunicationType Value)
		{
			string S = "";
			switch (Value)
			{
				case TGEDCOMCommunicationRecord.TCommunicationType.ctCall:
				{
					S = "call";
					break;
				}
				case TGEDCOMCommunicationRecord.TCommunicationType.ctEMail:
				{
					S = "email";
					break;
				}
				case TGEDCOMCommunicationRecord.TCommunicationType.ctFax:
				{
					S = "fax";
					break;
				}
				case TGEDCOMCommunicationRecord.TCommunicationType.ctLetter:
				{
					S = "letter";
					break;
				}
				case TGEDCOMCommunicationRecord.TCommunicationType.ctTape:
				{
					S = "tape";
					break;
				}
				case TGEDCOMCommunicationRecord.TCommunicationType.ctVisit:
				{
					S = "visit";
					break;
				}
			}
			base.SetTagStringValue("TYPE", S);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtCommunication;
			this.FName = "_COMM";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "NAME") == 0)
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "DATE") == 0)
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}
		public void GetCorresponder(ref TGEDCOMCommunicationRecord.TCommunicationDir aDir, ref TGEDCOMIndividualRecord aCorresponder)
		{
			aCorresponder = null;
			TGEDCOMCustomTag cr_tag = base.FindTag("FROM", 0);
			if (cr_tag == null)
			{
				cr_tag = base.FindTag("TO", 0);
			}
			if (cr_tag != null)
			{
				aCorresponder = ((this.FOwner as TGEDCOMTree).XRefIndex_Find(TGEDCOMObject.CleanXRef(cr_tag.StringValue)) as TGEDCOMIndividualRecord);
				if (BDSSystem.WStrCmp(cr_tag.Name, "FROM") == 0)
				{
					aDir = TGEDCOMCommunicationRecord.TCommunicationDir.cdFrom;
				}
				else
				{
					if (BDSSystem.WStrCmp(cr_tag.Name, "TO") == 0)
					{
						aDir = TGEDCOMCommunicationRecord.TCommunicationDir.cdTo;
					}
				}
			}
		}
		public void SetCorresponder(TGEDCOMCommunicationRecord.TCommunicationDir aDir, TGEDCOMIndividualRecord aCorresponder)
		{
			base.DeleteTag("FROM");
			base.DeleteTag("TO");
			if (aCorresponder != null)
			{
				this.AddTag(TGEDCOMCommunicationRecord.CommunicationTags[(int)aDir], TGEDCOMObject.EncloseXRef(aCorresponder.XRef), null);
			}
		}

		static TGEDCOMCommunicationRecord()
		{
			TGEDCOMCommunicationRecord.CommunicationTags = new string[]
			{
				"FROM", 
				"TO"
			};
		}

		public TGEDCOMCommunicationRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
