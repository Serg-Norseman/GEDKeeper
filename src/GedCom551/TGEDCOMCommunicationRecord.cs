using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMCommunicationRecord : TGEDCOMRecord
	{
		public static readonly string[] CommunicationTags = new string[] { "FROM", "TO" };

		public TGEDCOMDateExact Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact; }
		}

		public string CommName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public TCommunicationType CommunicationType
		{
			get { return this.GetCommunicationType(); }
			set { this.SetCommunicationType(value); }
		}

		/*public new <TGEDCOMNotes> Notes
		{
			get { return base.GetNote(Index); }
		}*/

		private TCommunicationType GetCommunicationType()
		{
			string S = base.GetTagStringValue("TYPE").Trim().ToLower();
			TCommunicationType Result;
			if (S == "call")
			{
				Result = TCommunicationType.ctCall;
			}
			else
			{
				if (S == "email")
				{
					Result = TCommunicationType.ctEMail;
				}
				else
				{
					if (S == "fax")
					{
						Result = TCommunicationType.ctFax;
					}
					else
					{
						if (S == "letter")
						{
							Result = TCommunicationType.ctLetter;
						}
						else
						{
							if (S == "tape")
							{
								Result = TCommunicationType.ctTape;
							}
							else
							{
								if (S == "visit")
								{
									Result = TCommunicationType.ctVisit;
								}
								else
								{
									Result = TCommunicationType.ctVisit;
								}
							}
						}
					}
				}
			}
			return Result;
		}

		private void SetCommunicationType([In] TCommunicationType Value)
		{
			string S = "";
			switch (Value)
			{
				case TCommunicationType.ctCall:
				{
					S = "call";
					break;
				}
				case TCommunicationType.ctEMail:
				{
					S = "email";
					break;
				}
				case TCommunicationType.ctFax:
				{
					S = "fax";
					break;
				}
				case TCommunicationType.ctLetter:
				{
					S = "letter";
					break;
				}
				case TCommunicationType.ctTape:
				{
					S = "tape";
					break;
				}
				case TCommunicationType.ctVisit:
				{
					S = "visit";
					break;
				}
			}
			base.SetTagStringValue("TYPE", S);
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecordType.rtCommunication;
			this.FName = "_COMM";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "NAME")
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else
			{
				if (ATag == "DATE")
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

		public void GetCorresponder(ref TCommunicationDir aDir, ref TGEDCOMIndividualRecord aCorresponder)
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
				if (cr_tag.Name == "FROM")
				{
					aDir = TCommunicationDir.cdFrom;
				}
				else
				{
					if (cr_tag.Name == "TO")
					{
						aDir = TCommunicationDir.cdTo;
					}
				}
			}
		}

		public void SetCorresponder(TCommunicationDir aDir, TGEDCOMIndividualRecord aCorresponder)
		{
			base.DeleteTag("FROM");
			base.DeleteTag("TO");
			if (aCorresponder != null)
			{
				this.AddTag(TGEDCOMCommunicationRecord.CommunicationTags[(int)aDir], TGEDCOMObject.EncloseXRef(aCorresponder.XRef), null);
			}
		}

		public TGEDCOMCommunicationRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
