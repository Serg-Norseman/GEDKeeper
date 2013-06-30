using System;
using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMCommunicationRecord : TGEDCOMRecord
	{
		public static readonly string[] CommunicationTags = new string[] { "FROM", "TO" };

		public TGEDCOMDateExact Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public string CommName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public TCommunicationType CommunicationType
		{
			get { return GetCommunicationTypeVal(base.GetTagStringValue("TYPE").Trim().ToLower()); }
			set { base.SetTagStringValue("TYPE", GetCommunicationTypeStr(value)); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtCommunication;
			this.FName = "_COMM";
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "NAME")
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public void GetCorresponder(ref TCommunicationDir aDir, ref TGEDCOMIndividualRecord aCorresponder)
		{
			aCorresponder = null;
            TGEDCOMTag cr_tag = base.FindTag("FROM", 0);
			if (cr_tag == null)
			{
				cr_tag = base.FindTag("TO", 0);
			}
			if (cr_tag != null)
			{
				aCorresponder = (this.FOwner.XRefIndex_Find(TGEDCOMObject.CleanXRef(cr_tag.StringValue)) as TGEDCOMIndividualRecord);
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

		public TGEDCOMCommunicationRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMCommunicationRecord(owner, parent, tagName, tagValue);
		}
	}
}
