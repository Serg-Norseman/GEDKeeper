using System;
using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMCommunicationRecord : TGEDCOMRecord
	{
		public static readonly string[] CommunicationTags = new string[] { "FROM", "TO" };

		public TGEDCOMDateExact Date
		{
			get { return base.TagClass("DATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public string CommName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public TCommunicationType CommunicationType
		{
			get { return GEDCOMUtils.GetCommunicationTypeVal(base.GetTagStringValue("TYPE").Trim().ToLower()); }
			set { base.SetTagStringValue("TYPE", GEDCOMUtils.GetCommunicationTypeStr(value)); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtCommunication;
			this.FName = "_COMM";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, null);
			}
			else if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
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
				aCorresponder = (this.FOwner.XRefIndex_Find(GEDCOMUtils.CleanXRef(cr_tag.StringValue)) as TGEDCOMIndividualRecord);
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
				this.AddTag(TGEDCOMCommunicationRecord.CommunicationTags[(int)aDir], GEDCOMUtils.EncloseXRef(aCorresponder.XRef), null);
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
