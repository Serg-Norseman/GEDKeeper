using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMChildToFamilyLink : TGEDCOMPointerWithNotes
	{
		public TGEDCOMChildLinkageStatus ChildLinkageStatus
		{
			get { return GetChildLinkageStatusVal(base.GetTagStringValue("STAT").Trim().ToLower()); }
			set { base.SetTagStringValue("STAT", GetChildLinkageStatusStr(value)); }
		}

		public TGEDCOMPedigreeLinkageType PedigreeLinkageType
		{
			get { return GetPedigreeLinkageTypeVal(base.GetTagStringValue("PEDI").Trim().ToLower()); }
			set { base.SetTagStringValue("PEDI", GetPedigreeLinkageTypeStr(value)); }
		}

		public TGEDCOMFamilyRecord Family
		{
			get { return base.GetValue() as TGEDCOMFamilyRecord; }
			set { base.SetValue(value); }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FAMC";
		}

		public TGEDCOMChildToFamilyLink(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMChildToFamilyLink(AOwner, AParent, AName, AValue);
		}
	}
}
