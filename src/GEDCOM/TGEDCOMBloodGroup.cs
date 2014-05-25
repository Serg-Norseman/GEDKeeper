using System;

namespace GedCom551
{
	public sealed class TGEDCOMBloodGroup : TGEDCOMIndividualAttribute
	{
		private BloodGroup fBloodGroup;

		public BloodGroup Value
		{
			get { return this.fBloodGroup; }
			set { this.fBloodGroup = value; }
		}

		protected override string GetStringValue()
		{
			string res;
			switch (this.fBloodGroup) {
				case BloodGroup.APositive:
					res = "A+";
					break;
				case BloodGroup.ANegative:
					res = "A-";
					break;
				case BloodGroup.BPositive:
					res = "B+";
					break;
				case BloodGroup.BNegative:
					res = "B-";
					break;
				case BloodGroup.ABPositive:
					res = "AB+";
					break;
				case BloodGroup.ABNegative:
					res = "AB-";
					break;
				case BloodGroup.OPositive:
					res = "O+";
					break;
				case BloodGroup.ONegative:
					res = "O-";
					break;
				default:
					res = "";
					break;
			}
			return res;
		}

		protected override void SetStringValue(string S)
		{
			if (!string.IsNullOrEmpty(S))
			{
				if (S == "A+") {
					this.fBloodGroup = BloodGroup.APositive;
				} else if (S == "A-") {
					this.fBloodGroup = BloodGroup.ANegative;
				} else if (S == "B+") {
					this.fBloodGroup = BloodGroup.BPositive;
				} else if (S == "B-") {
					this.fBloodGroup = BloodGroup.BNegative;
				} else if (S == "AB+") {
					this.fBloodGroup = BloodGroup.ABPositive;
				} else if (S == "AB-") {
					this.fBloodGroup = BloodGroup.ABNegative;
				} else if (S == "O+") {
					this.fBloodGroup = BloodGroup.OPositive;
				} else if (S == "O-") {
					this.fBloodGroup = BloodGroup.ONegative;
				} else {
					this.fBloodGroup = BloodGroup.Unknown;
				}
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "_BGRO";
		}

		public TGEDCOMBloodGroup(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMBloodGroup(owner, parent, tagName, tagValue);
		}
	}
}
