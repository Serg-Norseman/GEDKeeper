using System.IO;

namespace GKCommon.GEDCOM
{
	public abstract class GEDCOMCustomRecord : GEDCOMTag
	{
		protected string FXRef;

		public string XRef
		{
			get { return this.FXRef; }
			set {
				string oldXRef = this.FXRef;
				this.FXRef = value;
				if (this.fOwner != null) {
					this.fOwner.SetXRef(oldXRef, this);
				}
			}
		}

		protected override void SaveValueToStream(StreamWriter stream)
		{
			string S = base.Level.ToString();

			if (!string.IsNullOrEmpty(this.FXRef))
			{
				S = S + " " + "@" + this.FXRef + "@";
			}
			S = S + " " + base.Name;

			if (base.StringValue != "")
			{
				S = S + " " + base.StringValue;
			}

			stream.WriteLine(S);
		}

		public GEDCOMCustomRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
