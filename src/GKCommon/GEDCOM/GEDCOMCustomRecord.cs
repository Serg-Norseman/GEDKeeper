using System.IO;

namespace GKCommon.GEDCOM
{
	public abstract class GEDCOMCustomRecord : GEDCOMTag
	{
		private string fXRef;

		public string XRef
		{
			get { return this.fXRef; }
			set {
				string oldXRef = this.fXRef;
				this.fXRef = value;
				if (this.Owner != null) {
					this.Owner.SetXRef(oldXRef, this);
				}
			}
		}

		protected override void SaveValueToStream(StreamWriter stream)
		{
			string str = base.Level.ToString();

			if (!string.IsNullOrEmpty(this.fXRef))
			{
				str = str + " " + "@" + this.fXRef + "@";
			}
			str = str + " " + base.Name;

			if (base.StringValue != "")
			{
				str = str + " " + base.StringValue;
			}

			stream.WriteLine(str);
		}

	    protected GEDCOMCustomRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
