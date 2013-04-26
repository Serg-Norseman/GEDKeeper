using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public class TGEDCOMCustomRecord : TGEDCOMTag
	{
		protected string FXRef;

		public string XRef
		{
			get { return this.FXRef; }
			set { this.SetXRef(value); }
		}

		private void SetXRef([In] string newXRef)
		{
			string oldXRef = this.FXRef;
			this.FXRef = newXRef;
			if (this is TGEDCOMCustomRecord && this.FOwner != null)
			{
				this.FOwner.SetXRef(oldXRef, this);
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			return base.InternalCreateTag(this, ATag, AValue, ATagConstructor);
		}

		public virtual TGEDCOMTag AddSubTag(TGEDCOMTag AParent, [In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			return base.InternalCreateTag(AParent, ATag, AValue, ATagConstructor);
		}

		protected override void SaveValueToStream(StreamWriter AStream)
		{
			string S = base.Level.ToString();

			if (this.XRef != null && this.XRef != "")
			{
				S = S + " " + "@" + this.XRef + "@";
			}
			S = S + " " + base.Name;

			if (base.StringValue != "")
			{
				S = S + " " + base.StringValue;
			}

			AStream.WriteLine(S);
		}

		public TGEDCOMCustomRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
