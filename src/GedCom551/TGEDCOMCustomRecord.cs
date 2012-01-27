using System;
using System.IO;
using System.Runtime.InteropServices;

using GKSys;

namespace GedCom551
{
	public class TGEDCOMCustomRecord : TGEDCOMCustomTag
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

		public virtual TGEDCOMTag AddSubTag(TGEDCOMCustomTag AParent, [In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result = null;

			try
			{
				TGEDCOMTag tag;
				if (ATagConstructor != null) {
					tag = (TGEDCOMTag)ATagConstructor(base.Owner, AParent, ATag, AValue);
				} else {
					tag = base.CreateGEDCOMTag(base.Owner, AParent, ATag, AValue);
				}
				Result = AParent.InsertTag(tag);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGEDCOMCustomRecord.AddSubTag(): " + E.Message);
			}

			return Result;
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag tag;
			if (ATagConstructor != null) {
				tag = (TGEDCOMTag)ATagConstructor(base.Owner, this, ATag, AValue);
			} else {
				tag = base.CreateGEDCOMTag(base.Owner, this, ATag, AValue);
			}
			base.InsertTag(tag);
			return tag;
		}

		public TGEDCOMCustomRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
