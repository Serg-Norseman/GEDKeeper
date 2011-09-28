using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

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

		private void SetXRef([In] string AXRef)
		{
			this.FXRef = AXRef;
			if (this is TGEDCOMRecord && this.FOwner != null)
			{
				(this.FOwner as TGEDCOMTree).SetXRef(this as TGEDCOMRecord, this.FXRef);
			}
		}

		public virtual TGEDCOMTag AddSubTag(TGEDCOMCustomTag AParent, [In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result = null;

			try
			{
				if (AClass == null)
				{
					Result = AParent.InsertTag(base.CreateGEDCOMTag(base.Owner, AParent, ATag, AValue));
				}
				else
				{
					TGEDCOMTag tag = (TGEDCOMTag)Activator.CreateInstance(AClass, new object[] { base.Owner, AParent, ATag, AValue });
					//AClass.Create(base.Owner, AParent, ATag, AValue) as TGEDCOMTag;
					Result = AParent.InsertTag(tag);
				}
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (AClass == null)
			{
				Result = base.InsertTag(base.CreateGEDCOMTag(base.Owner, this, ATag, AValue));
			}
			else
			{
				TGEDCOMTag tag = (TGEDCOMTag)Activator.CreateInstance(AClass, new object[] { base.Owner, this, ATag, AValue });
				//AClass.Create(base.Owner, this, ATag, AValue) as TGEDCOMTag
				Result = base.InsertTag(tag);
			}
			return Result;
		}

		public TGEDCOMCustomRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
