using GKSys;
using System;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMCustomRecord : TGEDCOMCustomTag
	{

		internal string FXRef;
		protected internal string XRef
		{
			get
			{
				return this.FXRef;
			}
			set
			{
				this.SetXRef(value);
			}
		}

		internal void SetXRef([In] string AXRef)
		{
			this.FXRef = AXRef;
			if (this is TGEDCOMRecord && this.FOwner != null)
			{
				(this.FOwner as TGEDCOMTree).SetXRef(this as TGEDCOMRecord, this.XRef);
			}
		}

		protected internal virtual TGEDCOMTag AddSubTag(TGEDCOMCustomTag AParent, [In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
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
			return Result;
		}

		protected internal override void SaveValueToStream(StreamWriter AStream)
		{
			string S = base.Level.ToString();
			if (BDSSystem.WStrCmp(this.XRef, "") != 0)
			{
				S = string.Concat(new string[]
				{
					S, 
					" ", 
					"@", 
					this.XRef, 
					"@"
				});
			}
			S = S + " " + base.Name;
			if (BDSSystem.WStrCmp(base.StringValue, "") != 0)
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
