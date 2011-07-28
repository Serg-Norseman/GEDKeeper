using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMCustomEvent : TGEDCOMTag
	{

		private TGEDCOMEventDetail FDetail;

		public TGEDCOMEventDetail Detail
		{
			get { return this.FDetail; }
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDetail = new TGEDCOMEventDetail(base.Owner, this, "", "");
			this.FDetail.SetLevel(base.Level);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FDetail.Free();
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Assign(TGEDCOMCustomTag Source)
		{
			base.Assign(Source);
			if (Source is TGEDCOMCustomEvent)
			{
				this.FDetail.Assign(((TGEDCOMCustomEvent)Source).Detail);
			}
		}
		public override void Pack()
		{
			base.Pack();
			this.FDetail.Pack();
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.FDetail.ReplaceXRefs(aMap);
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			this.FDetail.ResetOwner(AOwner);
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this.FDetail.SaveToStream(AStream);
		}

		public TGEDCOMCustomEvent(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
