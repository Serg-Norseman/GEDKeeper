using System;
using System.IO;
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

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
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

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this.FDetail.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this.FDetail.SaveToStream(AStream);
		}

		public float IsMatch(TGEDCOMCustomEvent ev)
		{
			float match = 0F;

			// match date
			float dateMatch = 0;

			TGEDCOMDateValue dtVal = FDetail.Date;
			TGEDCOMDateValue dtVal2 = ev.FDetail.Date;

			if (dtVal == null && dtVal2 == null) {
				dateMatch = 100.0F;
			} else if (dtVal != null && dtVal2 != null) {
				dateMatch = dtVal.IsMatch(dtVal2);
			}

			// match location
			float locMatch = 0;
			if (FDetail.Place == null && ev.FDetail.Place == null)
			{
				locMatch = 100.0F;
			}
			else if (FDetail.Place != null && ev.FDetail.Place != null)
			{
				if (FDetail.Place.StringValue == ev.FDetail.Place.StringValue)
				{
					locMatch = 100.0F;
				}
			}

			match = (dateMatch + locMatch) / 2.0F;

			return match;
		}

		public TGEDCOMCustomEvent(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
