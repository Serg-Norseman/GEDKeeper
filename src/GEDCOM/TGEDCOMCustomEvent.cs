using System;
using System.IO;

namespace GedCom551
{
	public abstract class TGEDCOMCustomEvent : TGEDCOMTag
	{
		private TGEDCOMEventDetail fDetail;

		public TGEDCOMEventDetail Detail
		{
			get { return this.fDetail; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDetail = new TGEDCOMEventDetail(base.Owner, this, "", "");
			this.fDetail.SetLevel(base.Level);
		}

        protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fDetail.Dispose();
			}
            base.Dispose(disposing);
        }

        public override void Assign(TGEDCOMTag source)
		{
			base.Assign(source);
			if (source is TGEDCOMCustomEvent)
			{
				this.fDetail.Assign((source as TGEDCOMCustomEvent).Detail);
			}
		}

		public override void Pack()
		{
			base.Pack();
			this.fDetail.Pack();
		}

		public override void ReplaceXRefs(XRefReplacer map)
		{
			base.ReplaceXRefs(map);
			this.fDetail.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fDetail.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fDetail.SaveToStream(stream);
		}

		public override float IsMatch(TGEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			TGEDCOMCustomEvent ev = (TGEDCOMCustomEvent)tag;

			float match = 0.0f;

			// match date
			float dateMatch = 0.0f;
			TGEDCOMDateValue dtVal = this.fDetail.Date;
			TGEDCOMDateValue dtVal2 = ev.fDetail.Date;

			if ((dtVal == null && dtVal2 == null)) {
				dateMatch = 100.0f;
			} else if (dtVal != null && dtVal2 != null) {
				dateMatch = dtVal.IsMatch(dtVal2, matchParams);
			}

			// match location - late code-on by option implementation
			/*float locMatch = 0.0f;
			if (this.fDetail.Place == null && ev.fDetail.Place == null)
			{
				locMatch = 100.0f;
			}
			else if (this.fDetail.Place != null && ev.fDetail.Place != null)
			{
				if (this.fDetail.Place.StringValue == ev.fDetail.Place.StringValue)
				{
					locMatch = 100.0f;
				}
			}*/

			match = (dateMatch); /* + locMatch) / 2.0f;*/
			return match;
		}

	    protected TGEDCOMCustomEvent(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
