using System.IO;

namespace GKCommon.GEDCOM
{
	public abstract class GEDCOMCustomEvent : GEDCOMTag
	{
		private GEDCOMEventDetail fDetail;

		public GEDCOMEventDetail Detail
		{
			get { return this.fDetail; }
		}

	    protected GEDCOMCustomEvent(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fDetail = new GEDCOMEventDetail(base.Owner, this, "", "");
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

        public override void Assign(GEDCOMTag source)
		{
			base.Assign(source);

			if (source is GEDCOMCustomEvent)
			{
				this.fDetail.Assign((source as GEDCOMCustomEvent).Detail);
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

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fDetail.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fDetail.SaveToStream(stream);
		}

		public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			GEDCOMCustomEvent ev = (GEDCOMCustomEvent)tag;

			float match = 0.0f;

			// match date
			float dateMatch = 0.0f;
			GEDCOMDateValue dtVal = this.fDetail.Date;
			GEDCOMDateValue dtVal2 = ev.fDetail.Date;

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
	}
}
