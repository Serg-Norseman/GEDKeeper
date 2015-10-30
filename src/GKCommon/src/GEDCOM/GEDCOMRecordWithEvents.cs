using System;

namespace GKCommon.GEDCOM
{
	public abstract class GEDCOMRecordWithEvents : GEDCOMRecord, IGEDCOMRecordWithEvents
	{
		private GEDCOMList<GEDCOMCustomEvent> fEvents;

		public GEDCOMList<GEDCOMCustomEvent> Events
		{
			get { return this.fEvents; }
		}
		
		public GEDCOMRecordWithEvents(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fEvents = new GEDCOMList<GEDCOMCustomEvent>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				this.fEvents.Dispose();
			}
			base.Dispose(disposing);
		}

		public override void Clear()
		{
			base.Clear();
			this.fEvents.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && (this.fEvents.Count == 0);
		}

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
		{
            base.MoveTo(targetRecord, clearDest);

			GEDCOMRecordWithEvents target = targetRecord as GEDCOMRecordWithEvents;

			while (this.fEvents.Count > 0)
			{
                GEDCOMCustomEvent obj = this.fEvents.Extract(0);
                obj.ResetParent(target);
				target.AddEvent(obj);
			}
		}

		public override void Pack()
		{
			base.Pack();
			this.fEvents.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fEvents.ReplaceXRefs(map);
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fEvents.ResetOwner(newOwner);
		}

		public GEDCOMCustomEvent FindEvent(string eventName)
		{
			GEDCOMCustomEvent result = null;

			int num = this.fEvents.Count;
			for (int i = 0; i < num; i++)
			{
				GEDCOMCustomEvent evt = this.fEvents[i];

				if (evt.Name == eventName) {
					result = evt;
					break;
				}
			}

			return result;
		}

		public abstract GEDCOMCustomEvent AddEvent(GEDCOMCustomEvent evt);
		
		#region Auxiliary

		static float[] CA_Values = new float[] { 0.25f, 0.5f, 0.75f, 1.0f };

		public float GetCertaintyAssessment()
		{
			float result = 0;
			float wsum = 0;

			int num1 = this.fEvents.Count;
			for (int i = 0; i < num1; i++) {
				GEDCOMCustomEvent evt = this.fEvents[i];

				int num2 = evt.Detail.SourceCitations.Count;
				for (int k = 0; k < num2; k++) {
					GEDCOMSourceCitation cit = evt.Detail.SourceCitations[k];

					int ca = cit.CertaintyAssessment;
					int weight = (ca + 1);
					
					result += (CA_Values[ca] * weight);
					wsum += weight;
				}
			}
			
			int num3 = this.SourceCitations.Count;
			for (int i = 0; i < num3; i++) {
				GEDCOMSourceCitation cit = this.SourceCitations[i];

				int ca = cit.CertaintyAssessment;
				int weight = (ca + 1);
				
				result += (CA_Values[ca] * weight);
				wsum += weight;
			}

			if (wsum != 0.0f) {
				result /= wsum;
			} else {
				result = 0.0f;
			}
			
			return result;
		}

		#endregion
	}
}
