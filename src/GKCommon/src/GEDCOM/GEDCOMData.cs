namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMData : GEDCOMTagWithLists
	{
		private GEDCOMList<GEDCOMEvent> fEvents;

		public GEDCOMList<GEDCOMEvent> Events
		{
			get { return this.fEvents; }
		}

		public string Agency
		{
			get { return base.GetTagStringValue("AGNC"); }
			set { base.SetTagStringValue("AGNC", value); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "DATA";

			this.fEvents = new GEDCOMList<GEDCOMEvent>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fEvents.Dispose();
			}
			base.Dispose(disposing);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "EVEN")
			{
				result = this.fEvents.Add(new GEDCOMEvent(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
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

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fEvents.ResetOwner(newOwner);
		}

		public GEDCOMData(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMData(owner, parent, tagName, tagValue);
		}
	}
}
