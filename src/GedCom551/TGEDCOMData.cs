using System;
using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMData : TGEDCOMTagWithLists
	{
		private GEDCOMList<TGEDCOMEvent> fEvents;

		public GEDCOMList<TGEDCOMEvent> Events
		{
			get { return this.fEvents; }
		}

		public string Agency
		{
			get { return base.GetTagStringValue("AGNC"); }
			set { base.SetTagStringValue("AGNC", value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "DATA";

			this.fEvents = new GEDCOMList<TGEDCOMEvent>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.fEvents.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "EVEN")
			{
				result = this.fEvents.Add(new TGEDCOMEvent(base.Owner, this, tagName, tagValue));
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

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this.fEvents.ResetOwner(AOwner);
		}

		public TGEDCOMData(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMData(owner, parent, tagName, tagValue);
		}
	}
}
