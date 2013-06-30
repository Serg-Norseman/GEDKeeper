using System;
using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMData : TGEDCOMTagWithLists
	{
		private GEDCOMList<TGEDCOMEvent> _Events;

		public GEDCOMList<TGEDCOMEvent> Events
		{
			get { return this._Events; }
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

			this._Events = new GEDCOMList<TGEDCOMEvent>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Events.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "EVEN")
			{
				Result = this._Events.Add(new TGEDCOMEvent(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this._Events.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && (this._Events.Count == 0);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this._Events.ResetOwner(AOwner);
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
