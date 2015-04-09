namespace GKCommon.GEDCOM
{
	public class GEDCOMPointer : GEDCOMTag
	{
		protected string fXRef = "";

		public GEDCOMRecord Value
		{
			get	{
				return base.FindRecord(this.XRef);
			}
			set	{
				this.fXRef = "";
				if (value != null)
				{
					string xrf = value.XRef;
					if (string.IsNullOrEmpty(xrf))
					{
						xrf = value.NewXRef();
					}
					this.XRef = xrf;
				}
			}
		}

		public string XRef
		{
			get { return GEDCOMUtils.CleanXRef(this.fXRef); }
			set { this.fXRef = GEDCOMUtils.EncloseXRef(value); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fXRef = "";
		}

		protected override string GetStringValue()
		{
			return this.fXRef;
		}

		public override bool IsEmpty()
		{
			return (string.IsNullOrEmpty(this.fXRef));
		}

		public override string ParseString(string strValue)
		{
			this.fXRef = "";
			string result = strValue;
			result = GEDCOMUtils.ExtractDelimiter(result, 0);

			if (!string.IsNullOrEmpty(result) && result[0] == '@' && result[1] != '#')
			{
				int pos = result.IndexOf('@', 2);
				if (pos > 0)
				{
					pos++;
					this.fXRef = result.Substring(0, pos);
					result = result.Remove(0, pos);
				}
			}
			return result;
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.XRef = map.FindNewXRef(this.XRef);
		}

		public void SetNamedValue(string aName, GEDCOMRecord aValue)
		{
			base.Name = aName;
			this.Value = aValue;
		}

		public GEDCOMPointer(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMPointer(owner, parent, tagName, tagValue);
		}
	}
}
